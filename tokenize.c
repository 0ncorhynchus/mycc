#include "mycc.h"
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

static const char *filename;
static const char *user_input;
static int line_number = 1;
static const char *line_head;

const char *reserved[] = {
    "auto",       "break",     "case",           "char",
    "const",      "continue",  "default",        "do",
    "double",     "else",      "enum",           "extern",
    "float",      "for",       "goto",           "if",
    "inline",     "int",       "long",           "register",
    "restrict",   "return",    "short",          "signed",
    "sizeof",     "static",    "struct",         "switch",
    "typedef",    "union",     "unsigned",       "void",
    "volatile",   "while",     "_Alignas",       "_Alignof",
    "_Atomic",    "_Bool",     "_Complex",       "_Generic",
    "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local"};
const size_t num_reserved = sizeof(reserved) / sizeof(const char *);

const char *punctuators[] = {
    "~",   "}",  "||", "|=",   "|",  "{",  "^=",  "^",   "]",  "[",  "?",
    ">>=", ">>", ">=", ">",    "==", "=",  "<=",  "<<=", "<<", "<:", "<%",
    "<",   ";",  ":>", ":",    "/=", "/",  "...", ".",   "->", "-=", "--",
    "-",   ",",  "+=", "++",   "+",  "*=", "*",   ")",   "(",  "&=", "&&",
    "&",   "%>", "%=", "%:%:", "%:", "%",  "##",  "#",   "!=", "!"};
const size_t num_punctuators = sizeof(punctuators) / sizeof(const char *);

void
error_at(const Span *span, char *fmt, ...) {
    const char *begin = span->ptr - span->offset;
    const char *end = span->ptr;
    while (*end != '\n') {
        end++;
    }

    int indent = fprintf(stderr, "%s:%d: ", span->file, span->line);
    fprintf(stderr, "%.*s\n", (int)(end - begin), begin);

    int pos = span->offset + indent;
    fprintf(stderr, "%*s", pos, "");
    for (int i = 0; i < span->len; i++)
        fprintf(stderr, "^");
    fprintf(stderr, " ");

    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");

    exit(1);
}

static char *
read_file(const char *path) {
    char *buf;
    if (strcmp(path, "-") == 0) {
        int offset = 0;
        int size = 128;
        buf = calloc(1, size);
        char *line = calloc(1, 128);

        while (fgets(line, sizeof(line), stdin) != NULL) {
            int len = strlen(line);
            if (size < offset + len) {
                while ((size *= 2) < (offset + len)) {
                }
                char *tmp;
                if ((tmp = realloc(buf, size)) == NULL) {
                    error("failed to realloc\n");
                    break;
                } else {
                    buf = tmp;
                }
            }
            memcpy(buf + offset, line, len);
            offset += len;
        }
    } else {
        FILE *fp = fopen(path, "r");

        if (!fp) {
            error("cannot open %s: %s", path, strerror(errno));
        }

        if (fseek(fp, 0, SEEK_END) == -1) {
            error("%s: fseek: %s", path, strerror(errno));
        }

        size_t size = ftell(fp);
        if (fseek(fp, 0, SEEK_SET) == -1) {
            error("%s: fseek: %s", path, strerror(errno));
        }

        buf = calloc(1, size + 2);
        fread(buf, size, 1, fp);

        if (size == 0 || buf[size - 1] != '\n') {
            buf[size++] = '\n';
        }

        buf[size] = '\0';
        fclose(fp);
    }

    return buf;
}

static int
is_alnum(char c) {
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') ||
           ('0' <= c && c <= '9') || (c == '_');
}

static Span
make_span(const char *ptr, int len) {
    Span span = {};
    span.ptr = ptr;
    span.len = len;
    span.file = filename;
    span.line = line_number;
    span.offset = ptr - line_head;
    return span;
}

static Token *
gen_token(TokenKind kind, const char *str, int len) {
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->span = make_span(str, len);
    return tok;
}

static Token *
new_token(TokenKind kind, Token *cur, const char *str, int len) {
    Token *tok = gen_token(kind, str, len);
    cur->next = tok;
    return tok;
}

static void
skip_space(const char **p) {
    while (**p != '\n' && isspace(**p)) {
        (*p)++;
    }
}

static void
skip_to_break(const char **p) {
    while (**p != '\n') {
        (*p)++;
    }
    (*p)++;
    line_head = *p;
}

static int
read_decimal(const char **rest, const char *p, int base) {
    char *endptr;
    int val = strtol(p, &endptr, base);
    *rest = endptr;
    return val;
}

static void
process_macro(const char **p) {
    int line = 0;
    skip_space(p);
    if (isdigit(**p)) {
        line = read_decimal(p, *p, 10);
    } else {
        skip_to_break(p);
        return;
    }
    skip_space(p);
    if (**p == '"') {
        (*p)++;
        const char *end = *p;
        while (*end != '\n' && *end != '"') {
            end++;
        }
        if (*end == '\n') {
            return;
        }
        int len = end - *p;
        char *f = calloc(len + 1, sizeof(char));
        memcpy(f, *p, len);
        filename = f;
        line_number = line;
    }
    skip_to_break(p);
}

typedef enum {
    IS_UNSIGNED = 1,
    IS_LONG = 2,
} IntegerSuffix;

static bool
unsigned_suffix(const char **rest, const char *p) {
    if (*p == 'u' || *p == 'U') {
        *rest = p + 1;
        return true;
    } else {
        return false;
    }
}

static int
long_suffix(const char **rest, const char *p) {
    int kind = 0;
    switch (*p) {
    case 'l':
        p++;
        kind += IS_LONG;
        if (*p == 'l') {
            p++;
            kind += IS_LONG;
        }
        break;
    case 'L':
        p++;
        kind += IS_LONG;
        if (*p == 'L') {
            p++;
            kind += IS_LONG;
        }
        break;
    default:
        break;
    }

    *rest = p;
    return kind;
}

//
//  integer_suffix =
//          unsigned_suffix
//          unsigned_suffix long_suffix
//          unsigned_suffix longlong_suffix
//          long_suffix
//          long_suffix unsigned_suffix
//          longlong_suffix
//          longlong_suffix unsigned_suffix
//
static int
integer_suffix(const char **rest, const char *p) {
    int kind = 0;
    if (unsigned_suffix(&p, p)) {
        kind += IS_UNSIGNED;
        kind += long_suffix(&p, p);
    } else {
        kind += long_suffix(&p, p);
        if (unsigned_suffix(&p, p)) {
            kind += IS_UNSIGNED;
        }
    }

    *rest = p;
    return kind;
}

//
//  integer_constant =
//          decimal_constant integer_suffix?
//          octal_constant integer_suffix?
//          hexadecimal_constant integer_suffix?
//
static Token *
integer(const char **rest, const char *p) {
    const char *first = p;
    int base;
    if (*p == '0') {
        p++;
        if (*p == 'x' || *p == 'X') {
            p++;
            base = 16;
        } else {
            base = 8;
        }
    } else if (isdigit(*p)) {
        base = 10;
    } else {
        return NULL;
    }

    int val = read_decimal(&p, p, base);
    integer_suffix(&p, p);

    int len = p - first;
    *rest = p;
    Token *tok = gen_token(TK_CONST, first, len);
    tok->val = val;
    return tok;
}

const Token *
tokenize(const char *path) {
    filename = path;
    user_input = read_file(filename);
    line_head = user_input;
    const char *p = user_input;

    Token head;
    head.next = NULL;
    Token *cur = &head;

    while (*p) {
        if (*p == '\n') {
            p++;
            line_number++;
            line_head = p;
            continue;
        }

        if (isspace(*p)) {
            p++;
            continue;
        }

        // Macro
        if (strncmp(p, "#", 1) == 0) {
            p++;
            process_macro(&p);
            continue;
        }

        // Line Comment
        if (strncmp(p, "//", 2) == 0) {
            p += 2;
            while (*p != '\n') {
                p++;
            }
            continue;
        }

        // Block Comment
        if (strncmp(p, "/*", 2) == 0) {
            char *q = strstr(p + 2, "*/");
            if (!q) {
                const Span span = make_span(p, 1);
                error_at(&span, "Unclosed comment");
            }
            p = q + 2;
            continue;
        }

        Token *num = integer(&p, p);
        if (num) {
            cur->next = num;
            cur = num;
            continue;
        }

        if (*p == '"') {
            const char *first = p;

            p++; // consume begining '"'
            while (*p != '"') {
                p++;
            }
            p++; // consume ending '"'

            int len = p - first;
            cur = new_token(TK_STRING, cur, first, len);
            continue;
        }

        bool is_punctuate = false;
        for (int i = 0; i < num_punctuators; i++) {
            int len = strlen(punctuators[i]);
            if (strncmp(p, punctuators[i], len) == 0) {
                cur = new_token(TK_RESERVED, cur, p, len);
                p += len;
                is_punctuate = true;
                break;
            }
        }
        if (is_punctuate) {
            continue;
        }

        if (('a' <= *p && *p <= 'z') || ('A' <= *p && *p <= 'Z') || *p == '_') {
            const char *first = p;

            p++;
            while (is_alnum(*p)) {
                p++;
            }

            int len = p - first;
            TokenKind kind = TK_IDENT;

            for (int i = 0; i < num_reserved; i++) {
                if (len == strlen(reserved[i]) &&
                    strncmp(first, reserved[i], len) == 0) {
                    kind = TK_RESERVED;
                    break;
                }
            }

            cur = new_token(kind, cur, first, len);
            continue;
        }

        const Span span = make_span(p, 1);
        error_at(&span, "Failed to tokenize");
    }

    new_token(TK_EOF, cur, p, 0);

    return head.next;
}
