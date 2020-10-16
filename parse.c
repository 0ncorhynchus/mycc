#include "mycc.h"
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const char *filename;
static const char *user_input;
static const Token *token;

const char *reserved[] = {"if",  "else",   "while", "for",  "return",
                          "int", "sizeof", "char",  "void", NULL};

char *read_file(const char *path) {
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

    char *buf = calloc(1, size + 2);
    fread(buf, size, 1, fp);

    if (size == 0 || buf[size - 1] != '\n') {
        buf[size++] = '\n';
    }

    buf[size] = '\0';
    fclose(fp);
    return buf;
}

void error_at(const Span *span, char *fmt, ...) {
    const char *line = span->ptr;
    while (user_input < line && line[-1] != '\n') {
        line--;
    }

    const char *end = span->ptr;
    while (*end != '\n') {
        end++;
    }

    int line_num = 1;
    for (const char *p = user_input; p < line; p++) {
        if (*p == '\n') {
            line_num++;
        }
    }

    int indent = fprintf(stderr, "%s:%d: ", filename, line_num);
    fprintf(stderr, "%.*s\n", (int)(end - line), line);

    int pos = span->ptr - line + indent;
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

int is_alnum(char c) {
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') ||
           ('0' <= c && c <= '9') || (c == '_');
}

static void unexpected(const char *expected, const Token *got) {
    error_at(&got->span, "Unexpected token: '%s' expected, but got '%.*s'",
             expected, got->span.len, got->span.ptr);
}

static bool consume(const Token **rest, const Token *tok, char *op) {
    if (tok->kind != TK_RESERVED || strlen(op) != tok->span.len ||
        memcmp(tok->span.ptr, op, tok->span.len)) {
        return false;
    }
    *rest = tok->next;
    return true;
}

static void expect(const Token **rest, const Token *tok, char *op) {
    if (!consume(rest, tok, op)) {
        unexpected(op, tok);
    }
}

static const Token *consume_ident(const Token **rest, const Token *tok) {
    if (tok->kind != TK_IDENT) {
        return NULL;
    }
    *rest = tok->next;
    return tok;
}

static const Token *consume_string(const Token **rest, const Token *tok) {
    if (tok->kind != TK_STRING) {
        return NULL;
    }
    *rest = tok->next;
    return tok;
}

static bool number(const Token **rest, const Token *tok, int *val) {
    if (tok->kind != TK_NUM) {
        return false;
    }

    *val = tok->val;
    *rest = tok->next;

    return true;
}

static bool at_eof(const Token *tok) { return tok->kind == TK_EOF; }

static Token *new_token(TokenKind kind, Token *cur, const char *str, int len) {
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->span.ptr = str;
    tok->span.len = len;
    cur->next = tok;
    return tok;
}

void tokenize(const char *path) {
    filename = path;
    user_input = read_file(filename);
    const char *p = user_input;

    Token head;
    head.next = NULL;
    Token *cur = &head;

    while (*p) {
        if (isspace(*p)) {
            p++;
            continue;
        }

        if (strncmp(p, "//", 2) == 0) {
            p += 2;
            while (*p != '\n') {
                p++;
            }
            continue;
        }

        if (strncmp(p, "/*", 2) == 0) {
            char *q = strstr(p + 2, "*/");
            if (!q) {
                const Span span = {p, 1};
                error_at(&span, "Unclosed comment");
            }
            p = q + 2;
            continue;
        }

        if (*p == '=') {
            int len;
            if (*(p + 1) && *(p + 1) == '=')
                len = 2;
            else
                len = 1;
            cur = new_token(TK_RESERVED, cur, p, len);
            p += len;
            continue;
        }

        if (*p == '!') {
            if (!*(p + 1) || *(p + 1) != '=') {
                Span span = {p, 1};
                error_at(&span, "Failed to tokenize");
            }

            cur = new_token(TK_RESERVED, cur, p, 2);
            p += 2;
            continue;
        }

        if (*p == '<' || *p == '>') {
            int len;

            if (*(p + 1) && *(p + 1) == '=') {
                len = 2;
            } else {
                len = 1;
            }

            cur = new_token(TK_RESERVED, cur, p, len);
            p += len;
            continue;
        }

        if (*p == '+' || *p == '-' || *p == '*' || *p == '/' || *p == '(' ||
            *p == ')' || *p == ';' || *p == '{' || *p == '}' || *p == ',' ||
            *p == '&' || *p == '[' || *p == ']') {
            cur = new_token(TK_RESERVED, cur, p++, 1);
            continue;
        }

        if (isdigit(*p)) {
            const char *first = p;
            char *endptr;
            int val = strtol(p, &endptr, 10);
            p = endptr;
            int len = p - first;
            cur = new_token(TK_NUM, cur, first, len);
            cur->val = val;
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

        if (('a' <= *p && *p <= 'z') || ('A' <= *p && *p <= 'Z') || *p == '_') {
            const char *first = p;

            p++;
            while (is_alnum(*p)) {
                p++;
            }

            int len = p - first;
            TokenKind kind = TK_IDENT;

            for (int i = 0; reserved[i]; i++) {
                if (len == strlen(reserved[i]) &&
                    strncmp(first, reserved[i], len) == 0) {
                    kind = TK_RESERVED;
                    break;
                }
            }

            cur = new_token(kind, cur, first, len);
            continue;
        }

        Span span = {p, 1};
        error_at(&span, "Failed to tokenize");
    }

    new_token(TK_EOF, cur, p, 0);
    token = head.next;
}

const Type *type_specifier(const Token **rest, const Token *tok) {
    if (consume(rest, tok, "int")) {
        return &INT_T;
    }
    if (consume(rest, tok, "char")) {
        return &CHAR_T;
    }
    if (consume(rest, tok, "void")) {
        return &VOID_T;
    }
    return NULL;
}

//
//  type = type "*" | "int" | "char" | "void"
//
const Type *type(const Token **rest, const Token *tok) {
    const Type *ty = type_specifier(&tok, tok);
    if (ty == NULL) {
        return NULL;
    }

    while (consume(&tok, tok, "*")) {
        ty = mk_ptr(ty);
    }

    *rest = tok;
    return ty;
}

//
//  typename = ( "int" | "char" | "void" ) ( "*"* | "[" num "]" )
//
const Type *typename(const Token **rest, const Token *tok) {
    const Type *ty = type_specifier(&tok, tok);
    if (ty == NULL) {
        return NULL;
    }

    if (consume(&tok, tok, "*")) {
        ty = mk_ptr(ty);
        while (consume(&tok, tok, "*")) {
            ty = mk_ptr(ty);
        }
    } else if (consume(&tok, tok, "[")) {
        int size;
        if (!number(&tok, tok, &size)) {
            error("Expect a number.");
        }
        expect(&tok, tok, "]");

        Type *array_ty = calloc(1, sizeof(Type));
        array_ty->ty = ARRAY;
        array_ty->ptr_to = ty;
        array_ty->array_size = size;
        ty = array_ty;
    }

    *rest = tok;
    return ty;
}

// Implicit converter from array T[] to pointer T*
Node *as_ptr(Node *array) {
    if (array == NULL || array->ty == NULL || array->ty->ty != ARRAY)
        return array;

    Node *ptr = calloc(1, sizeof(Node));
    ptr->kind = ND_ADDR;
    ptr->lhs = array;
    ptr->ty = mk_ptr(array->ty->ptr_to);

    return ptr;
}

Node *new_node(NodeKind kind, Node *lhs, Node *rhs) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = kind;
    node->ty = get_type(lhs, rhs);
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

Node *new_node_num(int val) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_NUM;
    node->ty = &INT_T;
    node->val = val;
    return node;
}

static Node *expr(const Token **rest, const Token *tok, Env *env);

//
//  primary = num
//          | ident ( "(" ( expr ( "," expr )* )? ")" )?
//          | "(" expr ")"
//          | string
//
static Node *primary(const Token **rest, const Token *tok, Env *env) {
    if (consume(&tok, tok, "(")) {
        Node *node = expr(&tok, tok, env);
        expect(&tok, tok, ")");
        *rest = tok;
        return node;
    }

    const Token *tmp = consume_ident(&tok, tok);
    if (tmp) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_LVAR;
        node->ident = tmp->span;

        // function call
        if (consume(&tok, tok, "(")) {
            node->kind = ND_CALL;
            if (consume(&tok, tok, ")")) {
                *rest = tok;
                return node;
            }

            node->lhs = calloc(1, sizeof(Node));
            Node *current = node->lhs;
            current->kind = ND_ARGS;
            current->lhs = expr(&tok, tok, env);
            while (!consume(&tok, tok, ")")) {
                expect(&tok, tok, ",");
                current->rhs = calloc(1, sizeof(Node));
                current = current->rhs;
                current->kind = ND_ARGS;
                current->lhs = expr(&tok, tok, env);
            }

            *rest = tok;
            return node;
        }

        const Var *var = get_var(env, &tmp->span);
        node->ty = var->ty;
        node->offset = var->offset;
        node->vkind = var->kind;

        *rest = tok;
        return node;
    }

    tmp = consume_string(&tok, tok);
    if (tmp) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_STRING;
        node->ident = tmp->span;
        node->ident.ptr++;
        node->ident.len -= 2;
        node->ty = mk_ptr(&CHAR_T);

        const String *string = push_string(env, &node->ident);
        node->val = string->index;

        *rest = tok;
        return node;
    }

    int val;
    if (number(&tok, tok, &val)) {
        *rest = tok;
        return new_node_num(val);
    }

    return NULL;
}

Node *deref_offset_ptr(Node *ptr, Node *index) {
    Node *new = calloc(1, sizeof(Node));
    new->kind = ND_DEREF;
    new->lhs = new_node(ND_ADD, ptr, index);
    if (new->lhs->ty == NULL || new->lhs->ty->ty != PTR) {
        error("Cannot deref: '%s'", type_to_str(new->lhs->ty));
    }
    new->ty = new->lhs->ty->ptr_to;
    return new;
}

// parse primary ("[" expr "]")?
static Node *desugar_index(const Token **rest, const Token *tok, Env *env) {
    Node *node = primary(&tok, tok, env);
    if (consume(&tok, tok, "[")) {
        Node *index = expr(&tok, tok, env);
        expect(&tok, tok, "]");
        *rest = tok;
        return deref_offset_ptr(as_ptr(node), index);
    }
    *rest = tok;
    return node;
}

//
//  unary = ( "+" | "-" )? primary ( "[" expr "]" )?
//        | ( "*" | "&" | "sizeof" ) unary
//        | "sizeof" "(" type ")"
//
static Node *unary(const Token **rest, const Token *tok, Env *env) {
    if (consume(&tok, tok, "+")) {
        return desugar_index(rest, tok, env);
    }
    if (consume(&tok, tok, "-")) {
        return new_node(ND_SUB, new_node_num(0), desugar_index(rest, tok, env));
    }
    if (consume(&tok, tok, "*")) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_DEREF;
        Node *inner = as_ptr(unary(&tok, tok, env));
        if (inner->ty && inner->ty->ty == PTR) {
            node->lhs = inner;
            node->ty = inner->ty->ptr_to;
            *rest = tok;
            return node;
        }
        error("Cannot deref: '%s'", type_to_str(inner->ty));
    }
    if (consume(&tok, tok, "&")) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_ADDR;
        node->lhs = unary(&tok, tok, env);
        if (node->lhs && node->lhs->ty) {
            node->ty = mk_ptr(node->lhs->ty);
            *rest = tok;
            return node;
        }
        error("Internal compile error: try to obtain the address to an unknown "
              "type");
    }
    if (consume(&tok, tok, "sizeof")) {
        if (consume(&tok, tok, "(")) {
            const Type *ty = typename(&tok, tok);
            if (ty == NULL) {
                Node *node = expr(&tok, tok, env);
                if (node) {
                    ty = node->ty;
                }
            }
            expect(&tok, tok, ")");

            if (ty) {
                *rest = tok;
                return new_node_num(sizeof_ty(ty));
            }
        } else {
            Node *node = unary(&tok, tok, env);
            if (node->ty) {
                *rest = tok;
                return new_node_num(sizeof_ty(node->ty));
            }
        }
        error("Internal compile error: try to obtain the size of an unknown "
              "type");
    }

    return desugar_index(rest, tok, env);
}

//
//  mul = unary ( "*" unary | "/" unary )*
//
static Node *mul(const Token **rest, const Token *tok, Env *env) {
    Node *node = unary(&tok, tok, env);

    for (;;) {
        if (consume(&tok, tok, "*")) {
            node = new_node(ND_MUL, node, unary(&tok, tok, env));
        } else if (consume(&tok, tok, "/")) {
            node = new_node(ND_DIV, node, unary(&tok, tok, env));
        } else {
            *rest = tok;
            return node;
        }
    }
}

//
//  add = mul ( "+" mul | "-" mul )*
//
static Node *add(const Token **rest, const Token *tok, Env *env) {
    Node *node = mul(&tok, tok, env);

    for (;;) {
        if (consume(&tok, tok, "+")) {
            node = new_node(ND_ADD, as_ptr(node), as_ptr(mul(&tok, tok, env)));
        } else if (consume(&tok, tok, "-")) {
            node = new_node(ND_SUB, as_ptr(node), as_ptr(mul(&tok, tok, env)));
        } else {
            *rest = tok;
            return node;
        }
    }
}

//
//  relational= add ( "<" add | "<=" add | ">" add | ">=" add )*
//
static Node *relational(const Token **rest, const Token *tok, Env *env) {
    Node *node = add(&tok, tok, env);

    for (;;) {
        if (consume(&tok, tok, "<")) {
            Node *rhs = add(&tok, tok, env);
            if (!is_same_type(node->ty, rhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LT, node, rhs);
        } else if (consume(&tok, tok, "<=")) {
            Node *rhs = add(&tok, tok, env);
            if (!is_same_type(node->ty, rhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LE, node, rhs);
        } else if (consume(&tok, tok, ">")) {
            Node *lhs = add(&tok, tok, env);
            if (!is_same_type(node->ty, lhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LT, lhs, node);
        } else if (consume(&tok, tok, ">=")) {
            Node *lhs = add(&tok, tok, env);
            if (!is_same_type(node->ty, lhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LE, lhs, node);
        } else {
            *rest = tok;
            return node;
        }

        node->ty = &INT_T;
    }
}

//
//  equality = relational ( "==" relational | "!=" relational )*
//
static Node *equality(const Token **rest, const Token *tok, Env *env) {
    Node *node = relational(&tok, tok, env);

    for (;;) {
        if (consume(&tok, tok, "==")) {
            node = new_node(ND_EQ, node, relational(&tok, tok, env));
        } else if (consume(&tok, tok, "!=")) {
            node = new_node(ND_NE, node, relational(&tok, tok, env));
        } else {
            *rest = tok;
            return node;
        }
    }
}

//
//  assign = equality ( "=" assign )?
//
static Node *assign(const Token **rest, const Token *tok, Env *env) {
    Node *node = equality(&tok, tok, env);
    if (consume(&tok, tok, "=")) {
        const Type *ty = node->ty;
        node = new_node(ND_ASSIGN, node, as_ptr(assign(&tok, tok, env)));
        node->ty = ty;
    }
    *rest = tok;
    return node;
}

//
//  expr = assign
//
static Node *expr(const Token **rest, const Token *tok, Env *env) {
    return assign(rest, tok, env);
}

static Node *stmt(const Token **rest, const Token *tok, Env *env);

// try to parse a type and an ident.
// For parse a function and a declare.
static Node *type_ident(const Token **rest, const Token *tok) {
    const Type *ty = type(&tok, tok);
    if (ty == NULL) {
        return NULL;
    }

    Node *node = calloc(1, sizeof(Node));
    node->ty = ty;
    const Token *ident = consume_ident(&tok, tok);

    if (ident == NULL) {
        unexpected("an ident", tok);
    }

    node->ident = ident->span;

    *rest = tok;
    return node;
}

//
//  function = type ident
//             "(" (type ident ("," type ident)*)? ")"
//             "{" stmt* "}"
//
static Node *function(const Token **rest, const Token *tok, Env *parent,
                      Node *node) {
    if (!consume(&tok, tok, "("))
        return NULL;

    Env env = make_scope(parent);
    int argument_offset = 0;

    node->kind = ND_FUNC;

    if (!consume(&tok, tok, ")")) {
        Node *arg = type_ident(&tok, tok);
        arg->kind = ND_FUNC_ARGS;
        node->lhs = arg;

        declare_var(&env, arg->ty, &arg->ident);

        while (consume(&tok, tok, ",")) {
            arg = type_ident(&tok, tok);
            arg->kind = ND_FUNC_ARGS;
            arg->lhs = node->lhs;
            node->lhs = arg;

            declare_var(&env, arg->ty, &arg->ident);
        }
        expect(&tok, tok, ")");

        argument_offset = env.maximum_offset;
        node->lhs->val = argument_offset / 8;
        if (node->lhs->val > 6)
            error("Not supported: more than 6 arguments.");
    }

    expect(&tok, tok, "{");
    Node *body = node;
    while (!consume(&tok, tok, "}")) {
        body->rhs = calloc(1, sizeof(Node));
        body->rhs->kind = ND_FUNC_BODY;
        body->rhs->lhs = stmt(&tok, tok, &env);
        body = body->rhs;
    }
    if (node->rhs) {
        int variables_offset = env.maximum_offset - argument_offset;
        node->rhs->val = variables_offset;
    }

    *rest = tok;

    return node;
}

//
//  init = expr | "{" init ( "," init )* "}"
//
static Node *init(const Token **rest, const Token *tok, Env *env) {
    if (consume(&tok, tok, "{")) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_INIT;
        node->next = init(&tok, tok, env);
        node->num_initializers = 1;
        Node *n = node->next;
        while (consume(&tok, tok, ",")) {
            n->next = init(&tok, tok, env);
            n = n->next;
            node->num_initializers++;
        }
        expect(&tok, tok, "}");
        *rest = tok;
        return node;
    }

    *rest = tok;
    return expr(rest, tok, env);
}

//
//  declare = type ident ( "[" num "]" )? ( "=" init )? ";"
//
static Node *declare(const Token **rest, const Token *tok, Env *env,
                     Node *node) {
    node->kind = ND_DECLARE;
    bool is_array = false;
    bool is_known_size = false;

    Type *array_ty = NULL;

    if (consume(&tok, tok, "[")) {
        is_array = true;
        int array_size = -1;
        is_known_size = number(&tok, tok, &array_size);
        expect(&tok, tok, "]");

        array_ty = calloc(1, sizeof(Type));
        array_ty->ty = ARRAY;
        array_ty->ptr_to = node->ty;
        array_ty->array_size = array_size;
    }

    if (consume(&tok, tok, "=")) {
        node->init = as_ptr(init(&tok, tok, env));
        if (is_array && !is_known_size) {
            switch (node->init->kind) {
            case (ND_INIT):
                array_ty->array_size = node->init->num_initializers;
                break;
            case (ND_STRING):
                array_ty->array_size = node->init->ident.len + 1;
                break;
            default:
                error("array must be initialized with a brace-enclosed "
                      "initializer");
            }
        }
    }

    if (array_ty) {
        node->ty = array_ty;
    }

    const Var *var = declare_var(env, node->ty, &node->ident);
    node->vkind = var->kind;
    node->offset = var->offset;

    expect(&tok, tok, ";");
    *rest = tok;

    return node;
}

//
//  stmt =  expr ";"
//       | declare
//       | "{" stmt* "}"
//       | "if" "(" expr ")" stmt ( "else" stmt )?
//       | "while" "(" expr ")" stmt
//       | "for" "(" expr? ";" expr? ";" expr? ")" stmt
//       | "return" expr? ";"
//
static Node *stmt(const Token **rest, const Token *tok, Env *env) {
    Node *node = NULL;

    if (consume(&tok, tok, "if")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_IF_COND;
        expect(&tok, tok, "(");
        node->lhs = expr(&tok, tok, env);
        expect(&tok, tok, ")");

        node->rhs = calloc(1, sizeof(Node));
        node->rhs->kind = ND_IF_BODY;
        node->rhs->lhs = stmt(&tok, tok, env);
        if (consume(&tok, tok, "else")) {
            node->rhs->rhs = stmt(&tok, tok, env);
        }
    } else if (consume(&tok, tok, "while")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_WHILE;
        expect(&tok, tok, "(");
        node->lhs = expr(&tok, tok, env);
        expect(&tok, tok, ")");
        node->rhs = stmt(&tok, tok, env);
    } else if (consume(&tok, tok, "for")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_FOR_INIT;
        expect(&tok, tok, "(");
        if (!consume(&tok, tok, ";")) {
            node->lhs = expr(&tok, tok, env);
            expect(&tok, tok, ";");
        }

        node->rhs = calloc(1, sizeof(Node));
        node->rhs->kind = ND_FOR_COND;
        if (!consume(&tok, tok, ";")) {
            node->rhs->lhs = expr(&tok, tok, env);
            expect(&tok, tok, ";");
        }

        node->rhs->rhs = calloc(1, sizeof(Node));
        node->rhs->rhs->kind = ND_FOR_BODY;
        if (!consume(&tok, tok, ")")) {
            node->rhs->rhs->lhs = expr(&tok, tok, env);
            expect(&tok, tok, ")");
        }
        node->rhs->rhs->rhs = stmt(&tok, tok, env);
    } else if (consume(&tok, tok, "return")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_RETURN;
        node->lhs = as_ptr(expr(&tok, tok, env));
        expect(&tok, tok, ";");
    } else if (consume(&tok, tok, "{")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_BLOCK;
        Node *current = node;
        while (!consume(&tok, tok, "}")) {
            current->lhs = stmt(&tok, tok, env);
            current->rhs = calloc(1, sizeof(Node));
            current = current->rhs;
            current->kind = ND_BLOCK;
        }
    } else {
        node = type_ident(&tok, tok);
        if (node) {
            node = declare(&tok, tok, env, node);
        } else {
            node = calloc(1, sizeof(Node));
            node->kind = ND_SEMICOLON;
            node->lhs = expr(&tok, tok, env);
            expect(&tok, tok, ";");
        }
    }

    *rest = tok;

    return node;
}

//
//  program = ( function | declare )*
//
void program(Env *env, Node *code[]) {
    int i = 0;
    while (!at_eof(token)) {
        Node *node = type_ident(&token, token);
        if (node == NULL)
            error("Cannot parse the program.");

        Node *fn = function(&token, token, env, node);
        if (fn)
            code[i++] = fn;
        else
            code[i++] = declare(&token, token, env, node);
    }
    code[i] = NULL;
}
