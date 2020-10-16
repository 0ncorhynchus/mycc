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

bool number(int *val) {
    if (token->kind != TK_NUM) {
        return false;
    }

    *val = token->val;
    token = token->next;

    return true;
}

bool at_eof() { return token->kind == TK_EOF; }

Token *new_token(TokenKind kind, Token *cur, const char *str, int len) {
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

const Type *type_specifier() {
    if (consume(&token, token, "int")) {
        return &INT_T;
    }
    if (consume(&token, token, "char")) {
        return &CHAR_T;
    }
    if (consume(&token, token, "void")) {
        return &VOID_T;
    }
    return NULL;
}

//
//  type = type "*" | "int" | "char" | "void"
//
const Type *type() {
    const Type *ty = type_specifier();
    if (ty == NULL) {
        return NULL;
    }

    while (consume(&token, token, "*")) {
        ty = mk_ptr(ty);
    }

    return ty;
}

//
//  typename = ( "int" | "char" | "void" ) ( "*"* | "[" num "]" )
//
const Type *typename() {
    const Type *ty = type_specifier();
    if (ty == NULL) {
        return NULL;
    }

    if (consume(&token, token, "*")) {
        ty = mk_ptr(ty);
        while (consume(&token, token, "*")) {
            ty = mk_ptr(ty);
        }
    } else if (consume(&token, token, "[")) {
        int size;
        if (!number(&size)) {
            error("Expect a number.");
        }
        expect(&token, token, "]");

        Type *array_ty = calloc(1, sizeof(Type));
        array_ty->ty = ARRAY;
        array_ty->ptr_to = ty;
        array_ty->array_size = size;
        ty = array_ty;
    }

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

//
//  primary = num
//          | ident ( "(" ( expr ( "," expr )* )? ")" )?
//          | "(" expr ")"
//          | string
//
Node *primary(Env *env) {
    if (consume(&token, token, "(")) {
        Node *node = expr(env);
        expect(&token, token, ")");
        return node;
    }

    const Token *tok = consume_ident(&token, token);
    if (tok) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_LVAR;
        node->ident = tok->span;

        // function call
        if (consume(&token, token, "(")) {
            node->kind = ND_CALL;
            if (consume(&token, token, ")"))
                return node;

            node->lhs = calloc(1, sizeof(Node));
            Node *current = node->lhs;
            current->kind = ND_ARGS;
            current->lhs = expr(env);
            while (!consume(&token, token, ")")) {
                expect(&token, token, ",");
                current->rhs = calloc(1, sizeof(Node));
                current = current->rhs;
                current->kind = ND_ARGS;
                current->lhs = expr(env);
            }
            return node;
        }

        const Var *var = get_var(env, &tok->span);
        node->ty = var->ty;
        node->offset = var->offset;
        node->vkind = var->kind;

        return node;
    }

    tok = consume_string(&token, token);
    if (tok) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_STRING;
        node->ident = tok->span;
        node->ident.ptr++;
        node->ident.len -= 2;
        node->ty = mk_ptr(&CHAR_T);

        const String *string = push_string(env, &node->ident);
        node->val = string->index;

        return node;
    }

    int val;
    if (number(&val)) {
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
Node *desugar_index(Env *env) {
    Node *node = primary(env);
    if (consume(&token, token, "[")) {
        Node *index = expr(env);
        expect(&token, token, "]");
        return deref_offset_ptr(as_ptr(node), index);
    }
    return node;
}

//
//  unary = ( "+" | "-" )? primary ( "[" expr "]" )?
//        | ( "*" | "&" | "sizeof" ) unary
//        | "sizeof" "(" type ")"
//
Node *unary(Env *env) {
    if (consume(&token, token, "+")) {
        return desugar_index(env);
    }
    if (consume(&token, token, "-")) {
        return new_node(ND_SUB, new_node_num(0), desugar_index(env));
    }
    if (consume(&token, token, "*")) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_DEREF;
        Node *inner = as_ptr(unary(env));
        if (inner->ty && inner->ty->ty == PTR) {
            node->lhs = inner;
            node->ty = inner->ty->ptr_to;
            return node;
        }
        error("Cannot deref: '%s'", type_to_str(inner->ty));
    }
    if (consume(&token, token, "&")) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_ADDR;
        node->lhs = unary(env);
        if (node->lhs && node->lhs->ty) {
            node->ty = mk_ptr(node->lhs->ty);
            return node;
        }
        error("Internal compile error: try to obtain the address to an unknown "
              "type");
    }
    if (consume(&token, token, "sizeof")) {
        if (consume(&token, token, "(")) {
            const Type *ty = typename();
            if (ty == NULL) {
                Node *node = expr(env);
                if (node) {
                    ty = node->ty;
                }
            }
            expect(&token, token, ")");

            if (ty) {
                return new_node_num(sizeof_ty(ty));
            }
        } else {
            Node *node = unary(env);
            if (node->ty) {
                return new_node_num(sizeof_ty(node->ty));
            }
        }
        error("Internal compile error: try to obtain the size of an unknown "
              "type");
    }

    return desugar_index(env);
}

//
//  mul = unary ( "*" unary | "/" unary )*
//
Node *mul(Env *env) {
    Node *node = unary(env);

    for (;;) {
        if (consume(&token, token, "*"))
            node = new_node(ND_MUL, node, unary(env));
        else if (consume(&token, token, "/"))
            node = new_node(ND_DIV, node, unary(env));
        else
            return node;
    }
}

//
//  add = mul ( "+" mul | "-" mul )*
//
Node *add(Env *env) {
    Node *node = mul(env);

    for (;;) {
        if (consume(&token, token, "+"))
            node = new_node(ND_ADD, as_ptr(node), as_ptr(mul(env)));
        else if (consume(&token, token, "-"))
            node = new_node(ND_SUB, as_ptr(node), as_ptr(mul(env)));
        else
            return node;
    }
}

//
//  relational= add ( "<" add | "<=" add | ">" add | ">=" add )*
//
Node *relational(Env *env) {
    Node *node = add(env);

    for (;;) {
        if (consume(&token, token, "<")) {
            Node *rhs = add(env);
            if (!is_same_type(node->ty, rhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LT, node, rhs);
        } else if (consume(&token, token, "<=")) {
            Node *rhs = add(env);
            if (!is_same_type(node->ty, rhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LE, node, rhs);
        } else if (consume(&token, token, ">")) {
            Node *lhs = add(env);
            if (!is_same_type(node->ty, lhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LT, lhs, node);
        } else if (consume(&token, token, ">=")) {
            Node *lhs = add(env);
            if (!is_same_type(node->ty, lhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LE, lhs, node);
        } else {
            return node;
        }

        node->ty = &INT_T;
    }
}

//
//  equality = relational ( "==" relational | "!=" relational )*
//
Node *equality(Env *env) {
    Node *node = relational(env);

    for (;;) {
        if (consume(&token, token, "=="))
            node = new_node(ND_EQ, node, relational(env));
        else if (consume(&token, token, "!="))
            node = new_node(ND_NE, node, relational(env));
        else
            return node;
    }
}

//
//  assign = equality ( "=" assign )?
//
Node *assign(Env *env) {
    Node *node = equality(env);
    if (consume(&token, token, "=")) {
        const Type *ty = node->ty;
        node = new_node(ND_ASSIGN, node, as_ptr(assign(env)));
        node->ty = ty;
    }
    return node;
}

//
//  expr = assign
//
Node *expr(Env *env) { return assign(env); }

Node *stmt(Env *env);

// try to parse a type and an ident.
// For parse a function and a declare.
Node *type_ident() {
    const Type *ty = type();
    if (ty == NULL)
        return NULL;
    Node *node = calloc(1, sizeof(Node));
    node->ty = ty;
    const Token *ident = consume_ident(&token, token);
    if (ident == NULL) {
        unexpected("an ident", token);
    }
    node->ident = ident->span;
    return node;
}

//
//  function = type ident
//             "(" (type ident ("," type ident)*)? ")"
//             "{" stmt* "}"
//
Node *function(Env *parent, Node *node) {
    if (!consume(&token, token, "("))
        return NULL;

    Env env = make_scope(parent);
    int argument_offset = 0;

    node->kind = ND_FUNC;

    if (!consume(&token, token, ")")) {
        Node *arg = type_ident();
        arg->kind = ND_FUNC_ARGS;
        node->lhs = arg;

        declare_var(&env, arg->ty, &arg->ident);

        while (consume(&token, token, ",")) {
            arg = type_ident();
            arg->kind = ND_FUNC_ARGS;
            arg->lhs = node->lhs;
            node->lhs = arg;

            declare_var(&env, arg->ty, &arg->ident);
        }
        expect(&token, token, ")");

        argument_offset = env.maximum_offset;
        node->lhs->val = argument_offset / 8;
        if (node->lhs->val > 6)
            error("Not supported: more than 6 arguments.");
    }

    expect(&token, token, "{");
    Node *body = node;
    while (!consume(&token, token, "}")) {
        body->rhs = calloc(1, sizeof(Node));
        body->rhs->kind = ND_FUNC_BODY;
        body->rhs->lhs = stmt(&env);
        body = body->rhs;
    }
    if (node->rhs) {
        int variables_offset = env.maximum_offset - argument_offset;
        node->rhs->val = variables_offset;
    }

    return node;
}

//
//  init = expr | "{" init ( "," init )* "}"
//
Node *init(Env *env) {
    if (consume(&token, token, "{")) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_INIT;
        node->next = init(env);
        node->num_initializers = 1;
        Node *n = node->next;
        while (consume(&token, token, ",")) {
            n->next = init(env);
            n = n->next;
            node->num_initializers++;
        }
        expect(&token, token, "}");
        return node;
    }
    return expr(env);
}

//
//  declare = type ident ( "[" num "]" )? ( "=" init )? ";"
//
Node *declare(Env *env, Node *node) {
    node->kind = ND_DECLARE;
    bool is_array = false;
    bool is_known_size = false;

    Type *array_ty = NULL;

    if (consume(&token, token, "[")) {
        is_array = true;
        int array_size = -1;
        is_known_size = number(&array_size);
        expect(&token, token, "]");

        array_ty = calloc(1, sizeof(Type));
        array_ty->ty = ARRAY;
        array_ty->ptr_to = node->ty;
        array_ty->array_size = array_size;
    }

    if (consume(&token, token, "=")) {
        node->init = as_ptr(init(env));
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

    expect(&token, token, ";");

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
Node *stmt(Env *env) {
    Node *node = NULL;

    if (consume(&token, token, "if")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_IF_COND;
        expect(&token, token, "(");
        node->lhs = expr(env);
        expect(&token, token, ")");

        node->rhs = calloc(1, sizeof(Node));
        node->rhs->kind = ND_IF_BODY;
        node->rhs->lhs = stmt(env);
        if (consume(&token, token, "else")) {
            node->rhs->rhs = stmt(env);
        }
    } else if (consume(&token, token, "while")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_WHILE;
        expect(&token, token, "(");
        node->lhs = expr(env);
        expect(&token, token, ")");
        node->rhs = stmt(env);
    } else if (consume(&token, token, "for")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_FOR_INIT;
        expect(&token, token, "(");
        if (!consume(&token, token, ";")) {
            node->lhs = expr(env);
            expect(&token, token, ";");
        }

        node->rhs = calloc(1, sizeof(Node));
        node->rhs->kind = ND_FOR_COND;
        if (!consume(&token, token, ";")) {
            node->rhs->lhs = expr(env);
            expect(&token, token, ";");
        }

        node->rhs->rhs = calloc(1, sizeof(Node));
        node->rhs->rhs->kind = ND_FOR_BODY;
        if (!consume(&token, token, ")")) {
            node->rhs->rhs->lhs = expr(env);
            expect(&token, token, ")");
        }
        node->rhs->rhs->rhs = stmt(env);
    } else if (consume(&token, token, "return")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_RETURN;
        node->lhs = as_ptr(expr(env));
        expect(&token, token, ";");
    } else if (consume(&token, token, "{")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_BLOCK;
        Node *current = node;
        while (!consume(&token, token, "}")) {
            current->lhs = stmt(env);
            current->rhs = calloc(1, sizeof(Node));
            current = current->rhs;
            current->kind = ND_BLOCK;
        }
    } else {
        node = type_ident();
        if (node) {
            node = declare(env, node);
        } else {
            node = calloc(1, sizeof(Node));
            node->kind = ND_SEMICOLON;
            node->lhs = expr(env);
            expect(&token, token, ";");
        }
    }

    return node;
}

//
//  program = ( function | declare )*
//
void program(Env *env, Node *code[]) {
    int i = 0;
    while (!at_eof()) {
        Node *node = type_ident();
        if (node == NULL)
            error("Cannot parse the program.");

        Node *fn = function(env, node);
        if (fn)
            code[i++] = fn;
        else
            code[i++] = declare(env, node);
    }
    code[i] = NULL;
}
