#include "mycc.h"
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// # EBNF
//
//  program     =  ( function | declare )*
//  stmt        =  expr ";"
//               | declare
//               | "{" stmt* "}"
//               | "if" "(" expr ")" stmt ( "else" stmt )?
//               | "while" "(" expr ")" stmt
//               | "for" "(" expr? ";" expr? ";" expr? ")" stmt
//               | "return" expr ";"
//  declare     =  type ident ( "[" num "]" )? ( "=" init )? ";"
//  init        =  expr | "{" init ( "," init )* "}"
//  function    =  type ident
//                 "(" (type ident ("," type ident)*)? ")"
//                 "{" stmt* "}"
//  expr        =  assign
//  assign      =  equality ( "=" assign )?
//  equality    =  relational ( "==" relational | "!=" relational )*
//  relational  =  add ( "<" add | "<=" add | ">" add | ">=" add )*
//  add         =  mul ( "+" mul | "-" mul )*
//  mul         =  unary ( "*" unary | "/" unary )*
//  unary       =  ( "+" | "-" )? primary ( "[" expr "]" )?
//               | ( "*" | "&" | "sizeof" ) unary
//  primary     =  num
//               | ident ( "(" ( expr ( "," expr )* )? ")" )?
//               | "(" expr ")"
//               | string
//  type        = type "*" | "int" | "char"
//

static const char *filename;
static const char *user_input;
static const Token *token;

const char *reserved[] = {"if",  "else",   "while", "for", "return",
                          "int", "sizeof", "char",  NULL};

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

bool consume(char *op) {
    if (token->kind != TK_RESERVED || strlen(op) != token->span.len ||
        memcmp(token->span.ptr, op, token->span.len))
        return false;
    token = token->next;
    return true;
}

bool is_reserved(const Token *token, char *op) {
    if (token == NULL || token->kind != TK_RESERVED ||
        strlen(op) != token->span.len ||
        memcmp(token->span.ptr, op, token->span.len))
        return false;
    return true;
}

const Token *consume_ident() {
    if (token->kind != TK_IDENT)
        return NULL;
    const Token *tok = token;
    token = token->next;
    return tok;
}

const Token *expect_ident() {
    if (token->kind != TK_IDENT) {
        error_at(&token->span,
                 "Unexpected token: an ident expected, but got '%.*s'",
                 token->span.len, token->span.ptr);
    }

    const Token *tok = token;
    token = token->next;
    return tok;
}

const Token *consume_string() {
    if (token->kind != TK_STRING) {
        return NULL;
    }
    const Token *tok = token;
    token = token->next;
    return tok;
}

bool is_ident(const Token *token) { return token && token->kind == TK_IDENT; }

void expect(char *op) {
    if (token->kind != TK_RESERVED || strlen(op) != token->span.len ||
        memcmp(token->span.ptr, op, token->span.len)) {
        error_at(&token->span,
                 "Unexpected token: '%s': expected, but got '%.*s'", op,
                 token->span.len, token->span.ptr);
    }
    token = token->next;
}

bool number(int *val) {
    if (token->kind != TK_NUM) {
        return false;
    }

    *val = token->val;
    token = token->next;

    return true;
}

int expect_number() {
    int val;
    if (!number(&val)) {
        error_at(&token->span, "Unexpected token: a number expected.");
    }
    return val;
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

Type *type() {
    Type *ty = NULL;
    if (consume("int")) {
        ty = &INT_T;
    } else if (consume("char")) {
        ty = &CHAR_T;
    } else {
        return NULL;
    }

    const Token *tok = token;
    while (is_reserved(tok, "*")) {
        tok = tok->next;

        Type *new = calloc(1, sizeof(Type));
        new->ty = PTR;
        new->ptr_to = ty;
        ty = new;
    }

    token = tok;
    return ty;
}

bool is_subtype(Type *base, Type *derived) {
    if (base == NULL || derived == NULL) {
        return false;
    }

    switch (base->ty) {
    case INT:
        return derived->ty == INT || derived->ty == CHAR;
    case PTR:
        if (derived->ty == PTR) {
            return is_subtype(base->ptr_to, derived->ptr_to);
        }
        return derived->ty == INT || derived->ty == CHAR;
    case ARRAY:
        if (derived->ty == ARRAY && base->array_size == derived->array_size) {
            return is_subtype(base->ptr_to, derived->ptr_to);
        }
        return false;
    case CHAR:
        return derived->ty == CHAR;
    default:
        return false;
    }
}

bool is_same_type(Type *lhs, Type *rhs) {
    return is_subtype(lhs, rhs) && is_subtype(rhs, lhs);
}

Type *check_type(Type *lhs, Type *rhs) {
    if (is_subtype(lhs, rhs)) {
        return lhs;
    }

    if (is_subtype(rhs, lhs)) {
        return rhs;
    }

    return NULL;
}

Type *get_type(Node *lhs, Node *rhs) {
    if (lhs == NULL || rhs == NULL)
        return NULL;
    Type *ty = check_type(lhs->ty, rhs->ty);
    if (ty == NULL) {
        error("Type Mismatched: '%s' and '%s'", type_to_str(lhs->ty),
              type_to_str(rhs->ty));
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
    ptr->ty = calloc(1, sizeof(Type));
    ptr->ty->ty = PTR;
    ptr->ty->ptr_to = array->ty->ptr_to;
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

Node *primary(Env *env) {
    if (consume("(")) {
        Node *node = expr(env);
        expect(")");
        return node;
    }

    const Token *tok = consume_ident();
    if (tok) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_LVAR;

        // function call
        if (consume("(")) {
            node->kind = ND_CALL;
            node->ident = tok->span;
            if (consume(")"))
                return node;

            node->lhs = calloc(1, sizeof(Node));
            Node *current = node->lhs;
            current->kind = ND_ARGS;
            current->lhs = expr(env);
            while (!consume(")")) {
                expect(",");
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
        node->ident = var->ident;

        return node;
    }

    tok = consume_string();
    if (tok) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_STRING;
        node->ident = tok->span;
        node->ident.ptr++;
        node->ident.len -= 2;
        node->ty = calloc(1, sizeof(Type));
        node->ty->ty = PTR;
        node->ty->ptr_to = calloc(1, sizeof(Type));
        node->ty->ptr_to->ty = CHAR;

        const String *string = push_string(env, &node->ident);
        node->val = string->index;

        return node;
    }

    return new_node_num(expect_number());
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
    if (consume("[")) {
        Node *index = expr(env);
        expect("]");
        return deref_offset_ptr(as_ptr(node), index);
    }
    return node;
}

Node *unary(Env *env) {
    if (consume("+")) {
        return desugar_index(env);
    }
    if (consume("-")) {
        return new_node(ND_SUB, new_node_num(0), desugar_index(env));
    }
    if (consume("*")) {
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
    if (consume("&")) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_ADDR;
        node->lhs = unary(env);
        if (node->lhs && node->lhs->ty) {
            node->ty = calloc(1, sizeof(Type));
            node->ty->ty = PTR;
            if (node->lhs->ty->ty == ARRAY) {
                node->ty->ptr_to = node->lhs->ty->ptr_to;
            } else {
                node->ty->ptr_to = node->lhs->ty;
            }
            return node;
        }
        error("Internal compile error: try to obtain the address to an unknown "
              "type");
    }
    if (consume("sizeof")) {
        Node *node = unary(env);
        if (node->ty)
            return new_node_num(sizeof_ty(node->ty));
        error("Internal compile error: try to obtain the size of an unknown "
              "type");
    }

    return desugar_index(env);
}

Node *mul(Env *env) {
    Node *node = unary(env);

    for (;;) {
        if (consume("*"))
            node = new_node(ND_MUL, node, unary(env));
        else if (consume("/"))
            node = new_node(ND_DIV, node, unary(env));
        else
            return node;
    }
}

Node *add(Env *env) {
    Node *node = mul(env);

    for (;;) {
        if (consume("+"))
            node = new_node(ND_ADD, as_ptr(node), as_ptr(mul(env)));
        else if (consume("-"))
            node = new_node(ND_SUB, as_ptr(node), as_ptr(mul(env)));
        else
            return node;
    }
}

Node *relational(Env *env) {
    Node *node = add(env);

    for (;;) {
        if (consume("<")) {
            Node *rhs = add(env);
            if (!is_same_type(node->ty, rhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LT, node, rhs);
        } else if (consume("<=")) {
            Node *rhs = add(env);
            if (!is_same_type(node->ty, rhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LE, node, rhs);
        } else if (consume(">")) {
            Node *lhs = add(env);
            if (!is_same_type(node->ty, lhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LT, lhs, node);
        } else if (consume(">=")) {
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

Node *equality(Env *env) {
    Node *node = relational(env);

    for (;;) {
        if (consume("=="))
            node = new_node(ND_EQ, node, relational(env));
        else if (consume("!="))
            node = new_node(ND_NE, node, relational(env));
        else
            return node;
    }
}

Node *assign(Env *env) {
    Node *node = equality(env);
    if (consume("=")) {
        Type *ty = node->ty;
        node = new_node(ND_ASSIGN, node, as_ptr(assign(env)));
        node->ty = ty;
    }
    return node;
}

Node *expr(Env *env) { return assign(env); }

Node *stmt(Env *env);

// try to parse a type and an ident.
// For parse a function and a declare.
Node *type_ident() {
    Type *ty = type();
    if (ty == NULL)
        return NULL;
    Node *node = calloc(1, sizeof(Node));
    node->ty = ty;
    node->ident = expect_ident()->span;
    return node;
}

Node *function(Env *parent, Node *node) {
    if (!consume("("))
        return NULL;

    Env env = make_scope(parent);
    int argument_offset = 0;

    node->kind = ND_FUNC;

    if (!consume(")")) {
        Node *arg = type_ident();
        arg->kind = ND_FUNC_ARGS;
        node->lhs = arg;

        declare_var(&env, arg->ty, &arg->ident);

        while (consume(",")) {
            arg = type_ident();
            arg->kind = ND_FUNC_ARGS;
            arg->lhs = node->lhs;
            node->lhs = arg;

            declare_var(&env, arg->ty, &arg->ident);
        }
        expect(")");

        argument_offset = env.maximum_offset;
        node->lhs->val = argument_offset / 8;
        if (node->lhs->val > 6)
            error("Not supported: more than 6 arguments.");
    }

    expect("{");
    Node *body = node;
    while (!consume("}")) {
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

Node *init(Env *env) {
    if (consume("{")) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_INIT;
        node->next = init(env);
        node->num_initializers = 1;
        Node *n = node->next;
        while (consume(",")) {
            n->next = init(env);
            n = n->next;
            node->num_initializers++;
        }
        expect("}");
        return node;
    }
    return expr(env);
}

Node *declare(Env *env, Node *node) {
    node->kind = ND_DECLARE;
    bool is_array = false;
    bool is_known_size = false;

    if (consume("[")) {
        is_array = true;
        int array_size = -1;
        is_known_size = number(&array_size);
        expect("]");

        Type *array_ty = calloc(1, sizeof(Type));
        array_ty->ty = ARRAY;
        array_ty->ptr_to = node->ty;
        array_ty->array_size = array_size;
        node->ty = array_ty;
    }

    if (consume("=")) {
        node->init = init(env);
        if (is_array && !is_known_size) {
            switch (node->init->kind) {
            case (ND_INIT):
                node->ty->array_size = node->init->num_initializers;
                break;
            case (ND_STRING):
                node->ty->array_size = node->init->ident.len;
                break;
            default:
                error("array must be initialized with a brace-enclosed "
                      "initializer");
            }
        }
    }

    const Var *var = declare_var(env, node->ty, &node->ident);
    node->vkind = var->kind;
    node->offset = var->offset;

    expect(";");

    return node;
}

Node *stmt(Env *env) {
    Node *node = NULL;

    if (consume("if")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_IF_COND;
        expect("(");
        node->lhs = expr(env);
        expect(")");

        node->rhs = calloc(1, sizeof(Node));
        node->rhs->kind = ND_IF_BODY;
        node->rhs->lhs = stmt(env);
        if (consume("else")) {
            node->rhs->rhs = stmt(env);
        }
    } else if (consume("while")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_WHILE;
        expect("(");
        node->lhs = expr(env);
        expect(")");
        node->rhs = stmt(env);
    } else if (consume("for")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_FOR_INIT;
        expect("(");
        if (!consume(";")) {
            node->lhs = expr(env);
            expect(";");
        }

        node->rhs = calloc(1, sizeof(Node));
        node->rhs->kind = ND_FOR_COND;
        if (!consume(";")) {
            node->rhs->lhs = expr(env);
            expect(";");
        }

        node->rhs->rhs = calloc(1, sizeof(Node));
        node->rhs->rhs->kind = ND_FOR_BODY;
        if (!consume(")")) {
            node->rhs->rhs->lhs = expr(env);
            expect(")");
        }
        node->rhs->rhs->rhs = stmt(env);
    } else if (consume("return")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_RETURN;
        node->lhs = as_ptr(expr(env));
        expect(";");
    } else if (consume("{")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_BLOCK;
        Node *current = node;
        while (!consume("}")) {
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
            expect(";");
        }
    }

    return node;
}

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
