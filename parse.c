#include "mycc.h"
#include <ctype.h>
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
//  declare     =  type ident ( "[" num "]" )? ";"
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
//  type        = type "*" | "int" | "char"
//

char *user_input;
Token *token;

char *reserved[] = {"if",  "else",   "while", "for", "return",
                    "int", "sizeof", "char",  NULL};

void error_at(const Span *span, char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);

    int pos = span->ptr - user_input;
    fprintf(stderr, "%s\n", user_input);
    fprintf(stderr, "%*s", pos, "");
    for (int i = 0; i < span->len; i++)
        fprintf(stderr, "^");
    fprintf(stderr, " ");
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

bool is_reserved(Token *token, char *op) {
    if (token == NULL || token->kind != TK_RESERVED ||
        strlen(op) != token->span.len ||
        memcmp(token->span.ptr, op, token->span.len))
        return false;
    return true;
}

Token *consume_ident() {
    if (token->kind != TK_IDENT)
        return NULL;
    Token *tok = token;
    token = token->next;
    return tok;
}

Token *expect_ident() {
    if (token->kind != TK_IDENT) {
        error_at(&token->span,
                 "Unexpected token: an ident expected, but got '%.*s'",
                 token->span.len, token->span.ptr);
    }

    Token *tok = token;
    token = token->next;
    return tok;
}

bool is_ident(Token *token) { return token && token->kind == TK_IDENT; }

void expect(char *op) {
    if (token->kind != TK_RESERVED || strlen(op) != token->span.len ||
        memcmp(token->span.ptr, op, token->span.len)) {
        error_at(&token->span,
                 "Unexpected token: '%s': expected, but got '%.*s'", op,
                 token->span.len, token->span.ptr);
    }
    token = token->next;
}

int expect_number() {
    if (token->kind != TK_NUM)
        error_at(&token->span, "Unexpected token: a number expected.");
    int val = token->val;
    token = token->next;
    return val;
}

bool at_eof() { return token->kind == TK_EOF; }

Token *new_token(TokenKind kind, Token *cur, char *str, int len) {
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->span.ptr = str;
    tok->span.len = len;
    cur->next = tok;
    return tok;
}

void tokenize(char *p) {
    Token head;
    head.next = NULL;
    Token *cur = &head;

    while (*p) {
        if (isspace(*p)) {
            p++;
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
            char *first = p;
            int val = strtol(p, &p, 10);
            int len = p - first;
            cur = new_token(TK_NUM, cur, first, len);
            cur->val = val;
            continue;
        }

        if (('a' <= *p && *p <= 'z') || ('A' <= *p && *p <= 'Z') || *p == '_') {
            char *first = p;

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
    Type *ty = calloc(1, sizeof(Type));
    if (consume("int")) {
        ty->ty = INT;
    } else if (consume("char")) {
        ty->ty = CHAR;
    } else {
        free(ty);
        return NULL;
    }

    Token *tok = token;
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

Type *expect_type() {
    Type *ty = type();
    if (ty == NULL) {
        error_at(&token->span, "Unknown type");
    }

    return ty;
}

char *type_to_str(Type *ty) {
    int depth = 0;
    Type *tmp;
    for (tmp = ty; tmp; tmp = tmp->ptr_to) {
        switch (tmp->ty) {
        case INT:
            depth += 3; // length of "int"
            break;
        case PTR:
            depth += 1; // length of "*"
            break;
        case ARRAY:
            depth += snprintf(NULL, 0, "[%zu]", tmp->array_size);
            break;
        case CHAR:
            depth += 4; // length of "char"
            break;
        default:
            error("Not implemented for this type: type_to_str()");
        }
    }

    char *buffer = calloc(depth + 1, sizeof(char));
    for (tmp = ty; tmp; tmp = tmp->ptr_to) {
        switch (tmp->ty) {
        case INT:
            depth -= 3; // length of "int"
            memcpy(buffer + depth, "int", 3);
            break;
        case PTR:
            depth -= 1; // length of "*"
            memcpy(buffer + depth, "*", 2);
            break;
        case ARRAY:
            depth -= snprintf(NULL, 0, "[%zu]", tmp->array_size);
            sprintf(buffer + depth, "[%zu]", tmp->array_size);
            break;
        case CHAR:
            depth -= 4; // length of "char"
            memcpy(buffer + depth, "char", 4);
            break;
        default:
            error("Not implemented for this type: type_to_str()");
        }
    }

    return buffer;
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
    node->ty = calloc(1, sizeof(Type));
    node->ty->ty = INT;
    node->val = val;
    return node;
}

Node *primary(Env *env) {
    if (consume("(")) {
        Node *node = expr(env);
        expect(")");
        return node;
    }

    Token *tok = consume_ident();
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

        const LVar *lvar = get_lvar(env, &tok->span);
        node->ty = lvar->ty;
        node->offset = lvar->offset;
        node->vkind = lvar->kind;
        node->ident = lvar->ident;

        return node;
    }

    return new_node_num(expect_number());
}

// parse primary ("[" expr "]")?
Node *desugar_index(Env *env) {
    Node *node = primary(env);
    if (consume("[")) {
        Node *index = expr(env);
        expect("]");

        Node *new = calloc(1, sizeof(Node));
        new->kind = ND_DEREF;
        new->lhs = new_node(ND_ADD, as_ptr(node), as_ptr(index));
        if (new->lhs->ty && new->lhs->ty->ty == PTR) {
            new->ty = new->lhs->ty->ptr_to;
            return new;
        }
        error("Cannot deref: '%s'", type_to_str(new->lhs->ty));
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

        node->ty = calloc(1, sizeof(Type));
        node->ty->ty = INT;
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

        declare_lvar(&env, arg->ty, &arg->ident);

        while (consume(",")) {
            arg = type_ident();
            arg->kind = ND_FUNC_ARGS;
            arg->lhs = node->lhs;
            node->lhs = arg;

            declare_lvar(&env, arg->ty, &arg->ident);
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

Node *declare(Env *env, Node *node) {
    node->kind = ND_DECLARE;

    if (consume("[")) {
        size_t array_size = expect_number();
        expect("]");

        Type *array_ty = calloc(1, sizeof(Type));
        array_ty->ty = ARRAY;
        array_ty->ptr_to = node->ty;
        array_ty->array_size = array_size;
        node->ty = array_ty;
    }

    const LVar *var = declare_lvar(env, node->ty, &node->ident);
    node->vkind = var->kind;
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
            node = expr(env);
            expect(";");
        }
    }

    return node;
}

void program(Node *code[]) {
    Env env = init_env();
    int i = 0;
    while (!at_eof()) {
        Node *node = type_ident();
        if (node == NULL)
            error("Cannot parse the program.");

        Node *fn = function(&env, node);
        if (fn)
            code[i++] = fn;
        else
            code[i++] = declare(&env, node);
    }
    code[i] = NULL;
}
