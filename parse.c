#include "mycc.h"
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// # EBNF
//
//  program     =  function*
//  stmt        =  expr ";"
//               | declare
//               | "{" stmt* "}"
//               | "if" "(" expr ")" stmt ("else" stmt)?
//               | "while" "(" expr ")" stmt
//               | "for" "(" expr? ";" expr? ";" expr? ")" stmt
//               | "return" expr ";"
//  declare     =  "int" ident ";"
//  function    =  ident "(" (ident ("," ident)*)? ")" "{" stmt* "}"
//  expr        =  assign
//  assign      =  equality ("=" assign)?
//  equality    =  relational ("==" relational | "!=" relational)*
//  relational  =  add ("<" add | "<=" add | ">" add | ">=" add)*
//  add         =  mul ("+" mul | "-" mul)*
//  mul         =  unary ( "*" unary | "/" unary)*
//  unary       =  ("+" | "-")? primary | ("*" | "&") unary
//  primary     =  num
//               | ident ("(" (expr ("," expr)*)? ")")?
//               | "(" expr ")"
//

char *user_input;
Token *token;

char *reserved[] = {"if", "else", "while", "for", "return", "int", NULL};

void error_at(char *loc, int len, char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);

    int pos = loc - user_input;
    fprintf(stderr, "%s\n", user_input);
    fprintf(stderr, "%*s", pos, "");
    for (int i = 0; i < len; i++)
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
    if (token->kind != TK_RESERVED || strlen(op) != token->len ||
        memcmp(token->str, op, token->len))
        return false;
    token = token->next;
    return true;
}

bool is_reserved(Token *token, char *op) {
    if (token == NULL || token->kind != TK_RESERVED ||
        strlen(op) != token->len || memcmp(token->str, op, token->len))
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
        char *got = calloc(token->len + 1, sizeof(char));
        memcpy(got, token->str, token->len);
        error_at(token->str, token->len,
                 "Unexpected token: an ident expected, but got '%s'", got);
    }

    Token *tok = token;
    token = token->next;
    return tok;
}

bool is_ident(Token *token) { return token && token->kind == TK_IDENT; }

void expect(char *op) {
    if (token->kind != TK_RESERVED || strlen(op) != token->len ||
        memcmp(token->str, op, token->len)) {
        char *got = calloc(token->len + 1, sizeof(char));
        memcpy(got, token->str, token->len);
        error_at(token->str, token->len,
                 "Unexpected token: '%s': expected, but got '%s'", op, got);
    }
    token = token->next;
}

int expect_number() {
    if (token->kind != TK_NUM)
        error_at(token->str, token->len,
                 "Unexpected token: a number expected.");
    int val = token->val;
    token = token->next;
    return val;
}

bool is_function() {
    Token *next = token;
    if (!is_ident(next))
        return false;

    next = next->next;
    if (!is_reserved(next, "("))
        return false;

    next = next->next;
    if (!is_reserved(next, ")")) {
        if (!is_ident(next))
            return false;

        next = next->next;
        while (is_reserved(next, ",")) {
            next = next->next;
            if (!is_ident(next))
                return false;

            next = next->next;
        }

        if (!is_reserved(next, ")"))
            return false;
    }

    next = next->next;
    if (!is_reserved(next, "{"))
        return false;

    return true;
}

bool at_eof() { return token->kind == TK_EOF; }

Token *new_token(TokenKind kind, Token *cur, char *str, int len) {
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->str = str;
    tok->len = len;
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
                error_at(p, 1, "Failed to tokenize");
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
            *p == '&') {
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

        if ('a' <= *p && *p <= 'z' || 'A' <= *p && *p <= 'Z' || *p == '_') {
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

        error_at(p, 1, "Failed to tokenize");
    }

    new_token(TK_EOF, cur, p, 0);
    token = head.next;
}

Node *new_node(NodeKind kind, Node *lhs, Node *rhs) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = kind;
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

Node *new_node_num(int val) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_NUM;
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
            node->func = tok->str;
            node->len = tok->len;
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

        LVar *lvar = get_lvar(env, tok);
        node->offset = lvar->offset;

        return node;
    }

    return new_node_num(expect_number());
}

Node *unary(Env *env) {
    if (consume("+"))
        return primary(env);
    if (consume("-"))
        return new_node(ND_SUB, new_node_num(0), primary(env));
    if (consume("*"))
        return new_node(ND_DEREF, unary(env), NULL);
    if (consume("&"))
        return new_node(ND_ADDR, unary(env), NULL);
    return primary(env);
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
            node = new_node(ND_ADD, node, mul(env));
        else if (consume("-"))
            node = new_node(ND_SUB, node, mul(env));
        else
            return node;
    }
}

Node *relational(Env *env) {
    Node *node = add(env);

    for (;;) {
        if (consume("<"))
            node = new_node(ND_LT, node, add(env));
        else if (consume("<="))
            node = new_node(ND_LE, node, add(env));
        else if (consume(">"))
            node = new_node(ND_LT, add(env), node);
        else if (consume(">="))
            node = new_node(ND_LE, add(env), node);
        else
            return node;
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
    if (consume("="))
        node = new_node(ND_ASSIGN, node, assign(env));
    return node;
}

Node *expr(Env *env) { return assign(env); }

Node *stmt(Env *env);

Node *function() {
    Env env = {NULL, 0};
    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_FUNC;
    Token *tok = expect_ident();
    node->func = tok->str;
    node->len = tok->len;

    int argument_offset = 0;
    expect("(");
    if (!consume(")")) {
        tok = expect_ident();
        declare_lvar(&env, tok);
        Node *new = calloc(1, sizeof(Node));
        new->kind = ND_FUNC_ARGS;
        new->func = tok->str;
        new->len = tok->len;

        node->lhs = new;
        while (consume(",")) {
            tok = expect_ident();
            declare_lvar(&env, tok);
            new = calloc(1, sizeof(Node));
            new->kind = ND_FUNC_ARGS;
            new->func = tok->str;
            new->len = tok->len;
            new->lhs = node->lhs;
            node->lhs = new;
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
        node->lhs = expr(env);
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
    } else if (consume("int")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_DECLARE;
        Token *tok = expect_ident();
        declare_lvar(env, tok);
        node->func = tok->str;
        node->len = tok->len;
        expect(";");
    } else if (is_function()) {
        return NULL;
        /* node = function(); */
    } else {
        node = expr(env);
        expect(";");
    }

    return node;
}

void program(Node *code[]) {
    int i = 0;
    while (!at_eof()) {
        code[i++] = function();
        code[i] = NULL;
    }
}
