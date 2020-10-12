#pragma once
#include <stdarg.h>
#include <stdio.h>

typedef enum {
    TK_RESERVED,
    TK_IDENT,
    TK_NUM,
    TK_EOF,
} TokenKind;

typedef struct Token Token;

struct Token {
    TokenKind kind;
    Token *next;
    int val;
    char *str;
    int len;
};

typedef enum {
    ND_ADD,       // "+"
    ND_SUB,       // "-"
    ND_MUL,       // "*"
    ND_DIV,       // "/"
    ND_NUM,       // [0-9]+
    ND_LT,        // "<"
    ND_LE,        // "<="
    ND_EQ,        // "=="
    ND_NE,        // "!="
    ND_ASSIGN,    // "="
    ND_LVAR,      // [a-zA-Z_][a-zA-Z0-9_]*
    ND_RETURN,    // "return"
    ND_IF_COND,   // "if" "(" expr ")"
    ND_IF_BODY,   // stmt ("else" stmt)?
    ND_WHILE,     // "while"
    ND_FOR_INIT,  // "for" "(" expr?;
    ND_FOR_COND,  // expr?;
    ND_FOR_BODY,  // expr?; ")" stmt
    ND_BLOCK,     // "{" stmt* "}"
    ND_CALL,      // <function call>
    ND_ARGS,      // function call arguments
    ND_FUNC,      // function definition
    ND_FUNC_ARGS, // function arguments
    ND_FUNC_BODY, // function body
    ND_ADDR,      // "&"
    ND_DEREF,     // "*"
    ND_DECLARE,
} NodeKind;

typedef struct Node Node;

struct Node {
    NodeKind kind;
    Node *lhs;
    Node *rhs;
    int val;    // for ND_NUM
    int offset; // for ND_LVAR
    char *func; // for ND_CALL
    int len;
};

typedef struct LVar LVar;

struct LVar {
    LVar *next;
    char *name;
    int len;
    int offset;
};

typedef struct Env Env;

struct Env {
    LVar *locals;
    int maximum_offset;
};

extern char *user_input;
extern Token *token;

void error(char *fmt, ...);
void error_at(char *loc, int len, char *fmt, ...);

void tokenize(char *p);

LVar *get_lvar(Env *env, Token *tok);
LVar *declare_lvar(Env *env, Token *tok);

Node *expr();
void program();

void gen(Node *node);
void gen_func(Node *node);

static void debug(char *fmt, ...) {
    fprintf(stderr, "[debug] ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
}
