#pragma once

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
    ND_ADD,    // "+"
    ND_SUB,    // "-"
    ND_MUL,    // "*"
    ND_DIV,    // "/"
    ND_NUM,    // [0-9]+
    ND_LT,     // "<"
    ND_LE,     // "<="
    ND_EQ,     // "=="
    ND_NE,     // "!="
    ND_ASSIGN, // "="
    ND_LVAR,   // [a-z]+
} NodeKind;

typedef struct Node Node;

struct Node {
    NodeKind kind;
    Node *lhs;
    Node *rhs;
    int val;    // for ND_NUM
    int offset; // for ND_LVAR
};

typedef struct LVar LVar;

struct LVar {
    LVar *next;
    char *name;
    int len;
    int offset;
};

extern char *user_input;
extern Token *token;
extern Node *code[100];
extern LVar *locals;

void error(char *fmt, ...);

Token *tokenize(char *p);

LVar *find_lvar(Token *tok);

Node *expr();
void program();

void gen(Node *node);
