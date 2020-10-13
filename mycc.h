#pragma once
#include <stdarg.h>
#include <stdio.h>

typedef struct Span Span;
struct Span {
    const char *ptr;
    int len;
};

typedef enum {
    TK_RESERVED,
    TK_IDENT,
    TK_NUM,
    TK_STRING,
    TK_EOF,
} TokenKind;

typedef struct Token Token;

struct Token {
    TokenKind kind;
    Token *next;
    int val;
    Span span;
};

typedef struct Type Type;
struct Type {
    enum { INT, PTR, ARRAY, CHAR } ty;
    Type *ptr_to;
    size_t array_size;
};

typedef enum {
    VLOCAL,
    VGLOBAL,
} VarKind;

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
    ND_STRING,
    ND_SEMICOLON,
} NodeKind;

typedef struct Node Node;
struct Node {
    NodeKind kind;
    Type *ty;

    // For ND_NUM,
    int val;

    // For operators: ND_ADD, ND_SUB, ...
    Node *lhs;
    Node *rhs;

    // For ND_LVAR
    int offset;

    // For ND_LVAR, ND_FUNC, ND_CALL
    Span ident;

    // For ND_DECLARE
    VarKind vkind;
};

typedef struct LVar LVar;
struct LVar {
    VarKind kind;
    LVar *next;
    Type *ty;
    int offset;

    Span ident;
};

typedef struct String String;
struct String {
    String *next;
    int index;
    Span ident;
};

typedef struct Env Env;
struct Env {
    Env *parent;
    LVar *locals;
    String *strings;
    int maximum_offset;
    int maximum_strings;
};

static inline Env init_env() {
    Env env = {NULL, NULL, NULL, 0, 0};
    return env;
}

static inline Env make_scope(Env *parent) {
    Env env = {parent, NULL, NULL, 0, 0};
    return env;
}

extern char *filename;
extern char *user_input;
extern Token *token;

void error(char *fmt, ...);
void error_at(const Span *span, char *fmt, ...);

void tokenize(char *p);

size_t sizeof_ty(Type *ty);
char *type_to_str(Type *ty);

const LVar *get_lvar(Env *env, const Span *ident);
const LVar *declare_lvar(Env *env, Type *ty, const Span *ident);
const String *push_string(Env *env, const Span *ident);

Node *expr();
void program(Env *env, Node *code[]);

void gen(Node *node);
void gen_top(Node *node);
void gen_strings(Env *env);

static inline void debug(char *fmt, ...) {
    fprintf(stderr, "[debug] ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
}
