#pragma once
#include <stdbool.h>
#include <stdlib.h>

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
typedef struct Node Node;

typedef enum {
    VLOCAL,
    VGLOBAL,
} VarKind;

typedef struct Var Var;
struct Var {
    VarKind kind;
    const Type *ty;
    int offset;
    const char *ident;
};

typedef struct Initializer Initializer;

typedef struct InitList InitList;
struct InitList {
    InitList *next;
    const Initializer *inner;
};

struct Initializer {
    Node *expr;
    InitList *list;
    unsigned num_initializers;
};

typedef struct {
    Var *var;
    const Initializer *init;
} Declaration;

typedef struct ParamList ParamList;
struct ParamList {
    ParamList *next;
    Declaration *decl;
};

struct Type {
    enum { INTEGER, PTR, ARRAY, VOID, FUNCTION } ty;

    enum { CHAR, INT } ikind;

    const Type *ptr_to;
    int array_size;

    // For FUNCTION
    const Type *retty;
    const ParamList *args;
};

extern const Type INT_T;
extern const Type CHAR_T;
extern const Type VOID_T;

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
    ND_ADDR,      // "&"
    ND_DEREF,     // "*"
    ND_DECLARE,
    ND_STRING,
    ND_SEMICOLON,
} NodeKind;

typedef struct NodeList NodeList;

struct Node {
    NodeKind kind;
    const Type *ty;

    // For ND_NUM,
    int val;

    // For operators: ND_ADD, ND_SUB, ...
    Node *lhs;
    Node *rhs;

    // For ND_LVAR
    const Var *var;

    // For ND_FUNC, ND_CALL
    Span ident;

    // For ND_DECLARE
    const Declaration *decl;

    Node *next; // For the inner of ND_INIT

    // For ND_BODY
    NodeList *inner;
};

struct NodeList {
    NodeList *next;
    Node *node;
};

// Linked list for variables
typedef struct VarList VarList;
struct VarList {
    VarList *next;
    Var *var;
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
    VarList *vars;
    String *strings;
    int maximum_offset;
    int maximum_strings;
    unsigned int num_args;
    unsigned int maximum_arg_offset;
};

static inline Env
init_env() {
    Env env = {NULL, NULL, NULL, 0, 0, 0, 8};
    return env;
}

static inline Env
make_scope(Env *parent) {
    Env env = {parent, NULL, NULL, 0, 0, 0, 8};
    return env;
}

typedef struct Function Function;
struct Function {
    const char *ident;
    unsigned int num_args;
    int lvar_offset;
    const ParamList *args;
    Node *body;
};

typedef struct Unit Unit;
struct Unit {
    const Function *function;
    const Declaration *declaration;
};

void
debug(char *fmt, ...);
void
error(char *fmt, ...);
void
error_at(const Span *span, char *fmt, ...);

const Token *
tokenize(const char *path);

const Type *
mk_ptr(const Type *base);
const Type *
mk_array(const Type *base, int array_size);
const Type *
mk_func(const Type *retty, const ParamList *args);
size_t
sizeof_ty(const Type *ty);
char *
type_to_str(const Type *ty);
bool
is_same_type(const Type *lhs, const Type *rhs);
const Type *
get_type(const Node *lhs, const Node *rhs);

const Var *
get_var(Env *env, const char *ident);
bool
declare_arg(Env *env, Var *var);
bool
declare_var(Env *env, Var *var);
const String *
push_string(Env *env, const Span *ident);

void
program(const Token *token, Env *env, Unit *code[]);

Node *
as_ptr(Node *array);
Node *
new_node(NodeKind kind, Node *lhs, Node *rhs);
Node *
new_node_num(int val);
Node *
deref_offset_ptr(Node *ptr, Node *index);

void
gen(Node *node);
void
gen_top(Unit *node);
void
gen_strings(Env *env);
