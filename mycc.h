#pragma once
#include <stdbool.h>
#include <stdlib.h>

typedef struct Span Span;
struct Span {
    const char *file;
    int line;
    int offset;
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
    bool is_body_defined;
    bool is_const;
    int enum_val;
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

typedef struct String String;

typedef struct {
    const char *tag;
    String *consts;
} Enum;

typedef struct Members Members;
struct Members {
    Members *next;
    const Var *member;
};

typedef struct {
    const char *tag;
    const Members *members;
    size_t size;
} Struct;

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
    enum { INTEGER, PTR, ARRAY, VOID, FUNCTION, ENUM, STRUCT } ty;

    enum { CHAR, INT } ikind;

    const Type *ptr_to;
    int array_size;

    // For FUNCTION
    const Type *retty;
    const ParamList *args;

    // For ENUM
    Enum *enum_ty;
    // For STRUCT
    Struct *struct_ty;
};

extern const Type INT_T;
extern const Type CHAR_T;
extern const Type VOID_T;

typedef struct {
    enum { CASE, DEFAULT } kind;
    int jump_index;
    int val;
} Label;

typedef struct LabelList LabelList;
struct LabelList {
    LabelList *next;
    const Label *label;
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
    ND_LVAR,   // [a-zA-Z_][a-zA-Z0-9_]*
    ND_CALL,   // <function call>
    ND_ADDR,   // "&"
    ND_DEREF,  // "*"
    ND_STRING,
} NodeKind;

typedef struct NodeList NodeList;

struct Node {
    NodeKind kind;
    const Type *ty;

    // For ND_NUM, ND_STRING
    int val;

    // For operators: ND_ADD, ND_SUB, ...
    Node *lhs;
    Node *rhs;

    // For ND_LVAR
    const Var *var;

    // For ND_CALL
    const char *fn;
    NodeList *args; // reversed for simplifying pushing args to the stack

    // For ND_STRING
    const char *str;
};

struct NodeList {
    NodeList *next;
    Node *node;
};

typedef struct Statement Statement;

typedef struct BlockItems BlockItems;
struct BlockItems {
    BlockItems *next;
    const Declaration *declaration;
    const Statement *statement;
};

typedef enum {
    ST_LABEL,
    ST_COMPOUND,
    ST_EXPRESSION,

    // Selection
    ST_IF,
    ST_SWITCH,

    // Iteration
    ST_WHILE,
    ST_FOR,

    // Jump
    ST_CONTINUE,
    ST_BREAK,
    ST_RETURN,
} StatementKind;

struct Statement {
    StatementKind kind;

    // ST_LABEL
    Label label;

    // ST_LABEL, ST_IF, ST_SWITCH, ST_WHILE, ST_CONTINUE, ST_BREAK
    int jump_index;

    // ST_LABEL, ST_SWITCH, ST_WHILE, ST_FOR
    const Statement *body;

    // ST_COMPOUND
    BlockItems *block;

    // ST_EXPRESSION
    Node *expression;

    // ST_IF, ST_WHILE, ST_FOR
    Node *cond;

    // ST_IF
    const Statement *then_body;
    const Statement *else_body;

    // ST_SWITCH
    Node *value;
    LabelList *labels;

    // ST_FOR
    const Declaration *declaration;
    Node *init;
    Node *end;

    // ST_RETURN
    Node *retval;
};

// Linked list for variables
typedef struct VarList VarList;
struct VarList {
    VarList *next;
    Var *var;
    bool is_typedef;
};

struct String {
    String *next;
    int index;
    const char *ident;
};

typedef struct TagList TagList;
struct TagList {
    TagList *next;
    const char *tag;
    Type *ty;
};

typedef struct Env Env;
struct Env {
    Env *parent;
    VarList *vars;
    String *strings;
    TagList *tags;
    LabelList *labels;
    int maximum_offset;
    int maximum_strings;
    unsigned int num_args;
    unsigned int maximum_arg_offset;
    bool is_block_scope;
    bool is_switch_scope;
    int jump_index;
};

Env
init_env();
Env
make_scope(Env *parent);
Env
make_block_scope(Env *parent);
Env
make_jump_scope(Env *parent);
Env
make_switch_scope(Env *parent);

typedef struct Function Function;
struct Function {
    const Var *def; // For ident and type and args
    unsigned int num_args;
    int lvar_offset;
    const Statement *body;
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
const char *
type_to_str(const Type *ty);
bool
is_same_type(const Type *lhs, const Type *rhs);
const Type *
get_type(const Node *lhs, const Node *rhs);

size_t
expand_for_align(size_t t);

const Var *
get_var(Env *env, const char *ident);
bool
declare_arg(Env *env, Var *var);
bool
declare_var(Env *env, Var *var);
void
declare_fn(Env *env, Var *var);

void
declare_typedef(Env *env, const Var *var);
const Type *
get_typedef(const Env *env, const char *ident);

void
declare_tag(Env *env, const Type *ty);
const Type *
get_tag(const Env *env, const char *tag);

const String *
push_string(Env *env, const char *ident);

void
push_label(Env *env, const Label *label);

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
