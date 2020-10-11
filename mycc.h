
// # EBNF
//
//  expr        =  equality
//  equality    =  relational ("==" relational | "!=" relational)*
//  relational  =  add ("<" add | "<=" add | ">" add | ">=" add)*
//  add         =  mul ("+" mul | "-" mul)*
//  mul         =  unary ( "*" unary | "/" unary)*
//  unary       =  ("+" | "-")? primary
//  primary     =  num | "(" expr ")"
//

typedef enum {
    TK_RESERVED,
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
    ND_ADD, // "+"
    ND_SUB, // "-"
    ND_MUL, // "*"
    ND_DIV, // "/"
    ND_NUM, // [0-9]*
    ND_LT,  // "<"
    ND_LE,  // "<="
    ND_EQ,  // "=="
    ND_NE,  // "!="
} NodeKind;

typedef struct Node Node;

struct Node {
    NodeKind kind;
    Node *lhs;
    Node *rhs;
    int val;
};

extern char *user_input;
extern Token *token;

Token *tokenize(char *p);

Node *expr();

void gen(Node *node);
