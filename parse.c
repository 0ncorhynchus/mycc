#include "mycc.h"
#include <string.h>

static void
unexpected(const char *expected, const Token *got) {
    error_at(&got->span, "Unexpected token: '%s' expected, but got '%.*s'",
             expected, got->span.len, got->span.ptr);
}

static bool
consume(const Token **rest, const Token *tok, char *op) {
    if (tok->kind != TK_RESERVED || strlen(op) != tok->span.len ||
        memcmp(tok->span.ptr, op, tok->span.len)) {
        return false;
    }
    *rest = tok->next;
    return true;
}

static void
expect(const Token **rest, const Token *tok, char *op) {
    if (!consume(rest, tok, op)) {
        unexpected(op, tok);
    }
}

static const Token *
consume_ident(const Token **rest, const Token *tok) {
    if (tok->kind != TK_IDENT) {
        return NULL;
    }
    *rest = tok->next;
    return tok;
}

static const Token *
consume_string(const Token **rest, const Token *tok) {
    if (tok->kind != TK_STRING) {
        return NULL;
    }
    *rest = tok->next;
    return tok;
}

static bool
number(const Token **rest, const Token *tok, int *val) {
    if (tok->kind != TK_NUM) {
        return false;
    }

    *val = tok->val;
    *rest = tok->next;

    return true;
}

static bool
at_eof(const Token *tok) {
    return tok->kind == TK_EOF;
}

const Type *
type_specifier(const Token **rest, const Token *tok) {
    if (consume(rest, tok, "int")) {
        return &INT_T;
    }
    if (consume(rest, tok, "char")) {
        return &CHAR_T;
    }
    if (consume(rest, tok, "void")) {
        return &VOID_T;
    }
    return NULL;
}

static int
pointer(const Token **rest, const Token *tok) {
    unsigned int num_ptrs = 0;
    while (consume(&tok, tok, "*")) {
        num_ptrs++;
    }
    *rest = tok;
    return num_ptrs;
}

//
//  type = type "*" | "int" | "char" | "void"
//
const Type *
type(const Token **rest, const Token *tok) {
    const Type *ty = type_specifier(&tok, tok);
    if (ty == NULL) {
        return NULL;
    }

    for (int i = 0; i < pointer(&tok, tok); i++) {
        ty = mk_ptr(ty);
    }

    *rest = tok;
    return ty;
}

//
//  declaration_specifiers = (declaration_specifier)+
//  declaration_specifier =
//      storage_class_specifier
//      type_specifier
//      type_qualifier
//      function_specifier
//      alignment_specifier
//
static const Type *
declspec(const Token **rest, const Token *tok) {
    return type_specifier(rest, tok);
}

static Declaration *
declarator(const Token **rest, const Token *tok, const Type *ty);

//
//  parameter_declaration =
//      declaration_specifiers declarator
//      declaration_specifiers abstract_declarator?
//
static const Declaration *
param_decl(const Token **rest, const Token *tok) {
    const Type *base = declspec(&tok, tok);
    const Declaration *retval = declarator(&tok, tok, base);
    *rest = tok;
    return retval;
}

//
//  parameter_type_list = parameter_list ("," "...")?
//  parameter_list = parameter_declaration ("," parameter_declaration)*
//
static ParamList *
param_list(const Token **rest, const Token *tok) {
    const Declaration *decl = param_decl(&tok, tok);
    if (decl == NULL)
        return NULL;

    ParamList *top = calloc(1, sizeof(ParamList));
    top->decl = decl;
    ParamList *last = top;
    for (;;) {
        if (!consume(&tok, tok, ",")) {
            break;
        }
        decl = param_decl(&tok, tok);
        if (decl) {
            last->next = calloc(1, sizeof(ParamList));
            last->next->decl = decl;
            last = last->next;
        } else if (consume(&tok, tok, "...")) {
            // "..." == ParamList { NULL, NULL }
            last->next = calloc(1, sizeof(ParamList));
            break;
        } else {
            unexpected("<parameter_declaration> or \"...\"", tok);
        }
    }

    *rest = tok;
    return top;
}

const char *
char_from_span(const Span *span) {
    char *c = malloc(span->len + 1);
    memcpy(c, span->ptr, span->len);
    c[span->len] = '\0';
    return c;
}

//
//  direct_declarator =
//      identifier
//      "(" declarator ")"
//      direct_declarator "[" type_qualifier_list? assign? "]"
//      direct_declarator "[" "static" type_qualifier_list? assign "]"
//      direct_declarator "[" type_qualifier_list "static" assign "]"
//      direct_declarator "[" type_qualifier_list? "*" "]"
//      direct_declarator "(" function_args? ")"
//  function_args = ( parameter_type_list | identifier_list )
//
static Declaration *
direct_declarator(const Token **rest, const Token *tok, const Type *ty) {
    const Token *ident = consume_ident(&tok, tok);
    if (ident == NULL) {
        return NULL;
    }

    if (consume(&tok, tok, "(")) {
        ty = mk_func(ty, param_list(&tok, tok));
        expect(&tok, tok, ")");
    } else if (consume(&tok, tok, "[")) {
        int array_size = -1;
        number(&tok, tok, &array_size);
        expect(&tok, tok, "]");
        ty = mk_array(ty, array_size);
    }

    Declaration *decl = calloc(1, sizeof(Declaration));
    decl->ty = ty;
    decl->ident = char_from_span(&ident->span);

    *rest = tok;
    return decl;
}

//
// declarator = pointer? direct_declarator
//
static Declaration *
declarator(const Token **rest, const Token *tok, const Type *ty) {
    for (int i = 0; i < pointer(&tok, tok); i++) {
        ty = mk_ptr(ty);
    }

    return direct_declarator(rest, tok, ty);
}

//
//  typename = ( "int" | "char" | "void" )  "*"* ( "[" num "]" )?
//
static const Type *typename(const Token **rest, const Token *tok) {
    const Type *ty = type_specifier(&tok, tok);
    if (ty == NULL) {
        return NULL;
    }

    // abstract_declarator? = pointer? direct_abstract_declarator?
    for (int i = 0; i < pointer(&tok, tok); i++) {
        ty = mk_ptr(ty);
    }

    // direct_abstract_declarator
    if (consume(&tok, tok, "[")) {
        int size;
        if (!number(&tok, tok, &size)) {
            error("Expect a number.");
        }
        expect(&tok, tok, "]");

        ty = mk_array(ty, size);
    }

    *rest = tok;
    return ty;
}

// Implicit converter from array T[] to pointer T*
Node *
as_ptr(Node *array) {
    if (array == NULL || array->ty == NULL || array->ty->ty != ARRAY)
        return array;

    Node *ptr = calloc(1, sizeof(Node));
    ptr->kind = ND_ADDR;
    ptr->lhs = array;
    ptr->ty = mk_ptr(array->ty->ptr_to);

    return ptr;
}

Node *
new_node(NodeKind kind, Node *lhs, Node *rhs) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = kind;
    node->ty = get_type(lhs, rhs);
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

Node *
new_node_num(int val) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_NUM;
    node->ty = &INT_T;
    node->val = val;
    return node;
}

static Node *
expr(const Token **rest, const Token *tok, Env *env);

//
//  primary = num
//          | ident ( "(" ( expr ( "," expr )* )? ")" )?
//          | "(" expr ")"
//          | string
//
static Node *
primary(const Token **rest, const Token *tok, Env *env) {
    if (consume(&tok, tok, "(")) {
        Node *node = expr(&tok, tok, env);
        expect(&tok, tok, ")");
        *rest = tok;
        return node;
    }

    const Token *tmp = consume_ident(&tok, tok);
    if (tmp) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_LVAR;
        node->ident = tmp->span;

        // function call
        if (consume(&tok, tok, "(")) {
            node->kind = ND_CALL;
            if (consume(&tok, tok, ")")) {
                *rest = tok;
                return node;
            }

            node->lhs = calloc(1, sizeof(Node));
            Node *current = node->lhs;
            current->kind = ND_ARGS;
            current->lhs = expr(&tok, tok, env);
            while (!consume(&tok, tok, ")")) {
                expect(&tok, tok, ",");
                current->rhs = calloc(1, sizeof(Node));
                current = current->rhs;
                current->kind = ND_ARGS;
                current->lhs = expr(&tok, tok, env);
            }

            *rest = tok;
            return node;
        }

        const Var *var = get_var(env, &tmp->span);
        node->ty = var->ty;
        node->offset = var->offset;
        node->vkind = var->kind;

        *rest = tok;
        return node;
    }

    tmp = consume_string(&tok, tok);
    if (tmp) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_STRING;
        node->ident = tmp->span;
        node->ident.ptr++;
        node->ident.len -= 2;
        node->ty = mk_ptr(&CHAR_T);

        const String *string = push_string(env, &node->ident);
        node->val = string->index;

        *rest = tok;
        return node;
    }

    int val;
    if (number(&tok, tok, &val)) {
        *rest = tok;
        return new_node_num(val);
    }

    return NULL;
}

Node *
deref_offset_ptr(Node *ptr, Node *index) {
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
static Node *
desugar_index(const Token **rest, const Token *tok, Env *env) {
    Node *node = primary(&tok, tok, env);
    if (consume(&tok, tok, "[")) {
        Node *index = expr(&tok, tok, env);
        expect(&tok, tok, "]");
        *rest = tok;
        return deref_offset_ptr(as_ptr(node), index);
    }
    *rest = tok;
    return node;
}

//
//  unary = ( "+" | "-" )? primary ( "[" expr "]" )?
//        | ( "*" | "&" | "sizeof" ) unary
//        | "sizeof" "(" typename ")"
//
static Node *
unary(const Token **rest, const Token *tok, Env *env) {
    if (consume(&tok, tok, "+")) {
        return desugar_index(rest, tok, env);
    }
    if (consume(&tok, tok, "-")) {
        return new_node(ND_SUB, new_node_num(0), desugar_index(rest, tok, env));
    }
    if (consume(&tok, tok, "*")) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_DEREF;
        Node *inner = as_ptr(unary(&tok, tok, env));
        if (inner->ty && inner->ty->ty == PTR) {
            node->lhs = inner;
            node->ty = inner->ty->ptr_to;
            *rest = tok;
            return node;
        }
        error("Cannot deref: '%s'", type_to_str(inner->ty));
    }
    if (consume(&tok, tok, "&")) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_ADDR;
        node->lhs = unary(&tok, tok, env);
        if (node->lhs && node->lhs->ty) {
            node->ty = mk_ptr(node->lhs->ty);
            *rest = tok;
            return node;
        }
        error("Internal compile error: try to obtain the address to an unknown "
              "type");
    }
    if (consume(&tok, tok, "sizeof")) {
        if (consume(&tok, tok, "(")) {
            const Type *ty = typename(&tok, tok);
            if (ty == NULL) {
                Node *node = expr(&tok, tok, env);
                if (node) {
                    ty = node->ty;
                }
            }
            expect(&tok, tok, ")");

            if (ty) {
                *rest = tok;
                return new_node_num(sizeof_ty(ty));
            }
        } else {
            Node *node = unary(&tok, tok, env);
            if (node->ty) {
                *rest = tok;
                return new_node_num(sizeof_ty(node->ty));
            }
        }
        error("Internal compile error: try to obtain the size of an unknown "
              "type");
    }

    return desugar_index(rest, tok, env);
}

//
//  mul = unary ( "*" unary | "/" unary )*
//
static Node *
mul(const Token **rest, const Token *tok, Env *env) {
    Node *node = unary(&tok, tok, env);

    for (;;) {
        if (consume(&tok, tok, "*")) {
            node = new_node(ND_MUL, node, unary(&tok, tok, env));
        } else if (consume(&tok, tok, "/")) {
            node = new_node(ND_DIV, node, unary(&tok, tok, env));
        } else {
            *rest = tok;
            return node;
        }
    }
}

//
//  add = mul ( "+" mul | "-" mul )*
//
static Node *
add(const Token **rest, const Token *tok, Env *env) {
    Node *node = mul(&tok, tok, env);

    for (;;) {
        if (consume(&tok, tok, "+")) {
            node = new_node(ND_ADD, as_ptr(node), as_ptr(mul(&tok, tok, env)));
        } else if (consume(&tok, tok, "-")) {
            node = new_node(ND_SUB, as_ptr(node), as_ptr(mul(&tok, tok, env)));
        } else {
            *rest = tok;
            return node;
        }
    }
}

//
//  relational= add ( "<" add | "<=" add | ">" add | ">=" add )*
//
static Node *
relational(const Token **rest, const Token *tok, Env *env) {
    Node *node = add(&tok, tok, env);

    for (;;) {
        if (consume(&tok, tok, "<")) {
            Node *rhs = add(&tok, tok, env);
            if (!is_same_type(node->ty, rhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LT, node, rhs);
        } else if (consume(&tok, tok, "<=")) {
            Node *rhs = add(&tok, tok, env);
            if (!is_same_type(node->ty, rhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LE, node, rhs);
        } else if (consume(&tok, tok, ">")) {
            Node *lhs = add(&tok, tok, env);
            if (!is_same_type(node->ty, lhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LT, lhs, node);
        } else if (consume(&tok, tok, ">=")) {
            Node *lhs = add(&tok, tok, env);
            if (!is_same_type(node->ty, lhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LE, lhs, node);
        } else {
            *rest = tok;
            return node;
        }

        node->ty = &INT_T;
    }
}

//
//  equality = relational ( "==" relational | "!=" relational )*
//
static Node *
equality(const Token **rest, const Token *tok, Env *env) {
    Node *node = relational(&tok, tok, env);

    for (;;) {
        if (consume(&tok, tok, "==")) {
            node = new_node(ND_EQ, node, relational(&tok, tok, env));
        } else if (consume(&tok, tok, "!=")) {
            node = new_node(ND_NE, node, relational(&tok, tok, env));
        } else {
            *rest = tok;
            return node;
        }
    }
}

//
//  assign = equality ( "=" assign )?
//
static Node *
assign(const Token **rest, const Token *tok, Env *env) {
    Node *node = equality(&tok, tok, env);
    if (consume(&tok, tok, "=")) {
        const Type *ty = node->ty;
        node = new_node(ND_ASSIGN, node, as_ptr(assign(&tok, tok, env)));
        node->ty = ty;
    }
    *rest = tok;
    return node;
}

//
//  expr = assign
//
static Node *
expr(const Token **rest, const Token *tok, Env *env) {
    return assign(rest, tok, env);
}

static Node *
stmt(const Token **rest, const Token *tok, Env *env);

// try to parse a type and an ident.
// For parse a function and a declare.
static Node *
type_ident(const Token **rest, const Token *tok) {
    const Type *ty = type(&tok, tok);
    if (ty == NULL) {
        return NULL;
    }

    Node *node = calloc(1, sizeof(Node));
    node->ty = ty;
    const Token *ident = consume_ident(&tok, tok);

    if (ident == NULL) {
        unexpected("an ident", tok);
    }

    node->ident = ident->span;

    *rest = tok;
    return node;
}

static Node *
block(const Token **rest, const Token *tok, Env *env) {
    if (!consume(&tok, tok, "{")) {
        return NULL;
    }

    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_BLOCK;

    NodeList *body = NULL;
    while (!consume(&tok, tok, "}")) {
        NodeList *next = calloc(1, sizeof(NodeList));
        next->node = stmt(&tok, tok, env);
        if (body) {
            body->next = next;
            body = body->next;
        } else {
            node->inner = next;
            body = node->inner;
        }
    }

    *rest = tok;

    return node;
}

//
//  function_definition = declaration_specifiers declarator declartion_list?
//      compound_statement
//
static Function *
function(const Token **rest, const Token *tok, Env *parent) {
    const Type *ty = declspec(&tok, tok);
    if (ty == NULL) {
        return NULL;
    }

    const Declaration *decl = declarator(&tok, tok, ty);
    if (decl == NULL || decl->ty->ty != FUNCTION) {
        return NULL;
    }

    Function *fn = calloc(1, sizeof(Function));
    fn->ident = decl->ident;
    fn->args = decl->ty->args;

    Env env = make_scope(parent);
    const ParamList *arg = fn->args;
    while (arg) {
        const Declaration *decl = arg->decl;
        const Span span = {decl->ident, strlen(decl->ident)};
        declare_var(&env, decl->ty, &span);
        fn->num_args++;
        arg = arg->next;
    }
    if (fn->num_args > 6) {
        error("Not supported: more than 6 arguments.");
    }

    int argument_offset = env.maximum_offset;

    fn->body = block(&tok, tok, &env);
    fn->lvar_offset = env.maximum_offset - argument_offset;

    *rest = tok;

    return fn;
}

//
//  init = expr | "{" init ( "," init )* "}"
//
static Node *
init(const Token **rest, const Token *tok, Env *env) {
    if (consume(&tok, tok, "{")) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_INIT;
        node->next = init(&tok, tok, env);
        node->num_initializers = 1;
        Node *n = node->next;
        while (consume(&tok, tok, ",")) {
            n->next = init(&tok, tok, env);
            n = n->next;
            node->num_initializers++;
        }
        expect(&tok, tok, "}");
        *rest = tok;
        return node;
    }

    *rest = tok;
    return expr(rest, tok, env);
}

typedef struct {
    Node *expr;
} Initializer;

//
//  initializer =
//      assignment_expression
//      "{" initializer_list ","? "}"
//  initializer_list = designation? initializer ( "," designation? initializer)*
//  designation = designator_list "="
//  designator_list = designator+
//  designator =
//      "[" constant_expression "]"
//      "." identifier
//
static const Initializer *
initializer(const Token **rest, const Token *tok, Env *env) {
    Initializer *init;
    Node *e = expr(&tok, tok, env);
    if (e) {
        init = calloc(1, sizeof(Initializer));
        init->expr = e;
        *rest = tok;
        return init;
    }
    return NULL;
}

//
//  declaration =
//      declaration_specifiers init_declarator_list? ";"
//      static_assert_declaration
//  init_declarator_list = init_declarator ( "," init_declarator )*
//  init_declarator = declarator ( "=" initializer )?
//
static const Declaration *
declaration(const Token **rest, const Token *tok, Env *env) {
    const Type *ty = declspec(&tok, tok);
    if (ty == NULL) {
        return NULL;
    }
    Declaration *decl = declarator(&tok, tok, ty);
    if (decl == NULL || decl->ty->ty == FUNCTION) {
        return NULL;
    }
    if (consume(&tok, tok, "=")) {
        const Initializer *init = initializer(&tok, tok, env);
        if (init == NULL) {
            return NULL;
        }
        if (decl->ty->ty == ARRAY && decl->ty->array_size == -1) {
            int array_size = -1;
            switch (init->expr->kind) {
            case (ND_INIT):
                array_size = init->expr->num_initializers;
                break;
            case (ND_STRING):
                array_size = init->expr->ident.len + 1;
                break;
            default:
                error("array must be initialized with a brace-enclosed "
                      "initializer");
            }
            // XXX: Replace array type with newly allocated one
            // due to const Type*.
            decl->ty = mk_array(decl->ty->ptr_to, array_size);
        }
        decl->init = init->expr;
    }
    expect(&tok, tok, ";");

    const Span span = {decl->ident, strlen(decl->ident)};
    declare_var(env, decl->ty, &span);

    *rest = tok;
    return decl;
}

//
//  declare = type ident ( "[" num "]" )? ( "=" init )? ";"
//
static Node *
declare(const Token **rest, const Token *tok, Env *env, Node *node) {
    node->kind = ND_DECLARE;

    bool is_array = false;
    bool is_known_size = false;
    int array_size = 0;

    if (consume(&tok, tok, "[")) {
        is_array = true;
        is_known_size = number(&tok, tok, &array_size);
        expect(&tok, tok, "]");
    }

    if (consume(&tok, tok, "=")) {
        node->init = as_ptr(init(&tok, tok, env));
        if (is_array && !is_known_size) {
            switch (node->init->kind) {
            case (ND_INIT):
                array_size = node->init->num_initializers;
                break;
            case (ND_STRING):
                array_size = node->init->ident.len + 1;
                break;
            default:
                error("array must be initialized with a brace-enclosed "
                      "initializer");
            }
        }
    }

    if (is_array) {
        node->ty = mk_array(node->ty, array_size);
    }

    const Var *var = declare_var(env, node->ty, &node->ident);
    node->vkind = var->kind;
    node->offset = var->offset;

    expect(&tok, tok, ";");
    *rest = tok;

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
static Node *
stmt(const Token **rest, const Token *tok, Env *env) {
    Node *node = NULL;

    if (consume(&tok, tok, "if")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_IF_COND;
        expect(&tok, tok, "(");
        node->lhs = expr(&tok, tok, env);
        expect(&tok, tok, ")");

        node->rhs = calloc(1, sizeof(Node));
        node->rhs->kind = ND_IF_BODY;
        node->rhs->lhs = stmt(&tok, tok, env);
        if (consume(&tok, tok, "else")) {
            node->rhs->rhs = stmt(&tok, tok, env);
        }
    } else if (consume(&tok, tok, "while")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_WHILE;
        expect(&tok, tok, "(");
        node->lhs = expr(&tok, tok, env);
        expect(&tok, tok, ")");
        node->rhs = stmt(&tok, tok, env);
    } else if (consume(&tok, tok, "for")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_FOR_INIT;
        expect(&tok, tok, "(");
        if (!consume(&tok, tok, ";")) {
            node->lhs = expr(&tok, tok, env);
            expect(&tok, tok, ";");
        }

        node->rhs = calloc(1, sizeof(Node));
        node->rhs->kind = ND_FOR_COND;
        if (!consume(&tok, tok, ";")) {
            node->rhs->lhs = expr(&tok, tok, env);
            expect(&tok, tok, ";");
        }

        node->rhs->rhs = calloc(1, sizeof(Node));
        node->rhs->rhs->kind = ND_FOR_BODY;
        if (!consume(&tok, tok, ")")) {
            node->rhs->rhs->lhs = expr(&tok, tok, env);
            expect(&tok, tok, ")");
        }
        node->rhs->rhs->rhs = stmt(&tok, tok, env);
    } else if (consume(&tok, tok, "return")) {
        node = calloc(1, sizeof(Node));
        node->kind = ND_RETURN;
        node->lhs = as_ptr(expr(&tok, tok, env));
        expect(&tok, tok, ";");
    } else if ((node = block(&tok, tok, env))) {
        ;
    } else {
        node = type_ident(&tok, tok);
        if (node) {
            node = declare(&tok, tok, env, node);
        } else {
            node = calloc(1, sizeof(Node));
            node->kind = ND_SEMICOLON;
            node->lhs = expr(&tok, tok, env);
            expect(&tok, tok, ";");
        }
    }

    *rest = tok;

    return node;
}

//
//  program = ( function | declare )*
//
void
program(const Token *token, Env *env, Unit *code[]) {
    int i = 0;
    while (!at_eof(token)) {
        code[i] = calloc(1, sizeof(Unit));

        const Function *fn = function(&token, token, env);
        if (fn) {
            code[i++]->function = fn;
            continue;
        }

        const Declaration *decl = declaration(&token, token, env);
        if (decl) {
            code[i++]->declaration = decl;
            continue;
        }

        error_at(&token->span, "Cannot parse the program.");
    }
    code[i] = NULL;
}
