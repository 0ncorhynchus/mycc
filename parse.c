#include "mycc.h"
#include <string.h>

//
// Declarations
//

static const Type *
type_specifier(const Token **rest, const Token *tok, const Env *env);

static Declaration *
declarator(const Token **rest, const Token *tok, const Env *env,
           const Type *ty);

static const Type *
type_name(const Token **rest, const Token *tok, const Env *env);

static Node *
expr(const Token **rest, const Token *tok, Env *env);

static Node *
assign(const Token **rest, const Token *tok, Env *env);

static Node *
unary(const Token **rest, const Token *tok, Env *env);

static const Statement *
stmt(const Token **rest, const Token *tok, Env *env);

static const Declaration *
declaration(const Token **rest, const Token *tok, Env *env);

//
// Function definitions
//

static void
unexpected(const char *expected, const Token *got) {
    error_at(&got->span, "Unexpected token: '%s' expected, but got '%.*s'",
             expected, got->span.len, got->span.ptr);
}

static void
not_implemented(const Span *span, const char *name) {
    if (name) {
        error_at(span, "%s is not implemented yet.", name);
    } else {
        error_at(span, "Not implemented yet.");
    }
}

static bool
consume(const Token **rest, const Token *tok, const char *op) {
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

const char *
char_from_span(const Span *span) {
    char *c = malloc(span->len + 1);
    memcpy(c, span->ptr, span->len);
    c[span->len] = '\0';
    return c;
}

//
//  enum_specifier =
//      "enum" identifier? "{" enumerator_list ","? "}"
//      "enum" identifier
//
//  enumerator_list = enumerator ( "," enumerator )*
//  enumerator =
//      enumeration_constant
//      enumeration_constant = constant_expression
//
static const Type *
enum_specifier(const Token **rest, const Token *tok) {
    if (!consume(&tok, tok, "enum")) {
        return NULL;
    }

    Enum *e = calloc(1, sizeof(Enum));
    const Token *tag = consume_ident(&tok, tok);
    if (tag) {
        e->tag = char_from_span(&tag->span);
    }

    if (consume(&tok, tok, "{")) {
        int val = 0;
        const Token *constant = consume_ident(&tok, tok);
        if (constant == NULL) {
            error_at(&tok->span, "Failed to parse enum");
        }
        e->consts = calloc(1, sizeof(String));
        e->consts->index = val;
        e->consts->ident = char_from_span(&constant->span);

        String *last = e->consts;
        while (!consume(&tok, tok, "}")) {
            expect(&tok, tok, ",");

            constant = consume_ident(&tok, tok);
            if (constant == NULL) {
                expect(&tok, tok, "}");
                break;
            }

            val++;
            last->next = calloc(1, sizeof(String));
            last->next->index = val;
            last->next->ident = char_from_span(&constant->span);
            last = last->next;
        }
    } else if (tag == NULL) {
        error("enum requires an identifier or a block at least");
    }

    Type *ty = calloc(1, sizeof(Type));
    ty->ty = ENUM;
    ty->enum_ty = e;

    *rest = tok;
    return ty;
}

//
//  specifier_qualifier_list = ( type_specifier | type_qualifier )+
//
static const Type *
spec_qual_list(const Token **rest, const Token *tok, const Env *env) {
    return type_specifier(rest, tok, env);
}

//
//  struct_or_union_specifier =
//      ( "struct" | "union" ) identifier? "{" struct_declaration+ "}"
//      ( "struct" | "union" ) identifier
//  struct_declaration =
//      specifier_qualifier_list struct_declarator_list? ";"
//      static_assert_declaration
//  struct_declarator_list =
//      struct_declarator ( "," struct_declarator )*
//  struct_declarator =
//      declarator
//      declarator? ":" constant_expression
//
const Type *
struct_union_spec(const Token **rest, const Token *tok, const Env *env) {
    if (!consume(&tok, tok, "struct")) {
        return NULL;
    }

    const Token *tag = consume_ident(&tok, tok);
    size_t size = 0;
    Members *members = NULL;
    if (consume(&tok, tok, "{")) {
        const Type *ty = spec_qual_list(&tok, tok, env);
        const Declaration *decl = declarator(&tok, tok, env, ty);
        decl->var->offset = size;
        members = calloc(1, sizeof(Members));
        members->member = decl->var;
        size += expand_for_align(sizeof_ty(decl->var->ty));
        expect(&tok, tok, ";");

        Members *last = members;
        while (!consume(&tok, tok, "}")) {
            const Type *ty = spec_qual_list(&tok, tok, env);
            const Declaration *decl = declarator(&tok, tok, env, ty);
            decl->var->offset = size;
            last->next = calloc(1, sizeof(Members));
            last = last->next;
            last->member = decl->var;
            size += expand_for_align(sizeof_ty(decl->var->ty));
            expect(&tok, tok, ";");
        }
    } else if (tag == NULL) {
        error("struct requires an identifier or a block at least");
    }

    Struct *st = calloc(1, sizeof(Struct));
    if (tag) {
        st->tag = char_from_span(&tag->span);
    }
    st->size = size;
    st->members = members;

    Type *ty = calloc(1, sizeof(Type));
    ty->ty = STRUCT;
    ty->struct_ty = st;

    *rest = tok;
    return ty;
}

//
// atomic_type_specifier = "_Atomic" "(" type_name ")"
//
static const Type *
atomic_type_spec(const Token **rest, const Token *tok, const Env *env) {
    if (consume(rest, tok, "_Atomic")) {
        not_implemented(&tok->span, "_Atomic");
        /* expect(&tok, tok, "("); */
        /* const Type *ty = type_name(&tok, tok, env); */
        /* expect(&tok, tok, ")"); */
    }

    return NULL;
}

//
//  type_specifier = "void" | "char" | "short" | "int" | "long" | float" |
//      "double" | "signed" | "unsigned" | "_Bool" | "_Complex" |
//      atomic_type_specifier |
//      struct_or_union_specifier |
//      enum_specifier |
//      typedef_name
//  typedef_name = identifier
//
static const Type *
type_specifier(const Token **rest, const Token *tok, const Env *env) {
    if (consume(rest, tok, "void")) {
        return &VOID_T;
    }
    if (consume(rest, tok, "char")) {
        return &CHAR_T;
    }
    if (consume(rest, tok, "short")) {
        not_implemented(&tok->span, "short");
    }
    if (consume(rest, tok, "int")) {
        return &INT_T;
    }
    if (consume(rest, tok, "long")) {
        not_implemented(&tok->span, "long");
    }
    if (consume(rest, tok, "float")) {
        not_implemented(&tok->span, "float");
    }
    if (consume(rest, tok, "double")) {
        not_implemented(&tok->span, "double");
    }
    if (consume(rest, tok, "signed")) {
        not_implemented(&tok->span, "signed");
    }
    if (consume(rest, tok, "unsigned")) {
        not_implemented(&tok->span, "unsigned");
    }
    if (consume(rest, tok, "_Bool")) {
        not_implemented(&tok->span, "_Bool");
    }
    if (consume(rest, tok, "_Complex")) {
        not_implemented(&tok->span, "_Complex");
    }

    const Type *ty;
    ty = atomic_type_spec(rest, tok, env);
    if (ty) {
        return ty;
    }
    ty = struct_union_spec(rest, tok, env);
    if (ty) {
        if (ty->struct_ty->members == NULL) {
            const Type *orig = get_tag(env, ty->struct_ty->tag);
            if (orig) {
                return orig;
            }
        }
        return ty;
    }

    ty = enum_specifier(rest, tok);
    if (ty) {
        if (ty->enum_ty->consts == NULL) {
            const Type *orig = get_tag(env, ty->enum_ty->tag);
            if (orig) {
                return orig;
            }
        }
        return ty;
    }

    const Token *typedef_name = consume_ident(&tok, tok);
    if (typedef_name) {
        const Type *ty = get_typedef(env, char_from_span(&typedef_name->span));
        if (ty) {
            *rest = tok;
            return ty;
        }
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
//  storage_class_specifier
//
// At most, one storage-class specifier is allowed expect that
// `_Thread_local` may appear with `static` or `extern`.
//
typedef enum {
    EMPTY = 0,        // 0b00000
    THREAD_LOCAL = 1, // 0b00001
    EXTERN = 2,       // 0b00010
    STATIC = 6,       // 0b00110
    TYPEDEF = 11,     // 0b01011
    AUTO = 19,        // 0b10011
    REGISTER = 27,    // 0b11011
} StorageClass;

static StorageClass
storage_spec(const Token **rest, const Token *tok) {
    if (consume(rest, tok, "typedef")) {
        return TYPEDEF;
    }
    if (consume(rest, tok, "extern")) {
        return EXTERN;
    }
    if (consume(rest, tok, "static")) {
        return STATIC;
    }
    if (consume(rest, tok, "_Thread_local")) {
        return THREAD_LOCAL;
    }
    if (consume(rest, tok, "auto")) {
        return AUTO;
    }
    if (consume(rest, tok, "register")) {
        return REGISTER;
    }
    return EMPTY;
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
typedef struct {
    StorageClass storage;
    const Type *ty;
} DeclSpec;

static const DeclSpec
declspec(const Token **rest, const Token *tok, const Env *env) {
    DeclSpec spec = {};
    bool is_consumed = false;

    for (;;) {
        const StorageClass s = storage_spec(&tok, tok);
        if (s != EMPTY) {
            if ((spec.storage & s) != 0) {
                error("storage-class error");
            }
            spec.storage |= s;
            continue;
        }

        const Type *ty = type_specifier(&tok, tok, env);
        if (ty) {
            spec.ty = ty;
            is_consumed = true;
            continue;
        }

        break;
    }

    if (is_consumed) {
        *rest = tok;
    }
    return spec;
}

//
//  parameter_declaration =
//      declaration_specifiers declarator
//      declaration_specifiers abstract_declarator?
//
static Declaration *
param_decl(const Token **rest, const Token *tok, const Env *env) {
    const Type *base = declspec(&tok, tok, env).ty;
    Declaration *retval = declarator(&tok, tok, env, base);
    *rest = tok;
    return retval;
}

//
//  parameter_type_list = parameter_list ("," "...")?
//  parameter_list = parameter_declaration ("," parameter_declaration)*
//
static ParamList *
param_list(const Token **rest, const Token *tok, const Env *env) {
    Declaration *decl = param_decl(&tok, tok, env);
    if (decl == NULL)
        return NULL;

    ParamList *top = calloc(1, sizeof(ParamList));
    top->decl = decl;
    ParamList *last = top;
    for (;;) {
        if (!consume(&tok, tok, ",")) {
            break;
        }
        decl = param_decl(&tok, tok, env);
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
direct_declarator(const Token **rest, const Token *tok, const Env *env,
                  const Type *ty) {
    const Token *ident = consume_ident(&tok, tok);
    if (ident == NULL) {
        return NULL;
    }

    if (consume(&tok, tok, "(")) {
        ty = mk_func(ty, param_list(&tok, tok, env));
        expect(&tok, tok, ")");
    } else if (consume(&tok, tok, "[")) {
        int array_size = -1;
        number(&tok, tok, &array_size);
        expect(&tok, tok, "]");
        ty = mk_array(ty, array_size);
    }

    Declaration *decl = calloc(1, sizeof(Declaration));
    decl->var = calloc(1, sizeof(Var));
    decl->var->ty = ty;
    decl->var->ident = char_from_span(&ident->span);

    *rest = tok;
    return decl;
}

//
// declarator = pointer? direct_declarator
//
static Declaration *
declarator(const Token **rest, const Token *tok, const Env *env,
           const Type *ty) {
    for (int i = 0; i < pointer(&tok, tok); i++) {
        ty = mk_ptr(ty);
    }

    return direct_declarator(rest, tok, env, ty);
}

//
//  type_name = ( "int" | "char" | "void" )  "*"* ( "[" num "]" )?
//
static const Type *
type_name(const Token **rest, const Token *tok, const Env *env) {
    const Type *ty = type_specifier(&tok, tok, env);
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

Node *
refer(Node *inner, const Type *ty) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_ADDR;
    node->lhs = inner;
    node->ty = mk_ptr(ty);
    return node;
}

// Implicit converter from array T[] to pointer T*
Node *
as_ptr(Node *array) {
    if (array == NULL || array->ty == NULL || array->ty->ty != ARRAY)
        return array;

    return refer(array, array->ty->ptr_to);
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

//
//  primary_expression =
//      identifier
//      constant
//      string_literal
//      "(" expression ")"
//      generic_selection
//
static Node *
primary(const Token **rest, const Token *tok, Env *env) {
    const Token *tmp = consume_ident(&tok, tok);
    if (tmp) {
        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_LVAR;
        node->var = get_var(env, char_from_span(&tmp->span));
        node->ty = node->var->ty;
        *rest = tok;
        return node;
    }

    int val;
    if (number(rest, tok, &val)) {
        return new_node_num(val);
    }

    tmp = consume_string(&tok, tok);
    if (tmp) {
        Span span = tmp->span;
        span.ptr++;
        span.len -= 2;

        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_STRING;
        node->str = char_from_span(&span);
        node->ty = mk_ptr(&CHAR_T);

        const String *string = push_string(env, node->str);
        node->val = string->index;

        *rest = tok;
        return node;
    }

    if (consume(&tok, tok, "(")) {
        Node *node = expr(&tok, tok, env);
        expect(&tok, tok, ")");
        *rest = tok;
        return node;
    }

    return NULL;
}

static Node *
deref(Node *ptr) {
    if (ptr->ty->ty != PTR) {
        error("Cannot deref: '%s'", type_to_str(ptr->ty));
    }
    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_DEREF;
    node->lhs = ptr;
    node->ty = ptr->ty->ptr_to;
    return node;
}

Node *
deref_offset_ptr(Node *ptr, Node *index) {
    return deref(new_node(ND_ADD, ptr, index));
}

//
//  argument_expression_list? = ( assign ( "," assign )* )?
//
static NodeList *
argexprlist(const Token **rest, const Token *tok, Env *env) {
    Node *node = assign(&tok, tok, env);
    if (!node) {
        return NULL;
    }

    NodeList *list = calloc(1, sizeof(NodeList));
    list->node = node;

    while (consume(&tok, tok, ",")) {
        node = assign(&tok, tok, env);
        if (node == NULL) {
            error("Invalid argument");
        }
        NodeList *new = calloc(1, sizeof(NodeList));
        new->next = list;
        new->node = node;
        list = new;
    }

    *rest = tok;
    return list;
}

static Node *
member(Node *var, const Token *ident_token) {
    if (var->ty->ty != STRUCT) {
        error("Not struct");
    }

    const char *ident = char_from_span(&ident_token->span);
    const Members *members = var->ty->struct_ty->members;

    const Var *m = NULL;
    while (members) {
        if (strcmp(members->member->ident, ident) == 0) {
            m = members->member;
            break;
        }
        members = members->next;
    }
    if (m == NULL) {
        error_at(&ident_token->span, "Invalid member");
    }

    if (var->kind == ND_LVAR) {
        Var *mvar = calloc(1, sizeof(Var));
        mvar->ident = m->ident;
        mvar->ty = m->ty;
        mvar->offset = var->var->offset - m->offset;

        Node *node = calloc(1, sizeof(Node));
        node->kind = ND_LVAR;
        node->var = mvar;
        node->ty = mvar->ty;
        return node;
    }

    return deref_offset_ptr(refer(var, m->ty), new_node_num(-m->offset));
}

//
//  postfix_expression = postfix_head postfix_tail*
//  postfix_head =
//      primary_expression
//      "(" type_name ")" "{" initializer_list "}"
//      "(" type_name ")" "{" initializer_list "," "}"
//  postfix_tail =
//      "[" expression "]"
//      "(" argument_expression_list? ")"
//      "." identifier
//      "->" identifier
//      "++"
//      "--"
//
static Node *
postfix(const Token **rest, const Token *tok, Env *env) {
    Node *node = primary(&tok, tok, env);
    for (;;) {
        if (consume(&tok, tok, "[")) {
            Node *index = expr(&tok, tok, env);
            expect(&tok, tok, "]");
            *rest = tok;
            node = deref_offset_ptr(as_ptr(node), index);
            continue;
        }
        if (consume(&tok, tok, "(")) {
            if (node->kind == ND_LVAR) {
                node->fn = node->var->ident;
            }
            node->kind = ND_CALL;
            node->args = argexprlist(&tok, tok, env);
            if (consume(&tok, tok, ")")) {
                continue;
            }
            continue;
        }
        if (consume(&tok, tok, ".")) {
            const Token *ident_token = consume_ident(&tok, tok);
            node = member(node, ident_token);
            continue;
        }
        if (consume(&tok, tok, "->")) {
            const Token *ident_token = consume_ident(&tok, tok);
            node = member(deref(node), ident_token);
            continue;
        }
        if (consume(&tok, tok, "++")) {
            Node *new = calloc(1, sizeof(Node));
            new->kind = ND_INCR;
            new->lhs = as_ptr(node);
            new->ty = node->ty;
            node = new;
            continue;
        }
        if (consume(&tok, tok, "--")) {
            Node *new = calloc(1, sizeof(Node));
            new->kind = ND_DECR;
            new->lhs = as_ptr(node);
            new->ty = node->ty;
            node = new;
            continue;
        }
        break;
    }

    *rest = tok;
    return node;
}

//
//  cast_expression = ( "(" type_name ")" )* unary_expression
//
static Node *
cast(const Token **rest, const Token *tok, Env *env) {
    for (;;) {
        if (consume(&tok, tok, "(")) {
            not_implemented(&tok->span, "cast");
            expect(&tok, tok, ")");
        }
        break;
    }
    return unary(rest, tok, env);
}

//
//  unary_expression =
//      postfix_expression
//      "++" unary_expression
//      "--" unary_expression
//      unary_operator cast_expression
//      "sizeof" unary_expression
//      "sizeof" "(" type_name ")"
//      "_Alignof" "(" type_name ")"
//  unary_operator = "&" | "*" | "+" | "-" | "~" | "!"
//
static Node *
unary(const Token **rest, const Token *tok, Env *env) {
    if (consume(&tok, tok, "++")) {
        not_implemented(&tok->span, NULL);
    }
    if (consume(&tok, tok, "--")) {
        not_implemented(&tok->span, NULL);
    }
    if (consume(&tok, tok, "&")) {
        Node *node = unary(&tok, tok, env);
        if (node && node->ty) {
            *rest = tok;
            return refer(node, node->ty);
        }
        error("Internal compile error: try to obtain the address to an "
              "unknown "
              "type");
    }
    if (consume(&tok, tok, "*")) {
        return deref(as_ptr(unary(rest, tok, env)));
    }
    if (consume(&tok, tok, "+")) {
        return cast(rest, tok, env);
    }
    if (consume(&tok, tok, "-")) {
        return new_node(ND_SUB, new_node_num(0), cast(rest, tok, env));
    }
    if (consume(&tok, tok, "~")) {
        not_implemented(&tok->span, NULL);
    }
    if (consume(&tok, tok, "!")) {
        not_implemented(&tok->span, NULL);
    }
    if (consume(&tok, tok, "sizeof")) {
        if (consume(&tok, tok, "(")) {
            const Type *ty = type_name(&tok, tok, env);
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
    if (consume(&tok, tok, "_Alignof")) {
        not_implemented(&tok->span, NULL);
    }
    return postfix(rest, tok, env);
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

//
//  compound_statement =
//      "{" ( declaration | statement )* "}"
//
static const Statement *
compound(const Token **rest, const Token *tok, Env *env) {
    Env new = make_block_scope(env);
    if (!consume(&tok, tok, "{")) {
        return NULL;
    }

    Statement *comp = calloc(1, sizeof(Statement));
    comp->kind = ST_COMPOUND;

    BlockItems *body = NULL;
    while (!consume(&tok, tok, "}")) {
        BlockItems *next = calloc(1, sizeof(BlockItems));
        const Declaration *decl = declaration(&tok, tok, &new);
        if (decl) {
            next->declaration = decl;
        } else {
            next->statement = stmt(&tok, tok, &new);
        }
        if (body) {
            body->next = next;
            body = body->next;
        } else {
            comp->block = next;
            body = comp->block;
        }
    }

    *rest = tok;
    return comp;
}

//
//  function_definition =
//      declaration_specifiers declarator declartion_list?
//      compound_statement
//
static Function *
function(const Token **rest, const Token *tok, Env *parent) {
    const Type *ty = declspec(&tok, tok, parent).ty;
    if (ty == NULL) {
        return NULL;
    }

    const Declaration *decl = declarator(&tok, tok, parent, ty);
    if (decl == NULL || decl->var->ty->ty != FUNCTION) {
        return NULL;
    }

    Function *fn = calloc(1, sizeof(Function));
    fn->def = decl->var;

    Env env = make_scope(parent);
    const ParamList *arg = fn->def->ty->args;
    while (arg) {
        Declaration *decl = arg->decl;
        if (!declare_arg(&env, decl->var)) {
            error_at(&tok->span, "'%s' is already declared", decl->var->ident);
        }
        fn->num_args++;
        arg = arg->next;
    }
    const int argument_offset = env.maximum_offset;

    const Statement *body = compound(&tok, tok, &env);
    if (body == NULL) {
        free(fn);
        return NULL;
    }

    fn->body = body;
    fn->lvar_offset = env.maximum_offset - argument_offset;

    declare_fn(parent, decl->var);

    *rest = tok;

    return fn;
}

//
//  initializer =
//      assignment_expression
//      "{" initializer_list ","? "}"
//  initializer_list = designation? initializer ( "," designation?
//  initializer)*
//
//  designation = designator_list "=" designator_list = designator+
//  designator =
//      "[" constant_expression "]"
//      "." identifier
//
static const Initializer *
initializer(const Token **rest, const Token *tok, Env *env) {
    if (consume(&tok, tok, "{")) {
        Initializer *init = calloc(1, sizeof(Initializer));
        init->num_initializers = 1;
        init->list = calloc(1, sizeof(InitList));
        init->list->inner = initializer(&tok, tok, env);
        if (init->list->inner == NULL) {
            error_at(&tok->span, "Failed to parse initializer.");
        }
        InitList *last = init->list;

        while (consume(&tok, tok, ",")) {
            const Initializer *inner = initializer(&tok, tok, env);
            if (inner) {
                last->next = calloc(1, sizeof(InitList));
                last->next->inner = inner;
                last = last->next;
                init->num_initializers++;
            } else {
                break;
            }
        }
        expect(&tok, tok, "}");

        *rest = tok;
        return init;
    } else {
        Node *e = expr(&tok, tok, env);
        if (e) {
            Initializer *init = calloc(1, sizeof(Initializer));
            init->expr = as_ptr(e);
            *rest = tok;
            return init;
        }
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
    const DeclSpec spec = declspec(&tok, tok, env);
    if (spec.ty == NULL) {
        return NULL;
    }

    Declaration *decl = declarator(&tok, tok, env, spec.ty);
    if (decl == NULL) {
        expect(&tok, tok, ";");

        declare_tag(env, spec.ty);
        *rest = tok;
        decl = calloc(1, sizeof(Declaration));
        return decl;
    }

    // typedef
    if (spec.storage == TYPEDEF) {
        expect(&tok, tok, ";");
        declare_typedef(env, decl->var);
        *rest = tok;
        decl->var = NULL;
        return decl;
    }

    if (consume(&tok, tok, "=")) {
        const Initializer *init = initializer(&tok, tok, env);
        if (init == NULL) {
            return NULL;
        }
        if (decl->var->ty->ty == ARRAY && decl->var->ty->array_size == -1) {
            int array_size = -1;
            if (init->expr && init->expr->kind == ND_STRING) {
                array_size = strlen(init->expr->str) + 1;
            } else if (init->list) {
                array_size = init->num_initializers;
            } else {
                error("array must be initialized with a brace-enclosed "
                      "initializer");
            }
            // XXX: Replace array type with newly allocated one
            // due to const Type*.
            decl->var->ty = mk_array(decl->var->ty->ptr_to, array_size);
        }
        decl->init = init;
    }
    expect(&tok, tok, ";");

    if (!declare_var(env, decl->var)) {
        error_at(&tok->span, "'%s' is already declared", decl->var->ident);
    }

    *rest = tok;
    return decl;
}

//
//  labeled_statement =
//      identifier ":" statement
//      "case" constant_expression ":" statement
//      "default" ":" statement
//
static const Statement *
labeled(const Token **rest, const Token *tok, Env *env) {
    if (consume(&tok, tok, "case")) {
        const int jump_index = make_jump_scope(env).jump_index;

        int val;
        if (!number(&tok, tok, &val)) {
            error("a number is expected after case");
        }
        expect(&tok, tok, ":");
        const Statement *body = stmt(&tok, tok, env);
        if (body == NULL) {
            error("a statement is expected after a case label");
        }
        *rest = tok;

        Label label = {CASE, jump_index};
        label.val = val;

        Statement *statement = calloc(1, sizeof(Statement));
        statement->kind = ST_LABEL;
        statement->label = label;
        statement->body = body;

        push_label(env, &statement->label);

        return statement;
    } else if (consume(&tok, tok, "default")) {
        const int jump_index = make_jump_scope(env).jump_index;
        expect(&tok, tok, ":");
        const Statement *body = stmt(&tok, tok, env);
        if (body == NULL) {
            error("a statement is expected after a default label");
        }
        *rest = tok;

        Label label = {DEFAULT, jump_index};

        Statement *statement = calloc(1, sizeof(Statement));
        statement->kind = ST_LABEL;
        statement->label = label;
        statement->body = body;

        push_label(env, &statement->label);

        return statement;
    }

    return NULL;
}

//
//  selection_statement =
//      "if" "(" expression ")" statement ( "else" statement )?
//      "switch" "(" expression ")" statement
//
static const Statement *
selection(const Token **rest, const Token *tok, Env *env) {
    Statement *statement = NULL;

    if (consume(&tok, tok, "if")) {
        const Env new = make_jump_scope(env);

        expect(&tok, tok, "(");
        Node *cond = expr(&tok, tok, env);
        expect(&tok, tok, ")");
        const Statement *then_body = stmt(&tok, tok, env);
        const Statement *else_body = NULL;
        if (consume(&tok, tok, "else")) {
            else_body = stmt(&tok, tok, env);
        }

        statement = calloc(1, sizeof(Statement));
        statement->kind = ST_IF;
        statement->jump_index = new.jump_index;
        statement->cond = cond;
        statement->then_body = then_body;
        statement->else_body = else_body;
    } else if (consume(&tok, tok, "switch")) {
        Env new = make_switch_scope(env);
        expect(&tok, tok, "(");
        Node *value = expr(&tok, tok, env);
        expect(&tok, tok, ")");
        const Statement *body = stmt(&tok, tok, &new);

        statement = calloc(1, sizeof(Statement));
        statement->kind = ST_SWITCH;
        statement->value = value;
        statement->body = body;
        statement->labels = new.labels;
    }

    if (statement) {
        *rest = tok;
    }
    return statement;
}

//
//  iteration_statement =
//      while "(" expression ")" statement
//      "do" statement "while" "(" expression ")" ";"
//      "for" "(" expression? ";" expression? ";" expression? ")" statement
//      "for" "(" declaration expression? ":" expression? ")" statement
//
static const Statement *
iteration(const Token **rest, const Token *tok, Env *env) {
    Statement *statement = NULL;
    if (consume(&tok, tok, "while")) {
        Env new = make_jump_scope(env);

        expect(&tok, tok, "(");
        Node *cond = expr(&tok, tok, &new);
        expect(&tok, tok, ")");
        const Statement *body = stmt(&tok, tok, &new);

        statement = calloc(1, sizeof(Statement));
        statement->kind = ST_WHILE;
        statement->jump_index = new.jump_index;
        statement->cond = cond;
        statement->body = body;
    } else if (consume(&tok, tok, "do")) {
        not_implemented(&tok->span, "do");
    } else if (consume(&tok, tok, "for")) {
        Env new = make_jump_scope(env);

        statement = calloc(1, sizeof(Statement));
        statement->kind = ST_FOR;
        statement->jump_index = new.jump_index;

        expect(&tok, tok, "(");
        if (!consume(&tok, tok, ";")) {
            Node *init = NULL;
            const Declaration *decl = declaration(&tok, tok, &new);
            if (decl) {
                statement->declaration = decl;
            } else if ((init = expr(&tok, tok, &new))) {
                expect(&tok, tok, ";");
                statement->init = init;
            } else {
                unexpected("expression or declaration", tok);
            }
        }

        if (!consume(&tok, tok, ";")) {
            statement->cond = expr(&tok, tok, &new);
            expect(&tok, tok, ";");
        }
        if (!consume(&tok, tok, ")")) {
            statement->end = expr(&tok, tok, &new);
            expect(&tok, tok, ")");
        }

        statement->body = stmt(&tok, tok, &new);
    }
    if (statement) {
        *rest = tok;
    }
    return statement;
}

//
//  jump_statement =
//      "goto" identifier ";"
//      "continue" ";"
//      "break" ";"
//      "return" expression? ";"
//
static Statement *
jump(const Token **rest, const Token *tok, Env *env) {
    if (consume(&tok, tok, "goto")) {
        not_implemented(&tok->span, "goto");
    }
    if (consume(&tok, tok, "continue")) {
        expect(rest, tok, ";");

        Statement *statement = calloc(1, sizeof(Statement));
        statement->kind = ST_CONTINUE;
        statement->jump_index = env->jump_index;

        return statement;
    }
    if (consume(&tok, tok, "break")) {
        expect(rest, tok, ";");

        Statement *statement = calloc(1, sizeof(Statement));
        statement->kind = ST_BREAK;
        statement->jump_index = env->jump_index;

        return statement;
    }
    if (consume(&tok, tok, "return")) {
        Node *retval = as_ptr(expr(&tok, tok, env));
        expect(rest, tok, ";");

        Statement *statement = calloc(1, sizeof(Statement));
        statement->kind = ST_RETURN;
        statement->retval = retval;

        return statement;
    }
    return NULL;
}

//
//  expression_statement =
//      expression? ";"
//
static const Statement *
expr_stmt(const Token **rest, const Token *tok, Env *env) {
    Node *expression = expr(&tok, tok, env);
    if (!consume(rest, tok, ";")) {
        return NULL;
    }

    Statement *statement = calloc(1, sizeof(Statement));
    statement->kind = ST_EXPRESSION;
    statement->expression = expression;
    return statement;
}

//
//  statement =
//      labeled_statement
//      compound_statement
//      expression_statement
//      selection_statement
//      iteration_statement
//      jump_statement
//
static const Statement *
stmt(const Token **rest, const Token *tok, Env *env) {
    const Statement *statement = NULL;

    if ((statement = labeled(&tok, tok, env))) {
    } else if ((statement = compound(&tok, tok, env))) {
    } else if ((statement = expr_stmt(&tok, tok, env))) {
    } else if ((statement = selection(&tok, tok, env))) {
    } else if ((statement = iteration(&tok, tok, env))) {
    } else {
        statement = jump(&tok, tok, env);
    }

    if (statement) {
        *rest = tok;
    }

    return statement;
}

//
//  program = ( function | declaration )*
//
void
program(const Token *token, Env *env, Unit *code[]) {
    int i = 0;
    while (!at_eof(token)) {
        const Function *fn = function(&token, token, env);
        if (fn) {
            code[i] = calloc(1, sizeof(Unit));
            code[i]->function = fn;
            i++;
            continue;
        }

        const Declaration *decl = declaration(&token, token, env);
        if (decl) {
            if (decl->var) {
                code[i] = calloc(1, sizeof(Unit));
                code[i]->declaration = decl;
                i++;
            }
            continue;
        }

        error_at(&token->span, "Cannot parse the program.");
    }
    code[i] = NULL;
}
