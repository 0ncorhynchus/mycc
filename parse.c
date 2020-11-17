#include "mycc.h"
#include <string.h>

//
// Declarations
//

typedef enum {
    TS_VOID = 0x0001,
    TS_CHAR = 0x0002,
    TS_SHORT = 0x0004,
    TS_INT = 0x0008,
    TS_LONG = 0x0010,
    TS_FLOAT = 0x0040,
    TS_DOUBLE = 0x0080,
    TS_SIGNED = 0x0100,
    TS_UNSIGNED = 0x0200,
    TS_BOOL = 0x0400,
    TS_COMPLEX = 0x0800,
    TS_ATOMIC = 0x1000,
    TS_STRUCT_OR_UNION = 0x2000,
    TS_ENUM = 0x4000,
    TS_TYPEDEF_NAME = 0x8000,
} TypeSpecKind;

typedef struct {
    int kind;
    const Type *ty;
} TypeSpec;

static const TypeSpec
type_specifier(const Token **rest, const Token *tok, const Env *env);

typedef enum {
    TQ_NULL,
    TQ_CONST,
    TQ_RESTRICT,
    TQ_VOLATILE,
    TQ_ATOMIC,
} TypeQualifier;

static const TypeQualifier
type_qualifier(const Token **rest, const Token *tok);

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

static bool
composite_type_spec(TypeSpec *x, const TypeSpec y) {
    switch (x->kind & y.kind) {
    case 0:
    case TS_LONG:
        break;
    default:
        return false;
    }

    x->kind += y.kind;
    if (y.ty) {
        x->ty = y.ty;
    }
    return true;
}

const Type *
construct_type(const TypeSpec spec, const Span *span) {
    switch (spec.kind) {
    case 0:
        return NULL;
    case TS_VOID:
        return &VOID_T;
    case TS_CHAR:
        return &CHAR_T;
    case TS_SIGNED + TS_CHAR:
        not_implemented(span, "signed char");
        break;
    case TS_UNSIGNED + TS_CHAR:
        not_implemented(span, "unsigned char");
        break;
    case TS_SHORT:
    case TS_SIGNED + TS_SHORT:
    case TS_SHORT + TS_INT:
    case TS_SIGNED + TS_SHORT + TS_INT:
        return &SHORT_T;
    case TS_UNSIGNED + TS_SIGNED:
    case TS_UNSIGNED + TS_SIGNED + TS_INT:
        return &USHORT_T;
    case TS_INT:
    case TS_SIGNED + TS_INT:
        return &INT_T;
    case TS_UNSIGNED + TS_INT:
        return &UINT_T;
    case TS_LONG:
    case TS_SIGNED + TS_LONG:
    case TS_LONG + TS_INT:
    case TS_SIGNED + TS_LONG + TS_INT:
        return &LONG_T;
    case TS_UNSIGNED + TS_LONG:
    case TS_UNSIGNED + TS_LONG + TS_INT:
        return &ULONG_T;
    case TS_LONG + TS_LONG:
    case TS_SIGNED + TS_LONG + TS_LONG:
    case TS_LONG + TS_LONG + TS_INT:
    case TS_SIGNED + TS_LONG + TS_LONG + TS_INT:
        return &LONG_LONG_T;
    case TS_UNSIGNED + TS_LONG + TS_LONG:
    case TS_UNSIGNED + TS_LONG + TS_LONG + TS_INT:
        return &ULONG_LONG_T;
    case TS_FLOAT:
        return &FLOAT_T;
    case TS_DOUBLE:
        return &DOUBLE_T;
    case TS_LONG + TS_DOUBLE:
        return &LONG_DOUBLE_T;
    case TS_BOOL:
        return &BOOL_T;
    case TS_FLOAT + TS_COMPLEX:
        not_implemented(span, "float _Complex");
        break;
    case TS_DOUBLE + TS_COMPLEX:
        not_implemented(span, "double _Complex");
        break;
    case TS_LONG + TS_DOUBLE + TS_COMPLEX:
        not_implemented(span, "long double _Complex");
        break;
    case TS_ATOMIC:
        not_implemented(span, "_Atomic");
        break;
    case TS_STRUCT_OR_UNION:
    case TS_ENUM:
    case TS_TYPEDEF_NAME:
        return spec.ty;

    default:
        error_at(span, "invalid type");
        break;
    }

    return NULL;
}

//
//  specifier_qualifier_list = ( type_specifier | type_qualifier )+
//
static const Type *
spec_qual_list(const Token **rest, const Token *tok, const Env *env) {
    const Span start = tok->span;
    TypeSpec ty_spec = {};
    for (;;) {
        const TypeSpec spec = type_specifier(&tok, tok, env);
        if (spec.kind) {
            if (!composite_type_spec(&ty_spec, spec)) {
                error_at(&tok->span, "invalid type specifier");
            }
            continue;
        }

        const TypeQualifier qualifier = type_qualifier(&tok, tok);
        if (qualifier != TQ_NULL) {
            continue;
        }

        break;
    }

    const Type *ty = construct_type(ty_spec, &start);
    if (ty) {
        *rest = tok;
    }
    return ty;
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
    if (consume(&tok, tok, "_Atomic")) {
        expect(&tok, tok, "(");
        const Type *ty = type_name(&tok, tok, env);
        if (ty == NULL) {
            unexpected("type name", tok);
        }
        expect(&tok, tok, ")");
        *rest = tok;
        return ty;
    }

    return NULL;
}

static const TypeSpec
type_spec(const TypeSpecKind kind, const Type *ty) {
    const TypeSpec spec = {kind, ty};
    return spec;
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
static const TypeSpec
type_specifier(const Token **rest, const Token *tok, const Env *env) {
    if (consume(rest, tok, "void")) {
        return type_spec(TS_VOID, NULL);
    }
    if (consume(rest, tok, "char")) {
        return type_spec(TS_CHAR, NULL);
    }
    if (consume(rest, tok, "short")) {
        return type_spec(TS_SHORT, NULL);
    }
    if (consume(rest, tok, "int")) {
        return type_spec(TS_INT, NULL);
    }
    if (consume(rest, tok, "long")) {
        return type_spec(TS_LONG, NULL);
    }
    if (consume(rest, tok, "float")) {
        return type_spec(TS_FLOAT, NULL);
    }
    if (consume(rest, tok, "double")) {
        return type_spec(TS_DOUBLE, NULL);
    }
    if (consume(rest, tok, "signed")) {
        return type_spec(TS_SIGNED, NULL);
    }
    if (consume(rest, tok, "unsigned")) {
        return type_spec(TS_UNSIGNED, NULL);
    }
    if (consume(rest, tok, "_Bool")) {
        return type_spec(TS_BOOL, NULL);
    }
    if (consume(rest, tok, "_Complex")) {
        return type_spec(TS_COMPLEX, NULL);
    }

    const Type *ty;
    ty = atomic_type_spec(rest, tok, env);
    if (ty) {
        return type_spec(TS_ATOMIC, ty);
    }
    ty = struct_union_spec(rest, tok, env);
    if (ty) {
        if (ty->struct_ty->members == NULL) {
            const Type *orig = get_tag(env, ty->struct_ty->tag);
            if (orig) {
                ty = orig;
            }
        }
        return type_spec(TS_STRUCT_OR_UNION, ty);
    }

    ty = enum_specifier(rest, tok);
    if (ty) {
        if (ty->enum_ty->consts == NULL) {
            const Type *orig = get_tag(env, ty->enum_ty->tag);
            if (orig) {
                ty = orig;
            }
        }
        return type_spec(TS_ENUM, ty);
    }

    const Token *typedef_name = consume_ident(&tok, tok);
    if (typedef_name) {
        const Type *ty = get_typedef(env, char_from_span(&typedef_name->span));
        if (ty) {
            *rest = tok;
            return type_spec(TS_TYPEDEF_NAME, ty);
        }
    }

    return type_spec(0, NULL);
}

//
//  type_qualifier = "const" | "restrict" | "volatile" | "_Atomic"
//
static const TypeQualifier
type_qualifier(const Token **rest, const Token *tok) {
    if (consume(rest, tok, "const")) {
        return TQ_CONST;
    }
    if (consume(rest, tok, "restrict")) {
        return TQ_RESTRICT;
    }
    if (consume(rest, tok, "volatile")) {
        return TQ_VOLATILE;
    }
    if (consume(rest, tok, "_Atomic")) {
        return TQ_ATOMIC;
    }
    return TQ_NULL;
}

static const Type *
pointer(const Token **rest, const Token *tok, const Type *ty) {
    while (consume(&tok, tok, "*")) {
        ty = mk_ptr(ty);
    }

    *rest = tok;
    return ty;
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
    const Span start = tok->span;
    DeclSpec spec = {};
    TypeSpec ty_spec = {};

    for (;;) {
        const StorageClass s = storage_spec(&tok, tok);
        if (s != EMPTY) {
            if ((spec.storage & s) != 0) {
                error("storage-class error");
            }
            spec.storage |= s;
            continue;
        }

        const TypeSpec ty = type_specifier(&tok, tok, env);
        if (ty.kind) {
            if (!composite_type_spec(&ty_spec, ty)) {
                error_at(&tok->span, "invalid type specifier");
            }
            continue;
        }

        const TypeQualifier tq = type_qualifier(&tok, tok);
        if (tq != TQ_NULL) {
            continue;
        }

        break;
    }

    spec.ty = construct_type(ty_spec, &start);
    if (spec.ty) {
        *rest = tok;
    }

    return spec;
}

//
// abstract_declarator? = pointer? direct_abstract_declarator?
//
static const Type *
abstract_declarator(const Token **rest, const Token *tok, const Type *ty) {
    ty = pointer(&tok, tok, ty);

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

//
//  parameter_declaration =
//      declaration_specifiers declarator
//      declaration_specifiers abstract_declarator?
//
static Declaration *
param_decl(const Token **rest, const Token *tok, const Env *env) {
    const Type *ty = declspec(&tok, tok, env).ty;
    if (ty == NULL) {
        return NULL;
    }

    Declaration *decl = declarator(&tok, tok, env, ty);
    if (decl) {
        *rest = tok;
        return decl;
    }

    ty = abstract_declarator(rest, tok, ty);
    decl = calloc(1, sizeof(Declaration));
    decl->var = calloc(1, sizeof(Var));
    decl->var->ty = ty;

    return decl;
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
    ty = pointer(&tok, tok, ty);
    return direct_declarator(rest, tok, env, ty);
}

//
//  type_name = specifier_qualifier_list abstract_declarator?
//
static const Type *
type_name(const Token **rest, const Token *tok, const Env *env) {
    const Type *ty = spec_qual_list(&tok, tok, env);
    if (ty == NULL) {
        return NULL;
    }

    return abstract_declarator(rest, tok, ty);
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

        if (decl->var->ty == &VOID_T) {
            if (arg->next != NULL || fn->num_args > 0) {
                error_at(&tok->span, "void is allowed only for empty argument");
            }
            break;
        }

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
