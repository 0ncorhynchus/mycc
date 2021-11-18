#include "mycc.h"
#include <assert.h>
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
type_specifier(const Token **rest, const Token *tok, Env *env);

static const TypeQualifier
type_qualifier(const Token **rest, const Token *tok);

typedef struct PtrList PtrList;
struct PtrList {
    PtrList *next;
    int qualifier;
};

typedef struct ArrayFunDecl ArrayFunDecl;
struct ArrayFunDecl {
    bool is_func;
    ArrayFunDecl *next;
    int array_size;
    Vars *args;
};

typedef struct Declarator Declarator;

typedef struct {
    const char *ident;
    const Declarator *declarator;
    ArrayFunDecl *arr_fun_decl;
} DirectDeclarator;

struct Declarator {
    const PtrList *ptr;
    const DirectDeclarator *direct_decl;
};

static const Declarator *
declarator(const Token **rest, const Token *tok, Env *env);

static Var *
declarator_to_var(const Type *ty, const Declarator *decl);

static const Type *
type_name(const Token **rest, const Token *tok, Env *env);

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
    if (tok->kind != TK_CONST) {
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
//      enumeration_constant "=" constant_expression
//
static const Type *
enum_specifier(const Token **rest, const Token *tok) {
    if (!consume(&tok, tok, "enum")) {
        return NULL;
    }

    Enum e = {};
    const Token *tag = consume_ident(&tok, tok);
    if (tag) {
        e.tag = char_from_span(&tag->span);
    }

    if (consume(&tok, tok, "{")) {
        int val = 0;
        const Token *constant = consume_ident(&tok, tok);
        if (constant == NULL) {
            error_at(&tok->span, "Failed to parse enum");
        }
        e.consts = calloc(1, sizeof(String));
        if (consume(&tok, tok, "=")) {
            if (!number(&tok, tok, &e.consts->index)) {
                error("Expect a number.");
            }
        } else {
            e.consts->index = val;
        }
        e.consts->ident = char_from_span(&constant->span);

        String *last = e.consts;
        while (!consume(&tok, tok, "}")) {
            expect(&tok, tok, ",");

            constant = consume_ident(&tok, tok);
            if (constant == NULL) {
                expect(&tok, tok, "}");
                break;
            }

            val++;
            last->next = calloc(1, sizeof(String));
            if (consume(&tok, tok, "=")) {
                if (!number(&tok, tok, &last->next->index)) {
                    error("Expect a number.");
                }
            } else {
                last->next->index = val;
            }
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
        return &SIGNED_CHAR_T;
        break;
    case TS_UNSIGNED + TS_CHAR:
        return &UNSIGNED_CHAR_T;
    case TS_SHORT:
    case TS_SIGNED + TS_SHORT:
    case TS_SHORT + TS_INT:
    case TS_SIGNED + TS_SHORT + TS_INT:
        return &SHORT_T;
    case TS_UNSIGNED + TS_SHORT:
    case TS_UNSIGNED + TS_SHORT + TS_INT:
        return &USHORT_T;
    case TS_INT:
    case TS_SIGNED + TS_INT:
        return &INT_T;
    case TS_UNSIGNED:
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
spec_qual_list(const Token **rest, const Token *tok, Env *env) {
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

const Type *
generate_ptr_type(const PtrList *plist, const Type *ty) {
    while (plist) {
        ty = mk_ptr(ty, plist->qualifier);
        plist = plist->next;
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
struct_union_spec(const Token **rest, const Token *tok, Env *env) {
    bool is_struct = true;
    if (consume(&tok, tok, "union")) {
        is_struct = false;
    } else if (!consume(&tok, tok, "struct")) {
        return NULL;
    }

    const Token *tag = consume_ident(&tok, tok);
    size_t size = 0;
    Vars *members = NULL;
    if (consume(&tok, tok, "{")) {
        Vars *last = NULL;
        const Type *ty = spec_qual_list(&tok, tok, env);
        const Declarator *decl = declarator(&tok, tok, env);
        Var *var = declarator_to_var(ty, decl);
        if (var) {
            if (is_struct) {
                var->offset = size;
            } else {
                var->offset = 0;
            }
            members = calloc(1, sizeof(Vars));
            members->var = var;
            last = members;
            const size_t sz = expand_for_align(sizeof_ty(var->ty));
            if (is_struct) {
                size += sz;
            } else if (sz > size) {
                size = sz;
            }
        } else {
            if ((ty->ty != STRUCT && ty->ty != UNION) ||
                ty->struct_ty.tag != NULL) {
                error_at(&tok->span, "Invalid type for anonymous member");
            }
            const Vars *inner = ty->struct_ty.members;
            while (inner) {
                if (is_struct) {
                    inner->var->offset += size;
                }
                if (members) {
                    last->next = calloc(1, sizeof(Vars));
                    last = last->next;
                    last->var = inner->var;
                } else {
                    members = calloc(1, sizeof(Vars));
                    members->var = inner->var;
                    last = members;
                }
                inner = inner->next;
            }
            const size_t sz = expand_for_align(sizeof_ty(ty));
            if (is_struct) {
                size += sz;
            } else if (sz > size) {
                size = sz;
            }
        }
        expect(&tok, tok, ";");

        while (!consume(&tok, tok, "}")) {
            const Type *ty = spec_qual_list(&tok, tok, env);
            const Declarator *decl = declarator(&tok, tok, env);
            Var *var = declarator_to_var(ty, decl);
            if (var) {
                if (is_struct) {
                    var->offset = size;
                } else {
                    var->offset = 0;
                }
                last->next = calloc(1, sizeof(Vars));
                last = last->next;
                last->var = var;
                const size_t sz = expand_for_align(sizeof_ty(var->ty));
                if (is_struct) {
                    size += sz;
                } else if (sz > size) {
                    size = sz;
                }
            } else {
                if ((ty->ty != STRUCT && ty->ty != UNION) ||
                    ty->struct_ty.tag != NULL) {
                    error_at(&tok->span, "Invalid type for anonymous member");
                }
                const Vars *inner = ty->struct_ty.members;
                while (inner) {
                    if (is_struct) {
                        inner->var->offset += size;
                    }
                    last->next = calloc(1, sizeof(Vars));
                    last = last->next;
                    last->var = inner->var;
                    inner = inner->next;
                }
                const size_t sz = expand_for_align(sizeof_ty(ty));
                if (is_struct) {
                    size += sz;
                } else if (sz > size) {
                    size = sz;
                }
            }
            expect(&tok, tok, ";");
        }
    } else if (tag == NULL) {
        error("struct or union requires an identifier or a block at least");
    }

    StructOrUnion st = {};
    if (tag) {
        st.tag = char_from_span(&tag->span);
    }
    st.size = size;
    st.members = members;

    Type *ty = calloc(1, sizeof(Type));
    if (is_struct) {
        ty->ty = STRUCT;
    } else {
        ty->ty = UNION;
    }
    ty->struct_ty = st;

    *rest = tok;
    return ty;
}

//
// atomic_type_specifier = "_Atomic" "(" type_name ")"
//
static const Type *
atomic_type_spec(const Token **rest, const Token *tok, Env *env) {
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
type_specifier(const Token **rest, const Token *tok, Env *env) {
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
        if (ty->struct_ty.members == NULL) {
            const Type *orig = get_tag(env, ty->struct_ty.tag);
            if (orig) {
                ty = orig;
            }
        }
        return type_spec(TS_STRUCT_OR_UNION, ty);
    }

    ty = enum_specifier(rest, tok);
    if (ty) {
        if (ty->enum_ty.consts == NULL) {
            const Type *orig = get_tag(env, ty->enum_ty.tag);
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

//
//  type_qualifier_list = type_qualifier+
//
static int
type_qualifier_list(const Token **rest, const Token *tok) {
    int qualifier = TQ_NULL;
    for (;;) {
        const TypeQualifier tq = type_qualifier(&tok, tok);
        if (tq != TQ_NULL) {
            qualifier |= tq;
        } else {
            break;
        }
    }

    *rest = tok;
    return qualifier;
}

//
//  pointer = ( '*' type_qualifier_list? )+
//
static const PtrList *
pointer(const Token **rest, const Token *tok) {
    PtrList *list = NULL;
    PtrList *current;
    while (consume(&tok, tok, "*")) {
        const int qualifier = type_qualifier_list(&tok, tok);
        PtrList *new = calloc(1, sizeof(PtrList));
        new->qualifier = qualifier;
        if (list) {
            current->next = new;
            current = current->next;
        } else {
            list = new;
            current = list;
        }
    }

    *rest = tok;
    return list;
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

typedef enum {
    FS_NULL,
    FS_INLINE,
    FS_NORETURN,
} FuncSpec;

//
//  function_specifier = "inline" | "_Noreturn"
//
static FuncSpec
function_specifier(const Token **rest, const Token *tok) {
    if (consume(rest, tok, "inline")) {
        return FS_INLINE;
    }
    if (consume(rest, tok, "_Noreturn")) {
        return FS_NORETURN;
    }
    return FS_NULL;
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
declspec(const Token **rest, const Token *tok, Env *env) {
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

        const FuncSpec fs = function_specifier(&tok, tok);
        if (fs != FS_NULL) {
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

static const Type *
abstract_declarator(const Token **rest, const Token *tok, const Type *ty,
                    Env *env);

static Vars *
param_list(const Token **rest, const Token *tok, Env *env);

//
//  direct_abstract_declarator =
//      "(" abstract_declarator ")"
//      direct_abstract_declarator? "[" type_qualifier_list? assignment_expression? "]"
//      direct_abstract_declarator? "[" "static" type_qualifier_list? assignment_expression "]"
//      direct_abstract_declarator? "[" type_qualifier_list "static" assignment_expression "]"
//      direct_abstract_declarator? "[" "*" "]"
//      direct_abstract_declarator? "(" parameter_type_list? ")"
//
static const Type *
direct_abstract_declarator(const Token **rest, const Token *tok, const Type *ty,
                           Env *env) {
    const Type *retval = ty;

    if (consume(&tok, tok, "(")) {
        const Type *t = abstract_declarator(&tok, tok, retval, env);
        if (t) {
            retval = t;
        } else {
            const Vars *params = param_list(&tok, tok, env);
            if (params == NULL) {
                return NULL;
            }
            retval = mk_func(retval, params);
        }
        expect(&tok, tok, ")");
    }

    while (true) {
        if (consume(&tok, tok, "(")) {
            const Vars *params = param_list(&tok, tok, env);
            if (params == NULL) {
                return NULL;
            }
            retval = mk_func(retval, params);
            expect(&tok, tok, ")");
        } else if (consume(&tok, tok, "[")) {
            int size;
            if (!number(&tok, tok, &size)) {
                error("Expect a number.");
            }
            expect(&tok, tok, "]");

            retval = mk_array(retval, size);
        } else {
            break;
        }
    }

    if (retval == ty) {
        return NULL;
    }

    *rest = tok;
    return retval;
}

//
//  abstract_declarator =
//      pointer
//      pointer? direct_abstract_declarator
//
static const Type *
abstract_declarator(const Token **rest, const Token *tok, const Type *ty,
                    Env *env) {
    const Type *retval = ty;
    const PtrList *plist = pointer(&tok, tok);
    retval = generate_ptr_type(plist, retval);

    const Type *t = direct_abstract_declarator(&tok, tok, retval, env);
    if (t) {
        retval = t;
    }

    if (retval == ty) {
        return NULL;
    }

    *rest = tok;
    return retval;
}

//
//  parameter_declaration =
//      declaration_specifiers declarator
//      declaration_specifiers abstract_declarator?
//
static Var *
param_decl(const Token **rest, const Token *tok, Env *env) {
    const Type *ty = declspec(&tok, tok, env).ty;
    if (ty == NULL) {
        return NULL;
    }

    const Declarator *decl = declarator(&tok, tok, env);
    Var *var = declarator_to_var(ty, decl);
    if (var) {
        *rest = tok;
        return var;
    }

    const Type *t = abstract_declarator(&tok, tok, ty, env);
    if (t) {
        ty = t;
    }
    var = calloc(1, sizeof(Var));
    var->ty = ty;

    *rest = tok;
    return var;
}

//
//  parameter_type_list = parameter_list ("," "...")?
//  parameter_list = parameter_declaration ("," parameter_declaration)*
//
static Vars *
param_list(const Token **rest, const Token *tok, Env *env) {
    Var *var = param_decl(&tok, tok, env);
    if (var == NULL)
        return NULL;

    Vars *top = calloc(1, sizeof(Vars));
    top->var = var;
    Vars *last = top;
    for (;;) {
        if (!consume(&tok, tok, ",")) {
            break;
        }
        var = param_decl(&tok, tok, env);
        if (var) {
            last->next = calloc(1, sizeof(Vars));
            last->next->var = var;
            last = last->next;
        } else if (consume(&tok, tok, "...")) {
            // "..." == Vars { NULL, NULL }
            last->next = calloc(1, sizeof(Vars));
            break;
        } else {
            unexpected("<parameter_declaration> or \"...\"", tok);
        }
    }

    *rest = tok;
    return top;
}

//
//  direct_declarator_post =
//      "[" "static" type_qualifier_list? assign "]"
//      "[" type_qualifier_list "static" assign "]"
//      "[" type_qualifier_list? assign? "]"
//      "[" type_qualifier_list? "*" "]"
//      "(" parameter_type_list? ")"
//      "(" identifier_list? ")"
//
static ArrayFunDecl *
direct_declarator_post(const Token **rest, const Token *tok, Env *env) {
    ArrayFunDecl *retval = calloc(1, sizeof(ArrayFunDecl));
    if (consume(&tok, tok, "(")) {
        retval->is_func = true;
        Vars *args = param_list(&tok, tok, env);
        if (args) {
            retval->args = args;
        } else {
            // identifier_list
        }
        expect(&tok, tok, ")");
    } else if (consume(&tok, tok, "[")) {
        retval->is_func = false;
        if (consume(&tok, tok, "static")) {
            not_implemented(&tok->span, NULL);
        } else {
            const int qualifier = type_qualifier_list(&tok, tok);
            if (qualifier != TQ_NULL) {
                if (consume(&tok, tok, "static")) {
                    not_implemented(&tok->span, NULL);
                }
            }

            if (consume(&tok, tok, "*")) {
                not_implemented(&tok->span, NULL);
            } else {
                int array_size = -1;
                Node *node = assign(&tok, tok, env);
                if (node) {
                    eval_constexpr(node, &array_size);
                }
                retval->array_size = array_size;
            }
        }
        expect(&tok, tok, "]");
    } else {
        free(retval);
        return NULL;
    }

    *rest = tok;
    return retval;
}

//
//  direct_declarator =
//      identifier
//      "(" declarator ")"
//      direct_declarator direct_declarator_post
//
static DirectDeclarator *
direct_declarator(const Token **rest, const Token *tok, Env *env) {
    DirectDeclarator *retval = calloc(1, sizeof(DirectDeclarator));

    if (consume(&tok, tok, "(")) {
        const Declarator *decl = declarator(&tok, tok, env);
        if (decl == NULL) {
            return NULL;
        }
        retval->declarator = decl;
        expect(&tok, tok, ")");
    } else {
        const Token *ident = consume_ident(&tok, tok);
        if (ident == NULL) {
            return NULL;
        }
        retval->ident = char_from_span(&ident->span);
    }

    ArrayFunDecl *last = direct_declarator_post(&tok, tok, env);
    retval->arr_fun_decl = last;
    while (last) {
        ArrayFunDecl *next = direct_declarator_post(&tok, tok, env);
        last->next = next;
        last = next;
    }

    *rest = tok;
    return retval;
}

//
// declarator = pointer? direct_declarator
//
static const Declarator *
declarator(const Token **rest, const Token *tok, Env *env) {
    const PtrList *ptr = pointer(&tok, tok);
    const DirectDeclarator *direct_decl = direct_declarator(&tok, tok, env);
    if (direct_decl == NULL) {
        return NULL;
    }

    Declarator *retval = malloc(sizeof(Declarator));
    retval->ptr = ptr;
    retval->direct_decl = direct_decl;

    *rest = tok;
    return retval;
}

static Var *
declarator_to_var(const Type *ty, const Declarator *decl) {
    if (ty == NULL || decl == NULL) {
        return NULL;
    }
    ty = generate_ptr_type(decl->ptr, ty);

    const DirectDeclarator *direct_decl = decl->direct_decl;
    const ArrayFunDecl *arr_fun_decl = direct_decl->arr_fun_decl;
    while (arr_fun_decl) {
        if (arr_fun_decl->is_func) {
            ty = mk_func(ty, arr_fun_decl->args);
        } else {
            ty = mk_array(ty, arr_fun_decl->array_size);
        }
        arr_fun_decl = arr_fun_decl->next;
    }

    if (direct_decl->ident) {
        Var *retval = calloc(1, sizeof(Var));
        retval->ty = ty;
        retval->ident = direct_decl->ident;

        return retval;
    }

    return declarator_to_var(ty, direct_decl->declarator);
}

//
//  type_name = specifier_qualifier_list abstract_declarator?
//
static const Type *
type_name(const Token **rest, const Token *tok, Env *env) {
    const Type *ty = spec_qual_list(&tok, tok, env);
    if (ty == NULL) {
        return NULL;
    }

    const Type *t = abstract_declarator(&tok, tok, ty, env);
    if (t) {
        ty = t;
    }

    *rest = tok;
    return ty;
}

Node *
refer(Node *inner, const Type *ty) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_ADDR;
    node->lhs = inner;
    node->ty = mk_ptr(ty, TQ_NULL);
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
        node->ty = mk_ptr(&CHAR_T, TQ_NULL);

        const String *string = push_string(env, node->str);
        node->val = string->index;

        *rest = tok;
        return node;
    }

    if (consume(&tok, tok, "(")) {
        Node *node = expr(&tok, tok, env);
        if (node) {
            expect(&tok, tok, ")");
            *rest = tok;
            return node;
        }
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

Node *
new_cast(Node *node, const Type *ty) {
    Node *new = calloc(1, sizeof(Node));
    new->kind = ND_CAST;
    new->ty = ty;
    new->lhs = node;
    return new;
}

//
//  argument_expression_list? = ( assign ( "," assign )* )?
//
static NodeList *
argexprlist(const Token **rest, const Token *tok, Env *env,
            const Vars *params) {
    Node *node = assign(&tok, tok, env);
    if (!node) {
        return NULL;
    }

    if (!is_same_type(node->ty, params->var->ty)) {
        node = new_cast(node, params->var->ty);
    }
    params = params->next;

    NodeList *list = calloc(1, sizeof(NodeList));
    list->node = node;

    while (consume(&tok, tok, ",")) {
        node = assign(&tok, tok, env);
        if (node == NULL) {
            error("Invalid argument");
        }

        if (!is_same_type(node->ty, params->var->ty)) {
            node = new_cast(node, params->var->ty);
        }
        params = params->next;

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
    if (var->ty->ty != STRUCT && var->ty->ty != UNION) {
        error("Not struct or union");
    }

    const char *ident = char_from_span(&ident_token->span);
    const Vars *members = var->ty->struct_ty.members;

    const Var *m = NULL;
    while (members) {
        if (strcmp(members->var->ident, ident) == 0) {
            m = members->var;
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
    if (node == NULL) {
        return NULL;
    }
    for (;;) {
        if (consume(&tok, tok, "[")) {
            Node *index = expr(&tok, tok, env);
            expect(&tok, tok, "]");
            *rest = tok;
            node = deref_offset_ptr(as_ptr(node), index);
            continue;
        }
        if (consume(&tok, tok, "(")) {
            if (node->ty->ty != FUNCTION) {
                return NULL;
            }
            assert(node->ty->ty == FUNCTION);
            if (node->kind == ND_LVAR) {
                node->fn = node->var->ident;
            }
            node->kind = ND_CALL;
            node->args = argexprlist(&tok, tok, env, node->ty->args);
            node->ty = node->ty->retty;
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

static const Type *
cast(const Token **rest, const Token *tok, Env *env) {
    if (!consume(&tok, tok, "(")) {
        return NULL;
    }

    const Type *ty = type_name(&tok, tok, env);
    if (ty == NULL) {
        return NULL;
    }

    expect(rest, tok, ")");
    return ty;
}

//
//  cast_expression = ( "(" type_name ")" )* unary_expression
//
static Node *
cast_expression(const Token **rest, const Token *tok, Env *env) {
    Node *top = NULL;
    Node *current = NULL;
    for (;;) {
        const Type *ty = cast(&tok, tok, env);
        if (ty) {
            Node *node = calloc(1, sizeof(Node));
            node->kind = ND_CAST;
            node->ty = ty;
            if (current) {
                current->lhs = node;
                current = current->lhs;
            } else {
                top = node;
                current = top;
            }
        } else {
            break;
        }
    }
    Node *val = unary(rest, tok, env);
    if (val == NULL) {
        return NULL;
    }
    if (current) {
        current->lhs = val;
    } else {
        top = val;
    }
    return top;
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
        return cast_expression(rest, tok, env);
    }
    if (consume(&tok, tok, "-")) {
        return new_node(ND_SUB, new_node_num(0),
                        cast_expression(rest, tok, env));
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
//  multiplicative_expression =
//          cast_expression ( ( "*" | "/" | "%" ) cast_expression )*
//
static Node *
mul(const Token **rest, const Token *tok, Env *env) {
    Node *node = cast_expression(&tok, tok, env);

    for (;;) {
        if (consume(&tok, tok, "*")) {
            node = new_node(ND_MUL, node, cast_expression(&tok, tok, env));
        } else if (consume(&tok, tok, "/")) {
            node = new_node(ND_DIV, node, cast_expression(&tok, tok, env));
        } else {
            *rest = tok;
            return node;
        }
    }
}

//
//  additive_expression = multiplicative_expression
//          ( ( "+" | "-" ) multiplicative_expression )*
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
//  shift_expression =
//          additive_expression ( ( "<<" | ">>" ) additive_expression )*
//
static Node *
shift_expression(const Token **rest, const Token *tok, Env *env) {
    Node *node = add(&tok, tok, env);
    for (;;) {
        NodeKind kind;
        if (consume(&tok, tok, "<<")) {
            kind = ND_SHL;
        } else if (consume(&tok, tok, ">>")) {
            kind = ND_SHR;
        } else {
            break;
        }

        Node *rhs = add(&tok, tok, env);
        if (rhs == NULL) {
            return NULL;
        }

        Node *new = calloc(1, sizeof(Node));
        new->kind = kind;
        new->ty = node->ty;
        new->lhs = node;
        new->rhs = rhs;
        node = new;
    }

    *rest = tok;
    return node;
}

//
//  relational_expression =
//          shift_expression ( ( "<" | ">" | "<=" | ">=" ) shift_expression )*
//
static Node *
relational(const Token **rest, const Token *tok, Env *env) {
    Node *node = shift_expression(&tok, tok, env);

    for (;;) {
        if (consume(&tok, tok, "<")) {
            Node *rhs = shift_expression(&tok, tok, env);
            if (!is_same_type(node->ty, rhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LT, node, rhs);
        } else if (consume(&tok, tok, "<=")) {
            Node *rhs = shift_expression(&tok, tok, env);
            if (!is_same_type(node->ty, rhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LE, node, rhs);
        } else if (consume(&tok, tok, ">")) {
            Node *lhs = shift_expression(&tok, tok, env);
            if (!is_same_type(node->ty, lhs->ty))
                error("Not supported: compare between different types");
            node = new_node(ND_LT, lhs, node);
        } else if (consume(&tok, tok, ">=")) {
            Node *lhs = shift_expression(&tok, tok, env);
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
//  equality_expression = relational ( ( "==" | "!=" ) relational )*
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
//  and_expression =
//          equality_expression ( "&" equality_expression )*
//
static Node *
and_expression(const Token **rest, const Token *tok, Env *env) {
    Node *node = equality(&tok, tok, env);
    if (node == NULL) {
        return NULL;
    }

    while (consume(&tok, tok, "&")) {
        node = new_node(ND_AND, node, equality(&tok, tok, env));
    }

    *rest = tok;
    return node;
}

//
//  exclusive_or_expression =
//          and_expression ( "^" and_expression )*
//
static Node *
exclusive_or_expression(const Token **rest, const Token *tok, Env *env) {
    Node *node = and_expression(&tok, tok, env);
    if (node == NULL) {
        return NULL;
    }

    while (consume(&tok, tok, "^")) {
        node = new_node(ND_XOR, node, and_expression(&tok, tok, env));
    }

    *rest = tok;
    return node;
}

//
//  inclusive_or_expression =
//          exclusive_or_expression ( "|" exclusive_or_expression )*
//
static Node *
inclusive_or_expression(const Token **rest, const Token *tok, Env *env) {
    Node *node = exclusive_or_expression(&tok, tok, env);
    if (node == NULL) {
        return NULL;
    }

    while (consume(&tok, tok, "|")) {
        node = new_node(ND_OR, node, exclusive_or_expression(&tok, tok, env));
    }

    *rest = tok;
    return node;
}

//
//  logical_and_expression =
//          inclusive_or_expression ( "&&" inclusive_or_expression )*
//
static Node *
logical_and_expression(const Token **rest, const Token *tok, Env *env) {
    Node *node = inclusive_or_expression(&tok, tok, env);
    if (node == NULL) {
        return NULL;
    }

    while (consume(&tok, tok, "&&")) {
        node = new_node(ND_LAND, node, inclusive_or_expression(&tok, tok, env));
    }

    *rest = tok;
    return node;
}

//
//  logical_or_expression =
//          logical_and_expression ( "||" logical_and_expression )*
//
static Node *
logical_or_expression(const Token **rest, const Token *tok, Env *env) {
    Node *node = logical_and_expression(&tok, tok, env);
    if (node == NULL) {
        return NULL;
    }

    while (consume(&tok, tok, "||")) {
        node = new_node(ND_LOR, node, logical_and_expression(&tok, tok, env));
    }

    *rest = tok;
    return node;
}

//
//  conditional_expression =
//          logical_or_expression
//          logical_or_expression "?" expression ":" conditional_expression
//
static Node *
conditional_expression(const Token **rest, const Token *tok, Env *env) {
    Node *node = logical_or_expression(&tok, tok, env);
    if (node == NULL) {
        return NULL;
    }

    if (consume(&tok, tok, "?")) {
        Node *lhs = expr(&tok, tok, env);
        expect(&tok, tok, ":");
        Node *rhs = conditional_expression(&tok, tok, env);
        Node *cond = node;
        node = new_node(ND_TERNARY, lhs, rhs);
        node->cond = cond;
        node->jump_index = make_jump_scope(env).jump_index;
    }

    *rest = tok;
    return node;
}

static Node *
unary_and_assign(const Token **rest, const Token *tok, Env *env) {
    Node *node = unary(&tok, tok, env);
    if (node == NULL) {
        return NULL;
    }

    if (!consume(&tok, tok, "=")) {
        return NULL;
    }

    *rest = tok;
    return node;
}

//
//  assignment_expression =
//          conditional_expression
//          unary_expression assignment_operator assignment_expression
//
static Node *
assign(const Token **rest, const Token *tok, Env *env) {
    Node *lhs = unary_and_assign(&tok, tok, env);
    if (lhs) {
        Node *rhs = assign(&tok, tok, env);
        if (rhs == NULL) {
            return NULL;
        }

        *rest = tok;
        return new_node(ND_ASSIGN, lhs, as_ptr(rhs));
    }

    return conditional_expression(rest, tok, env);
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

    const Declarator *decl = declarator(&tok, tok, parent);
    Var *var = declarator_to_var(ty, decl);
    if (var == NULL || var->ty->ty != FUNCTION) {
        return NULL;
    }

    Function *fn = calloc(1, sizeof(Function));
    fn->def = var;

    Env env = make_scope(parent);
    const Vars *arg = fn->def->ty->args;
    while (arg) {
        Var *var = arg->var;

        if (var == NULL) { // in the case of "..."
            // TODO
            break;
        }
        if (var->ty == &VOID_T) {
            if (arg->next != NULL || fn->num_args > 0) {
                error_at(&tok->span, "void is allowed only for empty argument");
            }
            break;
        }

        //
        // In the case that function is declared without body.
        //
        if (var->ident == NULL) {
            // TODO
        } else if (!declare_arg(&env, var)) {
            error_at(&tok->span, "'%s' is already declared", var->ident);
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

    declare_fn(parent, var);

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

    Declaration *decl = calloc(1, sizeof(Declaration));

    const Declarator *d = declarator(&tok, tok, env);
    Var *var = declarator_to_var(spec.ty, d);
    if (var == NULL) {
        expect(&tok, tok, ";");

        declare_tag(env, spec.ty);
        *rest = tok;
        return decl;
    }

    // typedef
    if (spec.storage == TYPEDEF) {
        expect(&tok, tok, ";");
        declare_typedef(env, var);
        *rest = tok;
        return decl;
    }

    if (consume(&tok, tok, "=")) {
        const Initializer *init = initializer(&tok, tok, env);
        if (init == NULL) {
            return NULL;
        }
        if (var->ty->ty == ARRAY && var->ty->array_size == -1) {
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
            var->ty = mk_array(var->ty->ptr_to, array_size);
        }
        decl->init = init;
    }
    expect(&tok, tok, ";");

    if (!declare_var(env, var)) {
        error_at(&tok->span, "'%s' is already declared", var->ident);
    }

    decl->var = var;

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
const Unit *
program(const Token *token, Env *env) {
    Unit *code = NULL;
    Unit *current = NULL;

    while (!at_eof(token)) {
        const Function *fn = function(&token, token, env);
        if (fn) {
            if (current) {
                current->next = calloc(1, sizeof(Unit));
                current = current->next;
            } else {
                code = calloc(1, sizeof(sizeof(Unit)));
                current = code;
            }
            current->function = fn;
            continue;
        }

        const Declaration *decl = declaration(&token, token, env);
        if (decl) {
            if (decl->var) {
                if (current) {
                    current->next = calloc(1, sizeof(Unit));
                    current = current->next;
                } else {
                    code = calloc(1, sizeof(sizeof(Unit)));
                    current = code;
                }
                current->declaration = decl;
            }
            continue;
        }

        error_at(&token->span, "Cannot parse the program.");
    }

    return code;
}
