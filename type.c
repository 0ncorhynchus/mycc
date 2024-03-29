#include "mycc.h"
#include <stdio.h>
#include <string.h>

const Type VOID_T = {VOID};

const Type UNSIGNED_CHAR_T = {INTEGER, {CHAR, true}};
const Type SIGNED_CHAR_T = {INTEGER, {CHAR, false}};
const Type CHAR_T = SIGNED_CHAR_T;

const Type SHORT_T = {INTEGER, {SHORT, false}};
const Type USHORT_T = {INTEGER, {SHORT, true}};
const Type INT_T = {INTEGER, {INT, false}};
const Type UINT_T = {INTEGER, {INT, true}};
const Type LONG_T = {INTEGER, {LONG, false}};
const Type ULONG_T = {INTEGER, {LONG, true}};
const Type LONG_LONG_T = {INTEGER, {LONG_LONG, false}};
const Type ULONG_LONG_T = {INTEGER, {LONG_LONG, true}};

const Type FLOAT_T = {REAL, {0, false}, FLOAT};
const Type DOUBLE_T = {REAL, {0, false}, DOUBLE};
const Type LONG_DOUBLE_T = {REAL, {0, false}, LONG_DOUBLE};

const Type BOOL_T = {BOOL};

const Type *
mk_ptr(const Type *base, int qualifier) {
    Type *ptr = calloc(1, sizeof(Type));
    ptr->ty = PTR;
    ptr->ptr_to = base;
    return ptr;
}

const Type *
mk_array(const Type *base, int array_size) {
    Type *array = calloc(1, sizeof(Type));
    array->ty = ARRAY;
    array->ptr_to = base;
    array->array_size = array_size;
    return array;
}

const Type *
mk_func(const Type *retty, const Vars *args) {
    Type *func = calloc(1, sizeof(Type));
    func->ty = FUNCTION;
    func->retty = retty;
    func->args = args;
    return func;
}

size_t
sizeof_ty(const Type *ty) {
    if (ty == NULL) {
        error(
            "Internal compile error: try to obtain the size of unknown type.");
    }

    switch (ty->ty) {
    case INTEGER:
        switch (ty->integer.kind) {
        case CHAR:
            return 1;
        case SHORT:
            return 2;
        case INT:
            return 4;
        case LONG:
        case LONG_LONG:
            return 8;
        }
    case PTR:
        return 8;
    case ARRAY:
        return sizeof_ty(ty->ptr_to) * ty->array_size;
    case VOID:
        error("sizeof(void) is not allowed.");
        return 1; // GNU compatible.
    case ENUM:
        return sizeof_ty(&INT_T);
    case STRUCT:
        if (ty->struct_ty.size == 0) {
            error("storage size of struct '%s' is not known",
                  ty->struct_ty.tag);
        }
        return ty->struct_ty.size;
    case UNION:
        if (ty->struct_ty.size == 0) {
            error("storage size of union '%s' is not known", ty->struct_ty.tag);
        }
        return ty->struct_ty.size;
    case BOOL:
        return 1;
    case REAL:
        switch (ty->fkind) {
        case FLOAT:
            return 4;
        case DOUBLE:
            return 8;
        case LONG_DOUBLE:
            return 16;
        }
    default:
        error("The size of '%s' is unknown.", type_to_str(ty));
        return 0;
    }
}

static const char *
revstr(const char *str) {
    int len = strlen(str);
    char *reversed = calloc(len + 1, 1);
    for (int i = 0; i < len; i++)
        reversed[i] = str[len - i - 1];
    return reversed;
}

static const char *
function_to_revstr(const Type *ty) {
    char *buffer = calloc(256, 1);

    strcat(buffer, type_to_str(ty->retty));
    strcat(buffer, "(");
    const Vars *args = ty->args;
    if (args) {
        strcat(buffer, type_to_str(args->var->ty));
        for (args = args->next; args; args = args->next) {
            strcat(buffer, ", ");
            if (args->var) {
                strcat(buffer, type_to_str(args->var->ty));
            } else {
                strcat(buffer, "...");
            }
        }
    }
    strcat(buffer, ")");

    const char *retval = revstr(buffer);
    free(buffer);
    return retval;
}

const char *
type_to_str(const Type *ty) {
    char *buffer = calloc(256, 1);
    char *c;

    const Type *tmp;
    for (tmp = ty; tmp; tmp = tmp->ptr_to) {
        switch (tmp->ty) {
        case INTEGER:
            switch (tmp->integer.kind) {
            case CHAR:
                strcat(buffer, "rahc");
                break;
            case SHORT:
                strcat(buffer, "trohs");
                break;
            case INT:
                strcat(buffer, "tni");
                break;
            case LONG:
                strcat(buffer, "gnol");
                break;
            case LONG_LONG:
                strcat(buffer, "gnol gnol");
                break;
            }
            if (tmp->integer.is_unsigned) {
                strcat(buffer, " dengisnu");
            }
            break;
        case PTR:
            strcat(buffer, "*");
            break;
        case ARRAY:
            strcat(buffer, "]");
            if (tmp->array_size == 0) {
                strcat(buffer, "0");
            } else {
                for (int i = tmp->array_size; i > 0; i /= 10) {
                    char c = '0' + (i % 10);
                    strncat(buffer, &c, 1);
                }
            }
            strcat(buffer, "[");
            break;
        case VOID:
            strcat(buffer, "diov");
            break;
        case ENUM:
            if (tmp->enum_ty.tag) {
                c = calloc(6 + strlen(tmp->enum_ty.tag), 1);
                sprintf(c, "enum %s", tmp->enum_ty.tag);
                strcat(buffer, revstr(c));
                free(c);
            } else {
                strcat(buffer, "mune");
            }
            break;
        case STRUCT:
            if (tmp->struct_ty.tag) {
                c = calloc(8 + strlen(tmp->struct_ty.tag), 1);
                sprintf(c, "struct %s", tmp->struct_ty.tag);
                strcat(buffer, revstr(c));
                free(c);
            } else {
                strcat(buffer, "tcurts");
            }
            break;
        case UNION:
            if (tmp->struct_ty.tag) {
                c = calloc(7 + strlen(tmp->struct_ty.tag), 1);
                sprintf(c, "union %s", tmp->struct_ty.tag);
                strcat(buffer, revstr(c));
                free(c);
            } else {
                strcat(buffer, "noinu");
            }
            break;
        case FUNCTION:
            /* strcat(buffer, "noitcnuf"); */
            strcat(buffer, function_to_revstr(tmp));
            break;
        case BOOL:
            strcat(buffer, "looB_");
            break;
        case REAL:
            switch (tmp->fkind) {
            case FLOAT:
                strcat(buffer, "taolf");
                break;
            case DOUBLE:
                strcat(buffer, "elbuod");
                break;
            case LONG_DOUBLE:
                strcat(buffer, "elbuod gnol");
                break;
            }
            break;
        default:
            error("Not implemented for this type: type_to_str()");
        }
    }

    const char *retval = revstr(buffer);
    free(buffer);

    return retval;
}

static bool
is_subtype(const Type *base, const Type *derived) {
    if (base == NULL || derived == NULL) {
        return false;
    }

    if (base == derived) {
        return true;
    }

    if (base->ty == ENUM) {
        return is_subtype(&INT_T, derived);
    }
    if (derived->ty == ENUM) {
        return is_subtype(base, &INT_T);
    }

    const Vars *barg, *darg;

    switch (base->ty) {
    case INTEGER:
        if (derived->ty == INTEGER) {
            size_t base_size = sizeof_ty(base);
            size_t derived_size = sizeof_ty(derived);
            if (base_size > derived_size) {
                return true;
            } else if (base_size == derived_size) {
                if (base->integer.is_unsigned == derived->integer.is_unsigned) {
                    return true;
                } else {
                    return derived->integer.is_unsigned;
                }
            }
        }
        return false;
    case PTR:
        if (derived->ty == PTR) {
            return is_subtype(base->ptr_to, derived->ptr_to);
        }
        return derived->ty == INTEGER;
    case ARRAY:
        if (derived->ty == ARRAY && base->array_size == derived->array_size) {
            return is_subtype(base->ptr_to, derived->ptr_to);
        }
        return false;
    case FUNCTION:
        if (derived->ty != FUNCTION) {
            return false;
        }
        if (!is_subtype(base->retty, derived->retty)) {
            return false;
        }
        barg = base->args;
        darg = derived->args;
        while (barg && darg) {
            if (!is_subtype(darg->var->ty, barg->var->ty)) {
                return false;
            }
            barg = barg->next;
            darg = darg->next;
        }
        return (barg == NULL && darg == NULL);
    default:
        return false;
    }
}

// TODO
static const Type *
check_type(const Type *lhs, const Type *rhs) {
    if (is_subtype(lhs, rhs)) {
        return lhs;
    }

    if (is_subtype(rhs, lhs)) {
        return rhs;
    }

    return NULL;
}

bool
is_same_type(const Type *lhs, const Type *rhs) {
    return is_subtype(lhs, rhs) && is_subtype(rhs, lhs);
}

const Type *
get_type(const Node *lhs, const Node *rhs) {
    if (lhs == NULL || rhs == NULL)
        return NULL;
    const Type *ty = check_type(lhs->ty, rhs->ty);
    if (ty == NULL) {
        error("Type Mismatched: '%s' and '%s'", type_to_str(lhs->ty),
              type_to_str(rhs->ty));
    }
    return ty;
}
