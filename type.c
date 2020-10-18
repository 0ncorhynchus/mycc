#include "mycc.h"
#include <stdio.h>
#include <string.h>

const Type INT_T = {INTEGER, INT};
const Type CHAR_T = {INTEGER, CHAR};
const Type VOID_T = {VOID};

const Type *
mk_ptr(const Type *base) {
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
mk_func(const Type *retty, const ParamList *args) {
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
        switch (ty->ikind) {
        case CHAR:
            return 1;
        case INT:
            return 4;
        }
    case PTR:
        return 8;
    case ARRAY:
        return sizeof_ty(ty->ptr_to) * ty->array_size;
    case VOID:
        error("sizeof(void) is not allowed.");
        return 1; // GNU compatible.
    case ENUM:
        return 4;
    default:
        error("The size of %s is unknown.", type_to_str(ty));
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

const char *
type_to_str(const Type *ty) {
    char *buffer = calloc(256, 1);
    char *c;

    const Type *tmp;
    for (tmp = ty; tmp; tmp = tmp->ptr_to) {
        switch (tmp->ty) {
        case INTEGER:
            switch (tmp->ikind) {
            case CHAR:
                strcat(buffer, "rahc");
                break;
            case INT:
                strcat(buffer, "tni");
                break;
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
            strcat(buffer, "doiv");
            break;
        case ENUM:
            c = calloc(5 + strlen(ty->enum_ty->tag), 1);
            sprintf(c, "enum %s", ty->enum_ty->tag);
            strcat(buffer, revstr(c));
            free(c);
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

    const ParamList *barg, *darg;

    switch (base->ty) {
    case INTEGER:
        return derived->ty == INTEGER && sizeof_ty(base) >= sizeof_ty(derived);
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
            if (!is_subtype(darg->decl->var->ty, barg->decl->var->ty)) {
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
