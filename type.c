#include "mycc.h"
#include <string.h>

const Type INT_T = {INT, NULL, 0};
const Type CHAR_T = {CHAR, NULL, 0};
const Type VOID_T = {VOID, NULL, 0};

const Type *mk_ptr(const Type *base) {
    Type *ptr = calloc(1, sizeof(Type));
    ptr->ty = PTR;
    ptr->ptr_to = base;
    return ptr;
}

const Type *mk_array(const Type *base, int array_size) {
    Type *array = calloc(1, sizeof(Type));
    array->ty = ARRAY;
    array->ptr_to = base;
    array->array_size = array_size;
    return array;
}

size_t sizeof_ty(const Type *ty) {
    if (ty == NULL) {
        error(
            "Internal compile error: try to obtain the size of unknown type.");
    }

    switch (ty->ty) {
    case INT:
        return 4;
    case PTR:
        return 8;
    case ARRAY:
        return sizeof_ty(ty->ptr_to) * ty->array_size;
    case CHAR:
        return 1;
    case VOID:
        error("sizeof(void) is not allowed.");
        return 1; // GNU compatible.
    default:
        error("The size of %s is unknown.", type_to_str(ty));
        return 0;
    }
}

char *type_to_str(const Type *ty) {
    char *buffer = calloc(256, 1);

    const Type *tmp;
    for (tmp = ty; tmp; tmp = tmp->ptr_to) {
        switch (tmp->ty) {
        case INT:
            strcat(buffer, "tni");
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
        case CHAR:
            strcat(buffer, "rahc");
            break;
        case VOID:
            strcat(buffer, "doiv");
            break;
        default:
            error("Not implemented for this type: type_to_str()");
        }
    }

    int len = strlen(buffer);
    char *retval = calloc(len + 1, 1);
    for (int i = 0; i < len; i++) {
        retval[i] = buffer[len - i - 1];
    }
    free(buffer);

    return retval;
}

static bool is_subtype(const Type *base, const Type *derived) {
    if (base == NULL || derived == NULL) {
        return false;
    }

    switch (base->ty) {
    case INT:
        return derived->ty == INT || derived->ty == CHAR;
    case PTR:
        if (derived->ty == PTR) {
            return is_subtype(base->ptr_to, derived->ptr_to);
        }
        return derived->ty == INT || derived->ty == CHAR;
    case ARRAY:
        if (derived->ty == ARRAY && base->array_size == derived->array_size) {
            return is_subtype(base->ptr_to, derived->ptr_to);
        }
        return false;
    case CHAR:
        return derived->ty == CHAR;
    default:
        return false;
    }
}

static const Type *check_type(const Type *lhs, const Type *rhs) {
    if (is_subtype(lhs, rhs)) {
        return lhs;
    }

    if (is_subtype(rhs, lhs)) {
        return rhs;
    }

    return NULL;
}

bool is_same_type(const Type *lhs, const Type *rhs) {
    return is_subtype(lhs, rhs) && is_subtype(rhs, lhs);
}

const Type *get_type(const Node *lhs, const Node *rhs) {
    if (lhs == NULL || rhs == NULL)
        return NULL;
    const Type *ty = check_type(lhs->ty, rhs->ty);
    if (ty == NULL) {
        error("Type Mismatched: '%s' and '%s'", type_to_str(lhs->ty),
              type_to_str(rhs->ty));
    }
    return ty;
}
