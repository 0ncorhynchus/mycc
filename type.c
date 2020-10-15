#include "mycc.h"
#include <stdlib.h>
#include <string.h>

const Type INT_T = {INT, NULL, 0};
const Type CHAR_T = {CHAR, NULL, 0};
const Type VOID_T = {VOID, NULL, 0};

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
    int depth = 0;
    const Type *tmp;
    for (tmp = ty; tmp; tmp = tmp->ptr_to) {
        switch (tmp->ty) {
        case INT:
            depth += 3; // length of "int"
            break;
        case PTR:
            depth += 1; // length of "*"
            break;
        case ARRAY:
            if (tmp->array_size >= 0) {
                depth += snprintf(NULL, 0, "[%d]", tmp->array_size);
            }
            break;
        case CHAR:
            depth += 4; // length of "char"
            break;
        case VOID:
            depth += 4; // length of "void"
            break;
        default:
            error("Not implemented for this type: type_to_str()");
        }
    }

    char *buffer = calloc(depth + 1, sizeof(char));
    for (tmp = ty; tmp; tmp = tmp->ptr_to) {
        switch (tmp->ty) {
        case INT:
            depth -= 3; // length of "int"
            memcpy(buffer + depth, "int", 3);
            break;
        case PTR:
            depth -= 1; // length of "*"
            memcpy(buffer + depth, "*", 2);
            break;
        case ARRAY:
            if (tmp->array_size >= 0) {
                depth -= snprintf(NULL, 0, "[%d]", tmp->array_size);
                sprintf(buffer + depth, "[%d]", tmp->array_size);
            }
            break;
        case CHAR:
            depth -= 4; // length of "char"
            memcpy(buffer + depth, "char", 4);
            break;
        case VOID:
            depth -= 4;
            memcpy(buffer + depth, "void", 4);
            break;
        default:
            error("Not implemented for this type: type_to_str()");
        }
    }

    return buffer;
}
