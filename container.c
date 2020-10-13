#include "mycc.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

bool is_global(const Env *env) { return env->parent == NULL; }

size_t sizeof_ty(Type *ty) {
    switch (ty->ty) {
    case INT:
        return 4;
    case PTR:
        return 8;
    case ARRAY:
        return sizeof_ty(ty->ptr_to) * ty->array_size;
    case CHAR:
        return 1;
    default:
        error("The size of %s is unknown.", type_to_str(ty));
    }
}

LVar *find_lvar(Env *env, const Span *ident) {
    for (LVar *var = env->locals; var; var = var->next)
        if (var->ident.len == ident->len &&
            !memcmp(ident->ptr, var->ident.ptr, var->ident.len))
            return var;
    return NULL;
}

const LVar *get_lvar(Env *env, const Span *ident) {
    while (env) {
        LVar *retval = find_lvar(env, ident);
        if (retval)
            return retval;
        env = env->parent;
    }
    error_at(ident, "'%.*s' is undeclared", ident->len, ident->ptr);
}

const LVar *declare_lvar(Env *env, Type *ty, const Span *ident) {
    if (find_lvar(env, ident))
        error_at(ident, "'%.*s' is already declared", ident->len, ident->ptr);

    LVar *new = calloc(1, sizeof(LVar));
    new->next = env->locals;
    new->ty = ty;
    new->ident = *ident;

    if (is_global(env)) {
        new->kind = VGLOBAL;
    } else {
        new->kind = VLOCAL;
        size_t size = ((sizeof_ty(ty) - 1) / 8 + 1) * 8;
        env->maximum_offset += size;
        new->offset = env->maximum_offset;
    }

    env->locals = new;

    return new;
}
