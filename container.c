#include "mycc.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

size_t sizeof_ty(Type *ty) {
    switch (ty->ty) {
    case INT:
        return 4;
    case PTR:
        return 8;
    case ARRAY:
        return sizeof_ty(ty->ptr_to) * ty->array_size;
    }
}

LVar *find_lvar(Env *env, Token *tok) {
    for (LVar *var = env->locals; var; var = var->next)
        if (var->ident.len == tok->span.len &&
            !memcmp(tok->span.ptr, var->ident.ptr, var->ident.len))
            return var;
    return NULL;
}

LVar *get_lvar(Env *env, Token *tok) {
    LVar *retval = find_lvar(env, tok);
    if (retval)
        return retval;
    error_at(&tok->span, "'%.*s' is undeclared", tok->span.len, tok->span.ptr);
}

LVar *declare_lvar(Env *env, Type *ty, Token *tok) {
    if (find_lvar(env, tok))
        error_at(&tok->span, "'%.*s' is already declared", tok->span.len,
                 tok->span.ptr);

    LVar *new = calloc(1, sizeof(LVar));
    new->next = env->locals;
    new->ty = ty;
    new->ident = tok->span;

    size_t size = ((sizeof_ty(ty) - 1) / 8 + 1) * 8;
    env->maximum_offset += size;
    new->offset = env->maximum_offset;

    env->locals = new;

    return new;
}
