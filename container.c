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
        if (var->len == tok->len && !memcmp(tok->str, var->name, var->len))
            return var;
    return NULL;
}

LVar *get_lvar(Env *env, Token *tok) {
    LVar *retval = find_lvar(env, tok);
    if (retval == NULL)
        error_at(tok->str, tok->len, "'%.*s' is undeclared", tok->len,
                 tok->str);
    return retval;
}

LVar *declare_lvar(Env *env, Type *ty, Token *tok) {
    if (find_lvar(env, tok))
        error_at(tok->str, tok->len, "'%.*s' is already declared", tok->len,
                 tok->str);

    LVar *new = calloc(1, sizeof(LVar));
    new->next = env->locals;
    new->ty = ty;
    new->name = tok->str;
    new->len = tok->len;

    size_t size = ((sizeof_ty(ty) - 1) / 8 + 1) * 8;
    env->maximum_offset += size;
    new->offset = env->maximum_offset;

    env->locals = new;

    return new;
}
