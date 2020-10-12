#include "mycc.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

LVar *get_lvar(Env *env, Token *tok) {
    for (LVar *var = env->locals; var; var = var->next)
        if (var->len == tok->len && !memcmp(tok->str, var->name, var->len))
            return var;

    error_at(tok->str, tok->len, "'%.*s' is undeclared", tok->len, tok->str);
}

LVar *declare_lvar(Env *env, Token *tok) {
    for (LVar *var = env->locals; var; var = var->next)
        if (var->len == tok->len && !memcmp(tok->str, var->name, var->len))
            error_at(tok->str, tok->len, "'%.*s' is already declared", tok->len, tok->str);

    LVar *new = calloc(1, sizeof(LVar));
    new->next = env->locals;
    new->name = tok->str;
    new->len = tok->len;

    env->maximum_offset += 8;
    new->offset = env->maximum_offset;

    env->locals = new;

    return new;
}
