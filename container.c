#include "mycc.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

bool is_global(const Env *env) { return env->parent == NULL; }

Var *find_var(Env *env, const Span *ident) {
    for (VarList *next = env->vars; next; next = next->next) {
        if (strlen(next->var.ident) == ident->len &&
            !memcmp(ident->ptr, next->var.ident, ident->len)) {
            return &next->var;
        }
    }
    return NULL;
}

const Var *get_var(Env *env, const Span *ident) {
    while (env) {
        Var *retval = find_var(env, ident);
        if (retval)
            return retval;
        env = env->parent;
    }

    error_at(ident, "'%.*s' is undeclared", ident->len, ident->ptr);

    return NULL;
}

const Var *declare_var(Env *env, const Type *ty, const Span *ident) {
    if (find_var(env, ident))
        error_at(ident, "'%.*s' is already declared", ident->len, ident->ptr);

    VarList *new = calloc(1, sizeof(VarList));
    new->next = env->vars;
    new->var.ty = ty;
    char *id = calloc(ident->len + 1, 1);
    memcpy(id, ident->ptr, ident->len);
    new->var.ident = id;

    if (is_global(env)) {
        new->var.kind = VGLOBAL;
    } else {
        new->var.kind = VLOCAL;
        size_t size = ((sizeof_ty(ty) - 1) / 8 + 1) * 8;
        env->maximum_offset += size;
        new->var.offset = env->maximum_offset;
    }

    env->vars = new;

    return &new->var;
}

Env *get_global(Env *env) {
    while (!is_global(env)) {
        env = env->parent;
    }
    return env;
}

const String *push_string(Env *env, const Span *ident) {
    Env *global = get_global(env);

    String *str = calloc(1, sizeof(String));
    str->next = global->strings;
    str->index = global->maximum_strings;
    str->ident = *ident;
    global->maximum_strings++;

    global->strings = str;

    return str;
}
