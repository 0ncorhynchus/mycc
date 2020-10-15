#include "mycc.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

bool is_global(const Env *env) { return env->parent == NULL; }

size_t sizeof_ty(Type *ty) {
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
    default:
        error("The size of %s is unknown.", type_to_str(ty));
        return 0;
    }
}

Var *find_var(Env *env, const Span *ident) {
    for (Var *var = env->locals; var; var = var->next)
        if (var->ident.len == ident->len &&
            !memcmp(ident->ptr, var->ident.ptr, var->ident.len))
            return var;
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

const Var *declare_var(Env *env, Type *ty, const Span *ident) {
    if (find_var(env, ident))
        error_at(ident, "'%.*s' is already declared", ident->len, ident->ptr);

    Var *new = calloc(1, sizeof(Var));
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
