#include "mycc.h"
#include <string.h>

bool
is_global(const Env *env) {
    return env->parent == NULL;
}

static Var *
find_var(Env *env, const char *ident) {
    for (VarList *next = env->vars; next; next = next->next) {
        if (strcmp(ident, next->var->ident) == 0) {
            return next->var;
        }
    }
    return NULL;
}

const Var *
get_var(Env *env, const char *ident) {
    while (env) {
        Var *retval = find_var(env, ident);
        if (retval)
            return retval;
        env = env->parent;
    }
    error("'%s' is undeclared", ident);
    return NULL;
}

bool
declare_arg(Env *env, Var *var) {
    if (find_var(env, var->ident)) {
        return false;
    }

    if (is_global(env)) {
        error("Internal compiler error: %s:%d", __FILE__, __LINE__);
    } else {
        var->kind = VLOCAL;
        env->num_args++;
        size_t size = ((sizeof_ty(var->ty) - 1) / 8 + 1) * 8;
        if (env->num_args > 6) {
            env->maximum_arg_offset += size;
            var->offset = -env->maximum_arg_offset;
        } else {
            env->maximum_offset += size;
            var->offset = env->maximum_offset;
        }
    }

    VarList *new = calloc(1, sizeof(VarList));
    new->next = env->vars;
    new->var = var;
    env->vars = new;

    return true;
}

bool
declare_var(Env *env, Var *var) {
    if (find_var(env, var->ident)) {
        return false;
    }

    if (is_global(env)) {
        var->kind = VGLOBAL;
    } else {
        var->kind = VLOCAL;
        size_t size = ((sizeof_ty(var->ty) - 1) / 8 + 1) * 8;
        env->maximum_offset += size;
        var->offset = env->maximum_offset;
    }

    VarList *new = calloc(1, sizeof(VarList));
    new->next = env->vars;
    new->var = var;
    env->vars = new;

    return true;
}

Env *
get_global(Env *env) {
    while (!is_global(env)) {
        env = env->parent;
    }
    return env;
}

const String *
push_string(Env *env, const Span *ident) {
    Env *global = get_global(env);

    String *str = calloc(1, sizeof(String));
    str->next = global->strings;
    str->index = global->maximum_strings;
    str->ident = *ident;
    global->maximum_strings++;

    global->strings = str;

    return str;
}
