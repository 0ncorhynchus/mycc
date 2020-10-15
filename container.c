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
    for (VarList *next = env->vars; next; next = next->next) {
        if (next->var.ident.len == ident->len &&
            !memcmp(ident->ptr, next->var.ident.ptr, ident->len)) {
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

const Var *declare_var(Env *env, Type *ty, const Span *ident) {
    if (find_var(env, ident))
        error_at(ident, "'%.*s' is already declared", ident->len, ident->ptr);

    VarList *new = calloc(1, sizeof(VarList));
    new->next = env->vars;
    new->var.ty = ty;
    new->var.ident = *ident;

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
