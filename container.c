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

size_t
expand_for_align(size_t size) {
    return ((size - 1) / 8 + 1) * 8;
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
        size_t size = expand_for_align(sizeof_ty(var->ty));
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

static Env *
get_block_top(Env *env) {
    while (!is_global(env) && env->is_block_scope) {
        env = env->parent;
    }
    return env;
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
        size_t size = expand_for_align(sizeof_ty(var->ty));
        Env *top = get_block_top(env);
        top->maximum_offset += size;
        var->offset = top->maximum_offset;
    }

    VarList *new = calloc(1, sizeof(VarList));
    new->next = env->vars;
    new->var = var;
    env->vars = new;

    return true;
}

void
declare_fn(Env *env, Var *var) {
    Var *defined = find_var(env, var->ident);
    if (defined) {
        if (!is_same_type(var->ty, defined->ty)) {
            error("conflicting types for '%s'", var->ident);
        }
        if (defined->is_body_defined) {
            error("redefinition of '%s'", var->ident);
        }
        defined->is_body_defined = true;
    } else {
        VarList *new = calloc(1, sizeof(VarList));
        new->next = env->vars;
        new->var = var;
        env->vars = new;
    }
}

void
declare_typedef(Env *env, Var *var) {
    if (find_var(env, var->ident)) {
        error("'%s' is already defined.", var->ident);
    }

    VarList *new = calloc(1, sizeof(VarList));
    new->next = env->vars;
    new->var = var;
    new->is_typedef = true;
    env->vars = new;

    if (var->ty->enum_ty) {
        declare_tag(env, var->ty);
    }
}

static const Type *
find_typedef(const Env *env, const char *ident) {
    for (VarList *next = env->vars; next; next = next->next) {
        if (next->is_typedef && strcmp(ident, next->var->ident) == 0) {
            return next->var->ty;
        }
    }
    return NULL;
}

const Type *
get_typedef(const Env *env, const char *ident) {
    while (env) {
        const Type *retval = find_typedef(env, ident);
        if (retval)
            return retval;
        env = env->parent;
    }
    return NULL;
}

static void
declare_enum_const(Env *env, Var *var, int value) {
    if (find_var(env, var->ident)) {
        error("'%s' is already defined.", var->ident);
    }

    if (is_global(env)) {
        var->kind = VGLOBAL;
    } else {
        var->kind = VLOCAL;
    }

    var->is_const = true;
    var->enum_val = value;

    VarList *new = calloc(1, sizeof(VarList));
    new->next = env->vars;
    new->var = var;
    env->vars = new;
}

static const Type *
find_tag(const Env *env, const char *ident) {
    for (TagList *head = env->tags; head; head = head->next) {
        if (strcmp(ident, head->tag) == 0) {
            return head->ty;
        }
    }
    return NULL;
}

const Type *
get_tag(const Env *env, const char *tag) {
    while (env) {
        const Type *ty = find_tag(env, tag);
        if (ty) {
            return ty;
        }
        env = env->parent;
    }
    return NULL;
}

static bool
declare_enum(Env *env, const Type *ty) {
    if (ty->enum_ty->consts == NULL) {
        return false;
    }

    if (ty->enum_ty->tag) {
        if (find_tag(env, ty->enum_ty->tag)) {
            error("'%s' is already defined as a tag");
        }

        TagList *new = calloc(1, sizeof(TagList));
        new->next = env->tags;
        new->tag = ty->enum_ty->tag;
        new->ty = ty;
        env->tags = new;
    }

    const String *head = ty->enum_ty->consts;
    while (head) {
        Var *var = calloc(1, sizeof(Var));
        var->ty = ty;
        var->ident = head->ident;
        declare_enum_const(env, var, head->index);
        head = head->next;
    }

    return true;
}

static bool
declare_struct(Env *env, const Type *ty) {
    if (ty->struct_ty->tag) {
        if (find_tag(env, ty->struct_ty->tag)) {
            error("'%s' is already defined as a tag");
        }

        TagList *new = calloc(1, sizeof(TagList));
        new->tag = ty->struct_ty->tag;
        new->ty = ty;
        env->tags = new;
    }

    return true;
}

bool
declare_tag(Env *env, const Type *ty) {
    switch (ty->ty) {
    case ENUM:
        return declare_enum(env, ty);
    case STRUCT:
        return declare_struct(env, ty);
        break;
    default:
        error("Internal compiler error at %s:%d", __FILE__, __LINE__);
        break;
    }

    return false;
}

Env *
get_global(Env *env) {
    while (!is_global(env)) {
        env = env->parent;
    }
    return env;
}

const String *
push_string(Env *env, const char *ident) {
    Env *global = get_global(env);

    String *str = calloc(1, sizeof(String));
    str->next = global->strings;
    str->index = global->maximum_strings;
    str->ident = ident;
    global->maximum_strings++;

    global->strings = str;

    return str;
}
