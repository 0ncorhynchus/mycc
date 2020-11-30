#include "mycc.h"
#include <string.h>

static int jump_index = 0;

Env
init_env() {
    return (Env){};
}

Env
make_scope(Env *parent) {
    Env env = {parent};
    env.maximum_arg_offset = 8;
    env.jump_index = parent->jump_index;
    return env;
}

Env
make_block_scope(Env *parent) {
    Env env = {parent};
    env.is_block_scope = true;
    env.jump_index = parent->jump_index;
    return env;
}

Env
make_jump_scope(Env *parent) {
    Env env = {parent};
    env.is_block_scope = true;
    env.jump_index = jump_index++;
    return env;
}

Env
make_switch_scope(Env *parent) {
    Env env = {parent};
    env.is_block_scope = true;
    env.is_switch_scope = true;
    env.jump_index = jump_index++;
    return env;
}

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

static VarList *
push_var(Env *env, Var *var) {
    VarList *new = calloc(1, sizeof(VarList));
    new->var = var;
    new->next = env->vars;
    env->vars = new;
    return new;
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

    push_var(env, var);

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

    push_var(env, var);

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
        push_var(env, var);
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

    push_var(env, var);
}

static Type *
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

static void
push_tag(Env *env, const char *tag, Type *ty) {
    TagList *new = calloc(1, sizeof(TagList));
    new->tag = tag;
    new->ty = ty;
    new->next = env->tags;
    env->tags = new;
}

static const Type *
declare_enum(Env *env, const Type *ty) {
    if (ty->enum_ty.tag) {
        Type *defined = find_tag(env, ty->enum_ty.tag);
        if (defined) {
            if (defined == ty) {
                return defined;
            }
            if (defined->ty != ENUM || defined->enum_ty.consts) {
                error("'%s' is already defined as a tag");
            }
            defined->enum_ty.consts = ty->enum_ty.consts;
        } else {
            defined = calloc(1, sizeof(Type));
            defined->ty = ty->ty;
            defined->enum_ty = ty->enum_ty;
            push_tag(env, ty->enum_ty.tag, defined);
        }
        ty = defined;
    }

    const String *head = ty->enum_ty.consts;
    while (head) {
        Var *var = calloc(1, sizeof(Var));
        var->ty = ty;
        var->ident = head->ident;
        declare_enum_const(env, var, head->index);
        head = head->next;
    }

    return ty;
}

static const Type *
declare_struct(Env *env, const Type *ty) {
    if (ty->struct_ty.tag == NULL) {
        return ty;
    }

    Type *declared = find_tag(env, ty->struct_ty.tag);
    if (declared) {
        if (declared == ty) {
            return declared;
        }
        if (declared->ty != ty->ty || declared->struct_ty.members) {
            error("'%s' is already defined as a tag");
        }
        declared->struct_ty.members = ty->struct_ty.members;
        declared->struct_ty.size = ty->struct_ty.size;
        return declared;
    }

    Type *new_type = calloc(1, sizeof(Type));
    new_type->ty = ty->ty;
    new_type->struct_ty = ty->struct_ty;

    push_tag(env, ty->struct_ty.tag, new_type);

    return new_type;
}

void
declare_tag(Env *env, const Type *ty) {
    switch (ty->ty) {
    case ENUM:
        declare_enum(env, ty);
        return;
    case STRUCT:
    case UNION:
        declare_struct(env, ty);
        return;
    default:
        error("Internal compiler error at %s:%d", __FILE__, __LINE__);
        break;
    }
}

void
declare_typedef(Env *env, const Var *var) {
    if (find_var(env, var->ident)) {
        error("'%s' is already defined.", var->ident);
    }

    Var *redefined = calloc(1, sizeof(Var));
    redefined->ident = var->ident;
    switch (var->ty->ty) {
    case ENUM:
        redefined->ty = declare_enum(env, var->ty);
        break;
    case STRUCT:
    case UNION:
        redefined->ty = declare_struct(env, var->ty);
        break;
    default:
        redefined->ty = var->ty;
    }

    VarList *new = push_var(env, redefined);
    new->is_typedef = true;
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

static bool
find_label(const Env *env, const Label *label) {
    const LabelList *labels = env->labels;
    while (labels) {
        switch (labels->label->kind) {
        case CASE:
            if (label->kind == CASE && labels->label->val == label->val) {
                return true;
            }
            break;
        case DEFAULT:
            if (label->kind == DEFAULT) {
                return true;
            }
            break;
        }
        labels = labels->next;
    }
    return false;
}

void
push_label(Env *env, const Label *label) {
    while (env && !env->is_switch_scope) {
        env = env->parent;
    }

    if (env == NULL) {
        error("Not within a switch statement");
    }

    if (find_label(env, label)) {
        error("Duplicated labels");
    }

    LabelList *new = calloc(1, sizeof(LabelList));
    new->label = label;
    new->next = env->labels;
    env->labels = new;
}
