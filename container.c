#include "mycc.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

LVar *get_lvar(Env *env, Token *tok) {
    for (LVar *var = env->locals; var; var = var->next)
        if (var->len == tok->len && !memcmp(tok->str, var->name, var->len))
            return var;

    LVar *new = calloc(1, sizeof(LVar));
    new->next = env->locals;
    new->name = tok->str;
    new->len = tok->len;

    env->maximum_offset += 8;
    new->offset = env->maximum_offset;

    env->locals = new;

    return new;
}

FunList *retrieve_function(Node *node, FunList *fn) {
    if (node == NULL)
        return fn;

    if (node->kind == ND_FUNC) {
        fn->next = calloc(1, sizeof(FunList));
        fn->next->func = node;
        fn = fn->next;
    }

    fn = retrieve_function(node->lhs, fn);
    fn = retrieve_function(node->rhs, fn);

    return fn;
}

FunList *get_fun_list() {
    FunList *retval = calloc(1, sizeof(FunList));
    FunList *fn = retval;
    for (int i = 0; code[i]; i++) {
        fn = retrieve_function(code[i], fn);
    }
    if (retval->next)
        return retval->next;
    return NULL;
}
