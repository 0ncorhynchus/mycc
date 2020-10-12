#include "mycc.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

LVar *locals;
int maximum_offset = 8;

LVar *find_lvar(Token *tok) {
    for (LVar *var = locals; var; var = var->next)
        if (var->len == tok->len && !memcmp(tok->str, var->name, var->len))
            return var;
    return NULL;
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
