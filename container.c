#include "mycc.h"
#include <stddef.h>
#include <string.h>

LVar *locals;

LVar *find_lvar(Token *tok) {
    for (LVar *var = locals; var; var = var->next)
        if (var->len == tok->len && !memcmp(tok->str, var->name, var->len))
            return var;
    return NULL;
}
