#include "mycc.h"
#include <stdio.h>

char *user_input;
Token *token;
Node *code[100];

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Invalid number of arguments\n");
        return 1;
    }

    user_input = argv[1];
    token = tokenize(user_input);
    program();

    printf(".intel_syntax noprefix\n");
    for (int i = 0; code[i]; i++) {
        if (code[i]->kind != ND_FUNC)
            continue;
        printf(".global %.*s\n", code[i]->len, code[i]->func);
    }

    for (int i = 0; code[i]; i++) {
        gen_func(code[i]);
    }

    return 0;
}
