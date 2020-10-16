#include "mycc.h"
#include <stdio.h>

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Invalid number of arguments\n");
        return 1;
    }

    Node *code[100];

    const char *path = argv[1];
    const Token *token = tokenize(path);

    Env global = init_env();
    program(token, &global, code);

    printf(".intel_syntax noprefix\n");
    gen_strings(&global);
    for (int i = 0; code[i]; i++) {
        printf("\n");
        gen_top(code[i]);
    }

    return 0;
}
