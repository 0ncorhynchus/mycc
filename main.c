#include "mycc.h"
#include <stdio.h>

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Invalid number of arguments\n");
        return 1;
    }

    Node *code[100];

    user_input = argv[1];
    tokenize(user_input);

    Env global = init_env();
    program(&global, code);

    printf(".intel_syntax noprefix\n");
    gen_strings(&global);
    for (int i = 0; code[i]; i++) {
        printf("\n");
        gen_top(code[i]);
    }

    return 0;
}
