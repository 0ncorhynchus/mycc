#include "mycc.h"
#include <stdio.h>

int
main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Invalid number of arguments\n");
        return 1;
    }

    const char *path = argv[1];
    const Token *token = tokenize(path);

    Env global = init_env();
    const Unit *code = program(token, &global);

    printf(".intel_syntax noprefix\n");
    gen_strings(&global);
    while (code) {
        printf("\n");
        gen_top(code);
        code = code->next;
    }

    return 0;
}
