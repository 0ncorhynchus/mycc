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
    FunList *fn = get_fun_list();

    printf(".intel_syntax noprefix\n");
    printf(".global main\n");

    // generate functions
    FunList *tmp = fn;
    while (tmp) {
        gen_func(tmp->func);
        printf("\n");
        tmp = tmp->next;
    }

    printf("main:\n");
    // prologue
    printf("  push rbp\n");
    printf("  mov rbp, rsp\n");
    printf("  sub rsp, %d /* allocate for local variables */\n",
           maximum_offset);

    for (int i = 0; code[i]; i++) {
        gen(code[i]);
        // return the last value in stack, which is the result of the previous
        // code.
        printf("  pop rax\n");
    }

    // epilogue
    printf("  mov rsp, rbp\n");
    printf("  pop rbp\n");
    printf("  ret\n");
    return 0;
}
