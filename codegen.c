#include "mycc.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

int label_index;

void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

void push(char *arg) { printf("  push %s\n", arg); }

void push_val(int val) { printf("  push %d\n", val); }

void pop(char *arg) { printf("  pop %s\n", arg); }

void gen_lval(Node *node) {
    if (node->kind != ND_LVAR)
        error("lvalue is not a varialbe");

    printf("  mov rax, rbp\n");
    printf("  sub rax, %d\n", node->offset);
    push("rax");
}

void gen_if_body(Node *node, int l_index) {
    if (node->kind != ND_IF_BODY)
        error("Expected ND_IF_BODY");

    if (node->rhs) {
        printf("  je .Lelse%d\n", l_index);
        gen(node->lhs);
        printf("  jmp .Lend%d\n", l_index);
        printf(".Lelse%d:\n", l_index);
        gen(node->rhs);
    } else {
        printf("  je .Lend%d\n", l_index);
        gen(node->lhs);
    }

    printf(".Lend%d:\n", l_index);
    push_val(0); // if block returns zero.
}

void gen_while(Node *node, int l_index) {
    printf(".Lbegin%d:\n", l_index);
    gen(node->lhs);
    pop("rax");
    printf("  cmp rax, 0\n");
    printf("  je .Lend%d\n", l_index);
    gen(node->rhs);
    printf("  jmp .Lbegin%d\n", l_index);
    printf(".Lend%d:\n", l_index);
    push_val(0); // while loop returns zero.
}

void gen_for(Node *node, int l_index) {
    if (node->lhs)
        gen(node->lhs);
    printf(".Lbegin%d:\n", l_index);

    node = node->rhs;
    if (node->kind != ND_FOR_COND)
        error("Expected ND_FOR_COND");
    if (node->lhs) {
        gen(node->lhs);
        pop("rax");
        printf("  cmp rax, 0\n");
        printf("  je .Lend%d\n", l_index);
    }

    node = node->rhs;
    if (node->kind != ND_FOR_BODY)
        error("Expected ND_FOR_BODY");
    gen(node->rhs);
    if (node->lhs)
        gen(node->lhs);
    printf("  jmp .Lbegin%d\n", l_index);
    printf(".Lend%d:\n", l_index);
    push_val(0); // for loop returns zero.
}

void gen_block(Node *node) {
    while (node->lhs) {
        gen(node->lhs);
        node = node->rhs;
        if (!node || node->kind != ND_BLOCK)
            error("Expected ND_BLOCK");
    }
    push_val(0); // block returns zero.
}

typedef struct NodeList NodeList;
struct NodeList {
    Node *node;
    NodeList *next;
};

char *arg_registers[6] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};

// TODO: alignment RSP
void gen_call_args(Node *node) {
    int num_args = 0;
    NodeList *args;
    while (node) {
        if (node->kind != ND_ARGS)
            error("Expected ND_ARGS");

        NodeList *tmp = calloc(1, sizeof(NodeList));
        tmp->node = node->lhs;
        tmp->next = args;

        args = tmp;
        node = node->rhs;

        num_args++;
    }

    while (args) {
        gen(args->node);
        args = args->next;
    }

    for (int i = 0; i < num_args && i < 6; ++i) {
        pop(arg_registers[i]);
    }
}

void gen(Node *node) {
    switch (node->kind) {
    case ND_NUM:
        push_val(node->val);
        return;
    case ND_LVAR:
        gen_lval(node);
        pop("rax");
        printf("  mov rax, [rax]\n");
        push("rax");
        return;
    case ND_ASSIGN:
        gen_lval(node->lhs);
        gen(node->rhs);

        pop("rdi");
        pop("rax");
        printf("  mov [rax], rdi\n");
        push("rdi");
        return;
    case ND_IF_COND:
        gen(node->lhs);
        pop("rax");
        printf("  cmp rax, 0\n");
        gen_if_body(node->rhs, label_index++);
        return;
    case ND_WHILE:
        gen_while(node, label_index++);
        return;
    case ND_FOR_INIT:
        gen_for(node, label_index++);
        return;
    case ND_RETURN:
        gen(node->lhs);
        pop("rax");
        printf("  mov rsp, rbp\n");
        pop("rbp");
        printf("  ret\n");
        return;
    case ND_BLOCK:
        gen_block(node);
        return;
    case ND_CALL:
        gen_call_args(node->lhs);
        printf("  call %.*s\n", node->len, node->func);
        push_val(0);
        return;
    case ND_FUNC:
        debug("function definition here");
        return;
    }

    gen(node->lhs);
    gen(node->rhs);

    pop("rdi");
    pop("rax");

    switch (node->kind) {
    case ND_ADD:
        printf("  add rax, rdi\n");
        break;
    case ND_SUB:
        printf("  sub rax, rdi\n");
        break;
    case ND_MUL:
        printf("  imul rax, rdi\n");
        break;
    case ND_DIV:
        printf("  cqo\n");
        printf("  idiv rdi\n");
        break;
    case ND_LT:
        printf("  cmp rax, rdi\n");
        printf("  setl al\n");
        printf("  movzb rax, al\n");
        break;
    case ND_LE:
        printf("  cmp rax, rdi\n");
        printf("  setle al\n");
        printf("  movzb rax, al\n");
        break;
    case ND_EQ:
        printf("  cmp rax, rdi\n");
        printf("  sete al\n");
        printf("  movzb rax, al\n");
        break;
    case ND_NE:
        printf("  cmp rax, rdi\n");
        printf("  setne al\n");
        printf("  movzb rax, al\n");
        break;
    }

    push("rax");
}
