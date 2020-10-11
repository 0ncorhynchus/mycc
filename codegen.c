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

void gen_lval(Node *node) {
    if (node->kind != ND_LVAR)
        error("lvalue is not a varialbe");

    printf("  mov rax, rbp\n");
    printf("  sub rax, %d\n", node->offset);
    printf("  push rax\n");
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
    printf("  push 0\n"); // if block returns zero.
}

void gen_while(Node *node, int l_index) {
    printf(".Lbegin%d:\n", l_index);
    gen(node->lhs);
    printf("  pop rax\n");
    printf("  cmp rax, 0\n");
    printf("  je .Lend%d\n", l_index);
    gen(node->rhs);
    printf("  jmp .Lbegin%d\n", l_index);
    printf(".Lend%d:\n", l_index);
    printf("  push 0\n"); // while loop returns zero.
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
        printf("  pop rax\n");
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
    printf("  push 0\n"); // for loop returns zero.
}

void gen_block(Node *node) {
    while (node->lhs) {
        gen(node->lhs);
        node = node->rhs;
        if (!node || node->kind != ND_BLOCK)
            error("Expected ND_BLOCK");
    }
    printf("  push 0\n"); // block returns zero.
}

void gen(Node *node) {
    switch (node->kind) {
    case ND_NUM:
        printf("  push %d\n", node->val);
        return;
    case ND_LVAR:
        gen_lval(node);
        printf("  pop rax\n");
        printf("  mov rax, [rax]\n");
        printf("  push rax\n");
        return;
    case ND_ASSIGN:
        gen_lval(node->lhs);
        gen(node->rhs);

        printf("  pop rdi\n");
        printf("  pop rax\n");
        printf("  mov [rax], rdi\n");
        printf("  push rdi\n");
        return;
    case ND_IF_COND:
        gen(node->lhs);
        printf("  pop rax\n");
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
        printf("  pop rax\n");
        printf("  mov rsp, rbp\n");
        printf("  pop rbp\n");
        printf("  ret\n");
        return;
    case ND_BLOCK:
        gen_block(node);
        return;
    }

    gen(node->lhs);
    gen(node->rhs);

    printf("  pop rdi\n");
    printf("  pop rax\n");

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

    printf("  push rax\n");
}
