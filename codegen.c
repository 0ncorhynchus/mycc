#include "mycc.h"
#include <assert.h>
#include <stdio.h>

static const char *arg_registers[6] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};

const char *ax(size_t size) {
    switch (size) {
    case 1:
        return "al";
    case 4:
        return "eax";
    case 8:
        return "rax";
    default:
        error("Not supported register size.");
        return NULL;
    }
}

const char *di(size_t size) {
    switch (size) {
    case 1:
        return "dil";
    case 4:
        return "edi";
    case 8:
        return "rdi";
    default:
        error("Not supported register size.");
        return NULL;
    }
}

static int label_index;

static int stack = 0;
static int num_args = 0;
static int num_vars = 0;

void push(const char *arg) {
    stack++;
    printf("  push %s\n", arg);
}

void push_val(int val) {
    stack++;
    printf("  push %d\n", val);
}

void pop(const char *arg) {
    stack--;
    printf("  pop %s\n", arg);
}

static void epilogue(Node *node) {
    if (node) {
        gen(node);
        pop("rax");
    } else {
        printf("  mov rax, 0\n");
    }
    printf("  mov rsp, rbp\n");
    printf("  pop rbp\n");
    printf("  ret\n");

    assert(stack == num_args);
    stack = 0;
    num_args = 0;
    num_vars = 0;
}

void gen_lval(Node *node) {
    switch (node->kind) {
    case ND_LVAR:
        switch (node->vkind) {
        case VLOCAL:
            printf("  mov rax, rbp\n");
            printf("  sub rax, %d /* %.*s */\n", node->offset, node->ident.len,
                   node->ident.ptr);
            break;
        case VGLOBAL:
            printf("  lea rax, %.*s[rip]\n", node->ident.len, node->ident.ptr);
            break;
        }
        push("rax");
        break;
    case ND_DEREF:
        gen(node->lhs);
        break;
    default:
        error("lvalue is not a varialbe or dereference");
    }
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
}

void gen_for(Node *node, int l_index) {
    if (node->lhs) {
        gen(node->lhs);
        pop("rax"); // consume the retval
    }
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
    if (node->lhs) {
        gen(node->lhs);
        pop("rax"); // consume the retval
    }
    printf("  jmp .Lbegin%d\n", l_index);
    printf(".Lend%d:\n", l_index);
}

void gen_block(Node *node) {
    while (node->lhs) {
        gen(node->lhs);
        node = node->rhs;
        if (!node || node->kind != ND_BLOCK)
            error("Expected ND_BLOCK");
    }
}

typedef struct NodeList NodeList;
struct NodeList {
    Node *node;
    NodeList *next;
};

void gen_call(Node *node) {
    int num_args = 0;
    Node *arg = node->lhs;
    NodeList *args = NULL;
    bool is_shifted = (stack + num_vars) % 2 == 1;

    if (is_shifted) {
        push_val(0); // align stack
    }

    while (arg) {
        if (arg->kind != ND_ARGS)
            error("Expected ND_ARGS");

        NodeList *tmp = calloc(1, sizeof(NodeList));
        tmp->node = arg->lhs;
        tmp->next = args;

        args = tmp;
        arg = arg->rhs;

        num_args++;
    }

    while (args) {
        gen(args->node);
        args = args->next;
    }

    for (int i = 0; i < num_args && i < 6; ++i) {
        pop(arg_registers[i]);
    }

    printf("  call %.*s\n", node->ident.len, node->ident.ptr);

    for (int i = 6; i < num_args; i++) {
        pop("rdi"); // consume remained arguments
    }
    if (is_shifted) {
        pop("rdi"); // restore shifted stack
    }

    push("rax");
}

static void gen_func(const Node *node) {
    printf(".global %.*s\n", node->ident.len, node->ident.ptr);
    printf("%.*s:\n", node->ident.len, node->ident.ptr);
    printf("  push rbp\n");
    printf("  mov rbp, rsp\n");
    stack = 0;

    Node *args = node->lhs;
    Node *body = node->rhs;

    // TODO
    num_args = args ? args->val : 0;
    for (int i = 0; i < num_args; ++i) {
        push(arg_registers[i]);
    }

    int variables_offset = body ? body->val : 0;
    printf("  sub rsp, %d /* allocate for local variables */\n",
           variables_offset);
    num_vars = variables_offset / 8;
    while (body) {
        gen(body->lhs);
        body = body->rhs;
    }

    epilogue(NULL);
}

bool eval_constexpr(const Node *node, int *val) {
    int lhs, rhs;
    switch (node->kind) {
    case (ND_NUM):
        *val = node->val;
        return true;
    case (ND_ADD):
        if (eval_constexpr(node->lhs, &lhs) &&
            eval_constexpr(node->rhs, &rhs)) {
            *val = lhs + rhs;
            return true;
        }
        return false;
    case (ND_SUB):
        if (eval_constexpr(node->lhs, &lhs) &&
            eval_constexpr(node->rhs, &rhs)) {
            *val = lhs - rhs;
            return true;
        }
        return false;
    case (ND_MUL):
        if (eval_constexpr(node->lhs, &lhs) &&
            eval_constexpr(node->rhs, &rhs)) {
            *val = lhs * rhs;
            return true;
        }
        return false;
    case (ND_DIV):
        if (eval_constexpr(node->lhs, &lhs) &&
            eval_constexpr(node->rhs, &rhs)) {
            *val = lhs / rhs;
            return true;
        }
        return false;
    case (ND_LT):
        if (eval_constexpr(node->lhs, &lhs) &&
            eval_constexpr(node->rhs, &rhs)) {
            *val = lhs < rhs;
            return true;
        }
        return false;
    case (ND_LE):
        if (eval_constexpr(node->lhs, &lhs) &&
            eval_constexpr(node->rhs, &rhs)) {
            *val = lhs <= rhs;
            return true;
        }
        return false;
    case (ND_EQ):
        if (eval_constexpr(node->lhs, &lhs) &&
            eval_constexpr(node->rhs, &rhs)) {
            *val = lhs == rhs;
            return true;
        }
        return false;
    case (ND_NE):
        if (eval_constexpr(node->lhs, &lhs) &&
            eval_constexpr(node->rhs, &rhs)) {
            *val = lhs != rhs;
            return true;
        }
        return false;
    case (ND_STRING):
    default:
        return false;
    }
}

static void gen_declare(const Node *node) {
    size_t size = sizeof_ty(node->ty);
    printf(".global %.*s\n", node->ident.len, node->ident.ptr);
    if (node->init == NULL) {
        printf(".bss\n");
    } else {
        printf(".data\n");
    }

    printf("%.*s:\n", node->ident.len, node->ident.ptr);

    if (node->init == NULL) {
        printf("  .zero %zu\n", size);
        return;
    }

    int val;
    if (eval_constexpr(node->init, &val)) {
        printf("  .long %d\n", val);
        return;
    }

    switch (node->init->kind) {
    case (ND_STRING):
        printf("  .ascii \"%.*s\\0\"\n", node->init->ident.len,
               node->init->ident.ptr);
        return;
    case (ND_ADDR):
        if (node->init->lhs->kind == ND_LVAR &&
            node->init->lhs->vkind == VGLOBAL) {
            printf("  .quad %.*s\n", node->init->lhs->ident.len,
                   node->init->lhs->ident.ptr);
            return;
        }
    case (ND_ADD):
        if (node->init->lhs->kind == ND_ADDR &&
            node->init->lhs->lhs->kind == ND_LVAR &&
            node->init->lhs->lhs->vkind == VGLOBAL &&
            node->init->rhs->kind == ND_NUM) {
            printf("  .quad %.*s + %d\n", node->init->lhs->lhs->ident.len,
                   node->init->lhs->lhs->ident.ptr, node->init->rhs->val);
            return;
        }
    default:
        break;
    }
    error("Invalid initializer for a global variable");
}

void gen_top(Unit *code) {
    printf(".text\n");
    if (code->function) {
        gen_func(code->function);
    } else if (code->declaration) {
        gen_declare(code->declaration);
    } else {
        error("Invalid top node");
    }
}

void gen_add(Node *node, char *op) {
    if (node->lhs->ty->ty == PTR) {
        if (node->rhs->ty->ty == PTR) {
            error("Invalid operands to binary '%s' between pointers", op);
        } else {
            size_t size = sizeof_ty(node->lhs->ty->ptr_to);
            gen(node->lhs);

            gen(node->rhs);
            pop("rax");
            switch (node->rhs->ty->ty) {
            case (INT):
                printf("  movsx rax, eax\n");
                break;
            case (CHAR):
                printf("  movsx rax, al\n");
                break;
            default:
                break;
            }
            printf("  mov rdi, %zu\n", size);
            printf("  imul rax, rdi\n");
            push("rax");
        }
    } else {
        if (node->rhs->ty->ty == PTR) {
            size_t size = sizeof_ty(node->rhs->ty->ptr_to);
            gen(node->lhs);
            pop("rax");
            switch (node->lhs->ty->ty) {
            case (INT):
                printf("  movsx rax, eax\n");
                break;
            case (CHAR):
                printf("  movsx rax, al\n");
                break;
            default:
                break;
            }
            printf("  mov rdi, %zu\n", size);
            printf("  imul rax, rdi\n");
            push("rax");

            gen(node->rhs);
        } else {
            gen(node->lhs);
            pop("rax");
            switch (node->lhs->ty->ty) {
            case (INT):
                printf("  movsx rax, eax\n");
                break;
            case (CHAR):
                printf("  movsx rax, al\n");
                break;
            default:
                break;
            }
            push("rax");

            gen(node->rhs);
            pop("rax");
            switch (node->rhs->ty->ty) {
            case (INT):
                printf("  movsx rax, eax\n");
                break;
            case (CHAR):
                printf("  movsx rax, al\n");
                break;
            default:
                break;
            }
            push("rax");
        }
    }

    pop("rdi");
    pop("rax");
    printf("  %s rax, rdi\n", op);
    push("rax");
}

Node *new_assign(Node *lhs, Node *rhs) {
    const Type *ty = lhs->ty;
    Node *node = new_node(ND_ASSIGN, lhs, rhs);
    node->ty = ty;
    return node;
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
        printf("  mov [rax], %s\n", di(sizeof_ty(node->lhs->ty)));
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
        epilogue(node->lhs);
        return;
    case ND_BLOCK:
        gen_block(node);
        return;
    case ND_CALL:
        gen_call(node);
        return;
    case ND_FUNC:
        // skip
        return;
    case ND_DEREF:
        gen(node->lhs);
        pop("rax");
        switch (node->lhs->ty->ptr_to->ty) {
        case INT:
            printf("  movsx rax, WORD PTR [rax]\n");
            break;
        case CHAR:
            printf("  movsx rax, BYTE PTR [rax]\n");
            break;
        default:
            printf("  mov rax, [rax]\n");
            break;
        }
        push("rax");
        return;
    case ND_ADDR:
        gen_lval(node->lhs);
        return;
    case ND_DECLARE:
        if (node->vkind == VLOCAL && node->init != NULL) {
            Node *var = calloc(1, sizeof(Node));
            var->kind = ND_LVAR;
            var->ty = node->ty;
            var->offset = node->offset;
            var->vkind = node->vkind;
            var->ident = node->ident;

            if (node->ty->ty == ARRAY) {
                var = as_ptr(var);
                Node *init = node->init->next;
                switch (node->init->kind) {
                case (ND_INIT):
                    for (int index = 0; index < node->ty->array_size; index++) {
                        Node *idx = new_node_num(index);
                        Node *lhs = deref_offset_ptr(var, idx);
                        if (init) {
                            gen(new_assign(lhs, init));
                            init = init->next;
                        } else {
                            gen(new_assign(lhs, new_node_num(0)));
                        }
                        pop("rax");
                    }
                    return;
                case (ND_STRING):
                    for (int index = 0; index < node->ty->array_size; index++) {
                        Node *idx = new_node_num(index);
                        Node *lhs = deref_offset_ptr(var, idx);
                        Node *rhs = deref_offset_ptr(node->init, idx);
                        gen(new_assign(lhs, rhs));
                        pop("rax");
                    }
                    return;
                default:
                    error("Internal compiler error: unreachable at %s:%d",
                          __FILE__, __LINE__);
                }
            }

            gen(new_assign(var, node->init));
            pop("rax");

            return;
        }
        return;
    case ND_ADD:
        gen_add(node, "add");
        return;
    case ND_SUB:
        gen_add(node, "sub");
        return;
    case ND_STRING:
        printf("  lea rax, .LC%d[rip]\n", node->val);
        push("rax");
        return;
    case ND_SEMICOLON:
        gen(node->lhs);
        pop("rax");
        return;
    default:
        break;
    }

    gen(node->lhs);
    gen(node->rhs);
    pop("rdi");
    pop("rax");

    size_t size = sizeof_ty(node->lhs->ty);

    switch (node->kind) {
    case ND_MUL:
        printf("  imul rax, rdi\n");
        break;
    case ND_DIV:
        printf("  cqo\n");
        printf("  idiv rdi\n");
        break;
    case ND_LT:
        printf("  cmp %s, %s\n", ax(size), di(size));
        printf("  setl al\n");
        printf("  movzb rax, al\n");
        break;
    case ND_LE:
        printf("  cmp %s, %s\n", ax(size), di(size));
        printf("  setle al\n");
        printf("  movzb rax, al\n");
        break;
    case ND_EQ:
        printf("  cmp %s, %s\n", ax(size), di(size));
        printf("  sete al\n");
        printf("  movzb rax, al\n");
        break;
    case ND_NE:
        printf("  cmp %s, %s\n", ax(size), di(size));
        printf("  setne al\n");
        printf("  movzb rax, al\n");
        break;
    default:
        break;
    }

    push("rax");
}

void gen_strings(Env *env) {
    String *str = env->strings;

    if (str) {
        printf("\n");
        printf(".text\n");
        printf(".section .rodata\n");
    }

    while (str) {
        printf(".LC%d:\n", str->index);
        printf("  .string \"%.*s\"\n", str->ident.len, str->ident.ptr);

        str = str->next;
    }
}
