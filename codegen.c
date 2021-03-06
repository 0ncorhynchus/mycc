#include "mycc.h"
#include <assert.h>
#include <stdio.h>

static const char *arg_registers[6] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};

static const char *
ax(size_t size) {
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

static const char *
di(size_t size) {
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

static const char *
ptr_size(size_t size) {
    switch (size) {
    case 1:
        return "BYTE";
    case 2:
        return "WORD";
    case 4:
        return "DWORD";
    case 8:
        return "QWORD";
    default:
        error("Not supported size.");
        return NULL;
    }
}

static int stack = 0;
static int num_args = 0;
static int num_vars = 0;

static void
push(const char *arg) {
    stack++;
    printf("  push %s\n", arg);
}

static void
push_val(int val) {
    stack++;
    printf("  push %d\n", val);
}

static void
pop(const char *arg) {
    stack--;
    printf("  pop %s\n", arg);
}

static void
epilogue(Node *node) {
    if (node) {
        gen(node);
        pop("rax");
    } else {
        printf("  mov rax, 0\n");
    }
    printf("  leave\n");
    printf("  ret\n");

    assert(stack == num_args);
    stack = 0;
    num_args = 0;
    num_vars = 0;
}

static void
gen_lval(Node *node) {
    const Var *var;
    switch (node->kind) {
    case ND_LVAR:
        var = node->var;
        if (var->is_const) {
            error("Error at %s:%d", __FILE__, __LINE__);
        } else {
            switch (var->kind) {
            case VLOCAL:
                printf("  lea rax, -%d[rbp] /* %s */\n", var->offset,
                       var->ident);
                break;
            case VGLOBAL:
                printf("  lea rax, %s[rip]\n", var->ident);
                break;
            }
            push("rax");
        }
        break;
    case ND_DEREF:
        gen(node->lhs);
        break;
    default:
        error("lvalue is not a varialbe or dereference");
    }
}

static void
gen_statement(const Statement *statement);

static void
gen_while(const Statement *statement) {
    const int jump_index = statement->jump_index;
    printf(".Lbegin%d:\n", jump_index);
    gen(statement->cond);
    pop("rax");
    printf("  cmp rax, 0\n");
    printf("  je .Lend%d\n", jump_index);
    gen_statement(statement->body);
    printf(".Lcontin%d:\n", jump_index);
    printf("  jmp .Lbegin%d\n", jump_index);
    printf(".Lend%d:\n", jump_index);
}

static void
gen_local_declare(const Declaration *decl);

static void
gen_for(const Statement *statement) {
    const int jump_index = statement->jump_index;

    if (statement->init) {
        gen(statement->init);
        pop("rax"); // consume the retval
    } else if (statement->declaration) {
        gen_local_declare(statement->declaration);
    }
    printf(".Lbegin%d:\n", jump_index);

    if (statement->cond) {
        gen(statement->cond);
        pop("rax");
        printf("  cmp rax, 0\n");
        printf("  je .Lend%d\n", jump_index);
    }

    gen_statement(statement->body);

    printf(".Lcontin%d:\n", jump_index);
    if (statement->end) {
        gen(statement->end);
        pop("rax"); // consume the retval
    }
    printf("  jmp .Lbegin%d\n", jump_index);
    printf(".Lend%d:\n", jump_index);
}

static void
gen_block(const BlockItems *list) {
    while (list) {
        if (list->declaration) {
            gen_local_declare(list->declaration);
        } else if (list->statement) {
            gen_statement(list->statement);
        } else {
            error("Internal compiler error");
        }
        list = list->next;
    }
}

static void
gen_call(Node *node) {
    int num_args = 0;
    NodeList *args = node->args;
    bool is_shifted = (stack + num_vars) % 2 == 1;

    if (is_shifted) {
        push_val(0); // align stack
    }

    while (args) {
        gen(args->node);
        args = args->next;
        num_args++;
    }

    for (int i = 0; i < num_args && i < 6; ++i) {
        pop(arg_registers[i]);
    }

    printf("  call %s\n", node->fn);

    for (int i = 6; i < num_args; i++) {
        pop("rdi"); // consume remained arguments
    }
    if (is_shifted) {
        pop("rdi"); // restore shifted stack
    }

    push("rax");
}

static void
gen_func(const Function *fn) {
    printf(".global %s\n", fn->def->ident);
    printf("%s:\n", fn->def->ident);
    printf("  push rbp\n");
    printf("  mov rbp, rsp\n");
    stack = 0;

    // TODO
    num_args = fn->num_args <= 6 ? fn->num_args : 6;
    for (int i = 0; i < num_args; ++i) {
        push(arg_registers[i]);
    }

    printf("  sub rsp, %d /* allocate for local variables */\n",
           fn->lvar_offset);
    num_vars = fn->lvar_offset / 8;

    gen_block(fn->body->block);

    epilogue(NULL);
}

bool
eval_constexpr(const Node *node, int *val) {
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

static void
gen_declare(const Declaration *decl) {
    const Var *var = decl->var;
    if (var->ty->ty == FUNCTION) {
        return;
    }

    printf(".global %s\n", var->ident);
    if (decl->init == NULL) {
        printf(".bss\n");
    } else {
        printf(".data\n");
    }

    printf("%s:\n", var->ident);

    if (decl->init == NULL) {
        const size_t size = sizeof_ty(var->ty);
        printf("  .zero %zu\n", size);
        return;
    }

    if (decl->init->expr == NULL) {
        error("Not supported global variables with an initializer list yet.");
        return;
    }

    const Node *init = decl->init->expr;

    int val;
    if (eval_constexpr(init, &val)) {
        printf("  .long %d\n", val);
        return;
    }

    const Node *lhs = init->lhs;
    const Node *rhs = init->rhs;

    switch (init->kind) {
    case (ND_STRING):
        printf("  .ascii \"%s\\0\"\n", init->str);
        return;
    case (ND_ADDR):
        if (lhs->kind == ND_LVAR && lhs->var->kind == VGLOBAL) {
            printf("  .quad %s\n", lhs->var->ident);
            return;
        }
    case (ND_ADD):
        if (lhs->kind == ND_ADDR && lhs->lhs->kind == ND_LVAR &&
            lhs->lhs->var->kind == VGLOBAL && rhs->kind == ND_NUM) {
            printf("  .quad %s + %d\n", lhs->lhs->var->ident, rhs->val);
            return;
        }
    default:
        error("Invalid initializer for a global variable");
    }
}

void
gen_top(const Unit *code) {
    printf(".text\n");
    if (code->function) {
        gen_func(code->function);
    } else if (code->declaration) {
        gen_declare(code->declaration);
    } else {
        error("Invalid top node");
    }
}

static void
gen_add(const char *op, Node *lhs, Node *rhs) {
    if (lhs->ty->ty == PTR) {
        if (rhs->ty->ty == PTR) {
            error("Invalid operands to binary '%s' between pointers", op);
        } else {
            size_t size = sizeof_ty(lhs->ty->ptr_to);
            gen(lhs);
            gen(rhs);

            pop("rax");
            switch (sizeof_ty(rhs->ty)) {
            case 1:
                printf("  movsx rax, al\n");
                break;
            case 4:
                printf("  movsxd rax, eax\n");
                break;
            }
            printf("  mov rdi, %zu\n", size);
            printf("  imul rdi, rax\n");

            pop("rax");
            printf("  %s rax, rdi\n", op);
            push("rax");
        }
    } else {
        if (rhs->ty->ty == PTR) {
            size_t size = sizeof_ty(rhs->ty->ptr_to);
            gen(rhs);
            gen(lhs);

            pop("rax");
            switch (sizeof_ty(lhs->ty)) {
            case 1:
                printf("  movsx rax, al\n");
                break;
            case 4:
                printf("  movsxd rax, eax\n");
                break;
            }
            printf("  mov rdi, %zu\n", size);
            printf("  imul rax, rdi\n");

            pop("rdi");
            printf("  %s rax, rdi\n", op);
            push("rax");
        } else {
            gen(lhs);
            gen(rhs);

            pop("rdi");
            switch (sizeof_ty(rhs->ty)) {
            case 1:
                printf("  movsx rdi, dil\n");
                break;
            case 4:
                printf("  movsxd rdi, edi\n");
                break;
            }

            pop("rax");
            switch (sizeof_ty(lhs->ty)) {
            case 1:
                printf("  movsx rax, al\n");
                break;
            case 4:
                printf("  movsxd rax, eax\n");
                break;
            }

            printf("  %s rax, rdi\n", op);
            push("rax");
        }
    }
}

static void
gen_assign(Node *lhs, Node *rhs) {
    const size_t size = sizeof_ty(lhs->ty);
    gen_lval(lhs);
    gen(rhs);

    pop("rdi");
    pop("rax");
    printf("  mov %s PTR [rax], %s\n", ptr_size(size), di(size));
    push("rdi");
}

static void
gen_local_declare(const Declaration *decl) {
    if (decl->var == NULL || decl->var->kind != VLOCAL || decl->init == NULL) {
        return;
    }

    Node *var = calloc(1, sizeof(Node));
    var->kind = ND_LVAR;
    var->var = decl->var;
    var->ty = var->var->ty;

    const Type *ty = decl->var->ty;
    const Initializer *init = decl->init;
    switch (ty->ty) {
    case ARRAY:
        if (init->expr && init->expr->kind == ND_STRING) {
            for (int index = 0; index < ty->array_size; index++) {
                Node *idx = new_node_num(index);
                Node *lhs = deref_offset_ptr(as_ptr(var), idx);
                Node *rhs = deref_offset_ptr(init->expr, idx);
                gen_assign(lhs, rhs);
                pop("rax");
            }
            return;
        } else if (init->list) {
            const InitList *list = init->list;
            for (int index = 0; index < ty->array_size; index++) {
                Node *idx = new_node_num(index);
                Node *lhs = deref_offset_ptr(as_ptr(var), idx);
                if (list && list->inner) {
                    if (list->inner->expr == NULL) {
                        error("Not supported nested initilizer lists.");
                    }
                    gen_assign(lhs, list->inner->expr);
                    list = list->next;
                } else {
                    gen_assign(lhs, new_node_num(0));
                }
                pop("rax");
            }
            return;
        } else {
            error("Internal compiler error: unreachable at %s:%d", __FILE__,
                  __LINE__);
        }
    case STRUCT:
        if (init->list) {
            const Members *member = ty->struct_ty.members;
            const InitList *list = init->list;
            for (; member; member = member->next) {
                Var *mvar = calloc(1, sizeof(Var));
                mvar->ident = member->member->ident;
                mvar->ty = member->member->ty;
                mvar->offset = var->var->offset - member->member->offset;

                Node *lhs = calloc(1, sizeof(Node));
                lhs->kind = ND_LVAR;
                lhs->var = mvar;
                lhs->ty = lhs->var->ty;
                if (list && list->inner) {
                    if (list->inner->expr == NULL) {
                        error("Not supported nested initilizer lists.");
                    }
                    gen_assign(lhs, list->inner->expr);
                    list = list->next;
                } else {
                    gen_assign(lhs, new_node_num(0));
                }
                pop("rax");
            }
        } else {
            error("Not supported an initializer expression for struct.");
        }
        break;
    case UNION:
        if (init->list == NULL) {
            error("An initializer expression is invalid for union.");
        }
        const InitList *list = init->list;
        if (list->inner->expr == NULL) {
            error("A nested initilizer list is invalid for union.");
        }
        gen_assign(var, list->inner->expr);
        pop("rax");
        break;
    default:
        // check if `init` is an initializer list
        if (init->expr == NULL) {
            error("Not supported initializer lists for '%s'", type_to_str(ty));
        }
        gen_assign(var, init->expr);
        pop("rax");
    }
}

static void
gen_if(const Statement *statement) {
    gen(statement->cond);
    pop("rax");
    printf("  cmp rax, 0\n");
    if (statement->else_body) {
        printf("  je .Lelse%d\n", statement->jump_index);
        gen_statement(statement->then_body);
        printf("  jmp .Lend%d\n", statement->jump_index);
        printf(".Lelse%d:\n", statement->jump_index);
        gen_statement(statement->else_body);
    } else {
        printf("  je .Lend%d\n", statement->jump_index);
        gen_statement(statement->then_body);
    }
    printf(".Lend%d:\n", statement->jump_index);
}

static void
gen_switch(const Statement *statement) {
    gen(statement->value);
    pop("rax");

    const int *default_jump_index = NULL;
    LabelList *labels = statement->labels;
    while (labels) {
        const Label *label = labels->label;
        switch (label->kind) {
        case CASE:
            printf("  cmp %s, %d\n", ax(4), label->val);
            printf("  je .L%d\n", label->jump_index);
            break;
        case DEFAULT:
            default_jump_index = &label->jump_index;
            break;
        }
        labels = labels->next;
    }
    if (default_jump_index) {
        printf("  jmp .L%d\n", *default_jump_index);
    }

    gen_statement(statement->body);
    printf(".Lend%d:\n", statement->jump_index);
}

static void
gen_statement(const Statement *statement) {
    switch (statement->kind) {
    case ST_LABEL:
        printf(".L%d:\n", statement->label.jump_index);
        gen_statement(statement->body);
        break;
    case ST_COMPOUND:
        gen_block(statement->block);
        break;
    case ST_EXPRESSION:
        if (statement->expression) {
            gen(statement->expression);
            pop("rax");
        }
        break;
    case ST_IF:
        gen_if(statement);
        break;
    case ST_SWITCH:
        gen_switch(statement);
        break;
    case ST_WHILE:
        gen_while(statement);
        break;
    case ST_FOR:
        gen_for(statement);
        break;
    case ST_CONTINUE:
        printf("  jmp .Lcontin%d\n", statement->jump_index);
        break;
    case ST_BREAK:
        printf("  jmp .Lend%d\n", statement->jump_index);
        break;
    case ST_RETURN:
        epilogue(statement->retval);
        break;
    }
}

static void
gen_comparison(const char *op, Node *lhs, Node *rhs) {
    gen(lhs);
    gen(rhs);
    pop("rdi");
    pop("rax");
    const size_t size = sizeof_ty(lhs->ty);
    printf("  cmp %s, %s\n", ax(size), di(size));
    printf("  %s al\n", op);
    printf("  movzx rax, al\n");
    push("rax");
}

static void
gen_increment(const char *op, Node *node) {
    if (node->ty->ty == PTR) {
        const size_t size = sizeof_ty(node->ty->ptr_to);
        gen_lval(node);
        pop("rax");
        printf("  mov rdi, QWORD PTR [rax]\n");
        push("rdi");
        printf("  %s rdi, %ld\n", op, size);
        printf("  mov QWORD PTR [rax], rdi\n");
    } else if (node->ty->ty == INTEGER) {
        const size_t size = sizeof_ty(node->ty);
        gen_lval(node);
        pop("rax");
        printf("  mov %s, %s PTR [rax]\n", di(size), ptr_size(size));
        push("rdi");
        printf("  %s %s, 1\n", op, di(size));
        printf("  mov %s PTR [rax], %s\n", ptr_size(size), di(size));
    }
}

static void
gen_shift(const char *op, Node *lhs, Node *rhs) {
    gen(lhs);
    gen(rhs);
    pop("rcx");
    pop("rax");
    printf("  %s rax, cl\n", op);
    push("rax");
}

static void
gen_bitwise(const char *op, Node *lhs, Node *rhs) {
    gen(lhs);
    gen(rhs);
    pop("rdi");
    pop("rax");
    printf("  %s rax, rdi\n", op);
    push("rax");
}

static void
gen_logical(const char *op, Node *lhs, Node *rhs) {
    gen(lhs);
    gen(rhs);
    pop("rdi");
    pop("rax");
    printf("  %s al, dil\n", op);
    push("rax");
}

static void
gen_ternary(Node *node) {
    gen(node->cond);
    pop("rax");
    printf("  cmp rax, 0\n");

    printf("  je .Lelse%d\n", node->jump_index);
    gen(node->lhs);
    printf("  jmp .Lend%d\n", node->jump_index);
    printf(".Lelse%d:\n", node->jump_index);
    gen(node->rhs);
    stack--;

    printf(".Lend%d:\n", node->jump_index);
}

static void
gen_cast(Node *node) {
    gen(node->lhs);

    const Type *from = node->lhs->ty;
    const Type *to = node->ty;
    if ((from->ty != INTEGER && from->ty != ENUM && from->ty != PTR) ||
        (to->ty != INTEGER && to->ty != ENUM)) {
        error("Not implemented cast from '%s' to '%s'", type_to_str(from),
              type_to_str(to));
    }

    const size_t from_size = sizeof_ty(from);
    const size_t to_size = sizeof_ty(to);
    if (to_size <= from_size) {
        return;
    }

    pop("rax");
    if (to_size > 4) {
        if (from->integer.is_unsigned) {
            printf("  movzx rax, %s\n", ax(from_size));
        } else {
            if (from_size == 4) {
                printf("  movsxd rax, eax\n");
            } else {
                printf("  movsx rax, %s\n", ax(from_size));
            }
        }
    } else {
        if (from->integer.is_unsigned) {
            printf("  movzx eax, %s\n", ax(from_size));
        } else {
            printf("  movsx eax, %s\n", ax(from_size));
        }
    }
    push("rax");
}

void
gen(Node *node) {
    switch (node->kind) {
    case ND_NUM:
        push_val(node->val);
        break;
    case ND_LVAR:
        if (node->var->is_const) {
            push_val(node->var->enum_val);
        } else {
            gen_lval(node);
            pop("rax");
            printf("  mov rax, QWORD PTR [rax]\n");
            push("rax");
        }
        break;
    case ND_ASSIGN:
        gen_assign(node->lhs, node->rhs);
        break;
    case ND_CALL:
        gen_call(node);
        break;
    case ND_DEREF:
        gen(node->lhs);
        pop("rax");
        switch (sizeof_ty(node->lhs->ty->ptr_to)) {
        case (1):
            printf("  movsx rax, BYTE PTR [rax]\n");
            break;
        case (4):
            printf("  movsx rax, DWORD PTR [rax]\n");
            break;
        default:
            printf("  mov rax, QWORD PTR [rax]\n");
            break;
        }
        push("rax");
        break;
    case ND_ADDR:
        gen_lval(node->lhs);
        break;
    case ND_ADD:
        gen_add("add", node->lhs, node->rhs);
        break;
    case ND_SUB:
        gen_add("sub", node->lhs, node->rhs);
        break;
    case ND_STRING:
        printf("  lea rax, .LC%d[rip]\n", node->val);
        push("rax");
        break;
    case ND_MUL:
        gen(node->lhs);
        gen(node->rhs);
        pop("rdi");
        pop("rax");
        printf("  imul rax, rdi\n");
        push("rax");
        break;
    case ND_DIV:
        gen(node->lhs);
        gen(node->rhs);
        pop("rdi");
        pop("rax");
        printf("  cqo\n");
        printf("  idiv rdi\n");
        push("rax");
        break;
    case ND_LT:
        gen_comparison("setl", node->lhs, node->rhs);
        break;
    case ND_LE:
        gen_comparison("setle", node->lhs, node->rhs);
        break;
    case ND_EQ:
        gen_comparison("sete", node->lhs, node->rhs);
        break;
    case ND_NE:
        gen_comparison("setne", node->lhs, node->rhs);
        break;
    case ND_INCR:
        gen_increment("add", node->lhs);
        break;
    case ND_DECR:
        gen_increment("sub", node->lhs);
        break;
    case ND_SHL:
        gen_shift("shl", node->lhs, node->rhs);
        break;
    case ND_SHR:
        gen_shift("shr", node->lhs, node->rhs);
        break;
    case ND_AND:
        gen_bitwise("and", node->lhs, node->rhs);
        break;
    case ND_XOR:
        gen_bitwise("xor", node->lhs, node->rhs);
        break;
    case ND_OR:
        gen_bitwise("or", node->lhs, node->rhs);
        break;
    case ND_LAND:
        gen_logical("and", node->lhs, node->rhs);
        break;
    case ND_LOR:
        gen_logical("or", node->lhs, node->rhs);
        break;
    case ND_TERNARY:
        gen_ternary(node);
        break;
    case ND_CAST:
        gen_cast(node);
        break;
    }
}

void
gen_strings(Env *env) {
    const String *str = env->strings;

    if (str) {
        printf("\n");
        printf(".text\n");
        printf(".section .rodata\n");
    }

    while (str) {
        printf(".LC%d:\n", str->index);
        printf("  .string \"%s\"\n", str->ident);

        str = str->next;
    }
}
