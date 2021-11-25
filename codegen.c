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
drop(const char *arg, const size_t eightbytes) {
    for (size_t i = 0; i < eightbytes; i++) {
        pop(arg);
    }
}

static size_t
gen(Node *node);

static void
epilogue(Node *node) {
    if (node) {
        gen(node); // TODO
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
gen_lval(const Node *node) {
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
    case ND_UNARY:
        if (node->unary.kind == OP_DEREF) {
            gen(node->unary.operand);
            break;
        }
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
        drop("rax", gen(statement->init));
    } else if (statement->declaration) {
        gen_local_declare(statement->declaration);
    }
    printf(".Lbegin%d:\n", jump_index);

    if (statement->cond) {
        const size_t i = gen(statement->cond);
        assert(i == 1);
        pop("rax");
        printf("  cmp rax, 0\n");
        printf("  je .Lend%d\n", jump_index);
    }

    gen_statement(statement->body);

    printf(".Lcontin%d:\n", jump_index);
    if (statement->end) {
        drop("rax", gen(statement->end));
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
    case (ND_BINARY):
        if (eval_constexpr(node->binary.lhs, &lhs) &&
            eval_constexpr(node->binary.rhs, &rhs)) {
            switch (node->binary.kind) {
            case OP_ADD:
                *val = lhs + rhs;
                break;
            case OP_SUB:
                *val = lhs - rhs;
                break;
            case OP_MUL:
                *val = lhs * rhs;
                break;
            case OP_DIV:
                *val = lhs / rhs;
                break;
            case OP_SHL:
                *val = lhs << rhs;
                break;
            case OP_SHR:
                *val = lhs >> rhs;
                break;
            case OP_LT:
                *val = lhs < rhs;
                break;
            case OP_LE:
                *val = lhs <= rhs;
                break;
            case OP_EQ:
                *val = lhs == rhs;
                break;
            case OP_NE:
                *val = lhs != rhs;
                break;
            case OP_AND:
                *val = lhs & rhs;
                break;
            case OP_OR:
                *val = lhs | rhs;
                break;
            case OP_XOR:
                *val = lhs ^ rhs;
                break;
            case OP_LAND:
                *val = lhs && rhs;
                break;
            case OP_LOR:
                *val = lhs || rhs;
                break;
            }
            return true;
        }
        return false;
    case (ND_STRING):
    default:
        return false;
    }
}

static bool
is_global_var(const Node *node) {
    return node->kind == ND_LVAR && node->var->kind == VGLOBAL;
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

    const Node *lhs = NULL;
    const Node *rhs = NULL;

    switch (init->kind) {
    case (ND_STRING):
        printf("  .ascii \"%s\\0\"\n", init->str);
        return;
    case (ND_UNARY):
        if (init->unary.kind == OP_ADDR && is_global_var(init->unary.operand)) {
            printf("  .quad %s\n", init->unary.operand->var->ident);
            return;
        }
        break;
    case (ND_BINARY):
        if (init->binary.kind == OP_ADD) {
            lhs = init->binary.lhs;
            rhs = init->binary.rhs;
            if (lhs->kind == ND_UNARY && lhs->unary.kind == OP_ADDR &&
                is_global_var(lhs->unary.operand)) {
                printf("  .quad %s + %d\n", lhs->unary.operand->var->ident,
                       rhs->val);
                return;
            }
        }
        break;
    default:;
    }
    error("Invalid initializer for a global variable");
}

void
gen_top(const Unit *code) {
    if (code->function) {
        printf(".text\n");
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
            const Vars *members = ty->struct_ty.members;
            const InitList *list = init->list;
            for (; members; members = members->next) {
                Var *mvar = calloc(1, sizeof(Var));
                mvar->ident = members->var->ident;
                mvar->ty = members->var->ty;
                mvar->offset = var->var->offset - members->var->offset;

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
            const size_t eightbytes = gen(init->expr);
            gen_lval(var);
            pop("rax");
            for (size_t i = eightbytes; i > 0; i--) {
                pop("rdi");
                printf("  mov QWORD PTR [rax+%ld], rdi\n", (i - 1) * 8);
            }
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
            drop("rax", gen(statement->expression));
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
gen_cast(Node *node, const Type *ty) {
    gen(node);

    const Type *from = node->ty;
    const Type *to = ty;
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

static size_t
gen_lvar(const Node *node) {
    if (node->var->is_const) {
        push_val(node->var->enum_val);
        return 1;
    }

    gen_lval(node);
    pop("rax");

    size_t eightbytes = 0;
    const size_t size = sizeof_ty(node->var->ty);
    for (size_t i = 0; i < size; i += 8) {
        printf("  mov rdi, QWORD PTR [rax+%ld]\n", i);
        push("rdi");
        eightbytes++;
    }
    return eightbytes;
}

static void
gen_unary_op(const UnaryOp op, const Type *ty) {
    switch (op.kind) {
    case OP_INCR:
        gen_increment("add", op.operand);
        break;
    case OP_DECR:
        gen_increment("sub", op.operand);
        break;
    case OP_ADDR:
        gen_lval(op.operand);
        break;
    case OP_DEREF:
        gen(op.operand);
        pop("rax");
        switch (sizeof_ty(op.operand->ty->ptr_to)) {
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
    case OP_CAST:
        gen_cast(op.operand, ty);
        break;
    }
}

static void
gen_binary_op(const BinaryOp op) {
    switch (op.kind) {
    case OP_ADD:
        gen_add("add", op.lhs, op.rhs);
        break;
    case OP_SUB:
        gen_add("sub", op.lhs, op.rhs);
        break;
    case OP_MUL:
        gen(op.lhs);
        gen(op.rhs);
        pop("rdi");
        pop("rax");
        printf("  imul rax, rdi\n");
        push("rax");
        break;
    case OP_DIV:
        gen(op.lhs);
        gen(op.rhs);
        pop("rdi");
        pop("rax");
        printf("  cqo\n");
        printf("  idiv rdi\n");
        push("rax");
        break;
    case OP_SHL:
        gen_shift("shl", op.lhs, op.rhs);
        break;
    case OP_SHR:
        gen_shift("shr", op.lhs, op.rhs);
        break;
    case OP_LT:
        gen_comparison("setl", op.lhs, op.rhs);
        break;
    case OP_LE:
        gen_comparison("setle", op.lhs, op.rhs);
        break;
    case OP_EQ:
        gen_comparison("sete", op.lhs, op.rhs);
        break;
    case OP_NE:
        gen_comparison("setne", op.lhs, op.rhs);
        break;
    case OP_AND:
        gen_bitwise("and", op.lhs, op.rhs);
        break;
    case OP_OR:
        gen_bitwise("or", op.lhs, op.rhs);
        break;
    case OP_XOR:
        gen_bitwise("xor", op.lhs, op.rhs);
        break;
    case OP_LAND:
        gen_logical("and", op.lhs, op.rhs);
        break;
    case OP_LOR:
        gen_logical("or", op.lhs, op.rhs);
        break;
    }
}

static size_t
gen(Node *node) {
    switch (node->kind) {
    case ND_UNARY:
        gen_unary_op(node->unary, node->ty);
        break;
    case ND_BINARY:
        gen_binary_op(node->binary);
        break;
    case ND_NUM:
        push_val(node->val);
        break;
    case ND_LVAR:
        return gen_lvar(node);
    case ND_ASSIGN:
        gen_assign(node->lhs, node->rhs);
        break;
    case ND_CALL:
        gen_call(node);
        break;
    case ND_STRING:
        printf("  lea rax, .LC%d[rip]\n", node->val);
        push("rax");
        break;
    case ND_TERNARY:
        gen_ternary(node);
        break;
    }
    return 1;
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
