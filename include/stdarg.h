#pragma once

typedef struct {
    unsigned int gp_offset;
    unsigned int fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
} va_list[1];

#define va_start(ap, last)

#define va_end(ap)

#define va_arg(ap, type)

#define va_copy(dest, src)

typedef va_list __gnuc_va_list;
