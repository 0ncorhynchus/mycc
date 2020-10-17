#include "mycc.h"
#include <stdarg.h>
#include <stdio.h>

void
debug(char *fmt, ...) {
    fprintf(stderr, "[debug] ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
}

void
error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}
