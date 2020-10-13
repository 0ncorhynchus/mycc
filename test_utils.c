#include <stdlib.h>

int many(int x, int y, int z, int p, int q, int r, int m, int n) {
    return x + y + z + p + q + r + m + n;
}

int alloc4(int **p, int a, int b, int c, int d) {
    *p = malloc(4 * sizeof(int));
    (*p)[0] = a;
    (*p)[1] = b;
    (*p)[2] = c;
    (*p)[3] = d;
    return 0;
}
