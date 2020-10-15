#include <stdio.h>
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

static int test_no = 0;
static int num_passed = 0;
static int num_failed;

int assert_int(int expected, int actual) {
    test_no = test_no + 1;
    if (expected == actual) {
        num_passed = num_passed + 1;
    } else {
        num_failed = num_failed + 1;
        printf("%03d: ", test_no);
        printf("Failed. %d expected, but got %d\n", expected, actual);
        printf("     expected: 0x%08X\n", expected);
        printf("     actual:   0x%08X\n", actual);
    }
    return 0;
}

int summary() {
    printf("SUMMARY:\n");
    printf("    %d tests passed.\n", num_passed);
    printf("    %d tests failed.\n", num_failed);
    return num_failed > 0;
}

