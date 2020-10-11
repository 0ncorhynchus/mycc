#include <stdio.h>

int foo() { printf("[foo] OK\n"); }

int bar(int x) { printf("[bar] x: %d\n", x); }

int piyo(int x, int y, int z, int p, int q, int r, int m, int n) {
    printf("[piyo] x: %d\n", x);
    printf("[piyo] y: %d\n", y);
    printf("[piyo] z: %d\n", z);
    printf("[piyo] p: %d\n", p);
    printf("[piyo] q: %d\n", q);
    printf("[piyo] r: %d\n", r);
    printf("[piyo] m: %d\n", m);
    printf("[piyo] n: %d\n", n);
}
