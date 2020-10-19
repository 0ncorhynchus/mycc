#include "test.h"

enum Base {
    A,
    G,
    C,
    T,
};

int
main() {
    assert_int(4, sizeof(enum Base));
    assert_int(4, sizeof(enum Base));

    assert_int(0, A);
    assert_int(1, G);
    assert_int(2, C);
    assert_int(3, T);

    int i = A;
    assert_int(A, i);

    enum Base j = 1;
    assert_int(G, j);

    assert_int(1, A + 1);

    enum Real { Float, Double };
    assert_int(0, Float);
    assert_int(1, Double);

    return summary();
}
