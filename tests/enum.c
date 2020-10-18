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

    return summary();
}
