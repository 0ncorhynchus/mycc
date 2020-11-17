#include "test.h"

int
main() {
    int x = 1;

    assert_int(1, x);
    assert_int(1, x << 0);
    assert_int(1, x >> 0);

    return summary();
}

