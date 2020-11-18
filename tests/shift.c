#include "test.h"

int
main() {
    assert_int(1, 1 << 0);
    assert_int(1, 1 >> 0);

    assert_int(2, 1 << 1);
    assert_int(0, 1 >> 1);

    assert_int(4, 1 << 2);
    assert_int(0, 1 >> 2);

    return summary();
}
