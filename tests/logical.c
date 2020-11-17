#include "test.h"

int
main() {
    assert_int(0, 0 & 0);
    assert_int(0, 0 & 1);
    assert_int(0, 1 & 0);
    assert_int(1, 1 & 1);

    assert_int(0, 0 ^ 0);
    assert_int(1, 0 ^ 1);
    assert_int(1, 1 ^ 0);
    assert_int(0, 1 ^ 1);

    return summary();
}
