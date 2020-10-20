#include "test.h"

int
foo(int i) {
    int retval;
    switch (i) {
    case 0:
        retval = 1;
        break;
    case 1:
        retval = 2;
    case 2:
        retval = 3;
        break;
    case 3:
        retval = 4;
        break;
    default:
        retval = 5;
    }
    return retval;
}

int
main() {
    assert_int(1, foo(0));
    assert_int(3, foo(1));
    assert_int(3, foo(2));
    assert_int(4, foo(3));
    assert_int(5, foo(4));
    assert_int(5, foo(5));

    return summary();
}
