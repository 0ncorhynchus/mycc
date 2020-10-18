#include "test.h"

int
main() {
    int x;

    {} // check compile is passed
    {
        ; // check compile is passed
    }
    {
        x = 0;
        { // nested block
            x = 1;
        }
        assert_int(1, x);
    }
    assert_int(1, x);
    {
        x = 2;
        assert_int(2, x);
        x = 3;
        assert_int(3, x);
    }
    {
        x = 4;
        assert_int(4, x);
        x = 5;
        assert_int(5, x);
        return summary();
    }
}
