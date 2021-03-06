#include "test.h"

int
main() {
    assert_int(0, 0);
    assert_int(42, 42);
    assert_int(8, 010);
    assert_int(16, 0x10);

    assert_int(21, 5 + 20 - 4);
    assert_int(41, 12 + 34 - 5);
    assert_int(47, 5 + 6 * 7);
    assert_int(15, 5 * (9 - 6));
    assert_int(4, (3 + 5) / 2);
    assert_int(15, 3 * +5);
    assert_int(10, -10 + 20);

    assert_int(0, 1 == (1 + 1));
    assert_int(1, 1 != (1 + 1));
    assert_int(1, 1 <= (1 + 1));
    assert_int(1, 1 < (1 + 1));
    assert_int(0, 1 >= (1 + 1));
    assert_int(0, 1 > (1 + 1));

    int x;
    char y;
    char *z;

    x = 0;
    y = 0;
    z = "abc";

    assert_int(0, x);
    assert_int(0, y);

    assert_int(0, x + y);
    assert_int(0, y + x);
    assert_int(0, x - y);
    assert_int(0, y - x);

    assert_int(97, z[0]);
    assert_int(97, z[x]);
    assert_int(97, z[y]);

    assert_int(0, x);
    assert_int(0, x++);
    assert_int(1, x);
    assert_int(1, x++);
    assert_int(2, x);
    assert_int(2, x++);
    assert_int(3, x);
    assert_int(3, x--);
    assert_int(2, x);
    assert_int(2, x--);
    assert_int(1, x);
    assert_int(1, x--);
    assert_int(0, x);

    long lx = 0;
    long int ly;
    signed int long lz;

    assert_int(0, lx);
    ly = lx + 1;
    assert_int(1, ly);
    lz = lx + ly;
    assert_int(1, lz);

    return summary();
}
