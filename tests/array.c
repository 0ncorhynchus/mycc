#include "test.h"

int
main() {
    int a[2] = {1, 2};

    assert_int(1, a[0]);
    assert_int(2, a[1]);
    assert_int(2, a[1]);
    assert_int(2, +a[1]);
    assert_int(1, -a[1] + 3);
    assert_int(4, sizeof(a[1]));

    *a = 1;
    assert_int(1, *a);

    *(a + 1) = 2;
    assert_int(2, *(1 + a));

    *(1 + a) = 2;
    assert_int(2, *(a + 1));

    int *p = a;
    *p = 0;
    assert_int(0, *p);

    *a = 1;
    *(a + 1) - 2;
    p = a;
    assert_int(3, *p + *(p + 1));

    char *str = "abcdefg";
    char *c_ptr = str;

    assert_int('a', *c_ptr);
    c_ptr++;
    assert_int('b', *c_ptr);

    return summary();
}
