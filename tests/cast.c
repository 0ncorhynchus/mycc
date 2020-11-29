#include "test.h"

static void
cast_to_greater_rank() {
    unsigned char uchar;
    char schar;

    uchar = 255;
    schar = -1;

    // signed  -> unsigned
    assert_unsigned_int(4294967295, (unsigned int)schar);

    // unsigned -> unsigned
    assert_unsigned_int(255, (unsigned int)uchar);

    // signed -> signed
    assert_int(-1, (int)schar);

    // unsigned -> signed
    assert_int(255, (int)uchar);
}

static void
cast_to_lower_rank() {
    unsigned long long ulong;
    long long slong;

    ulong = 18446744073709551615;
    slong = -1;

    // signed  -> unsigned
    assert_unsigned_int(4294967295, (unsigned int)slong);

    // unsigned -> unsigned
    assert_unsigned_int(4294967295, (unsigned int)ulong);

    // signed -> signed
    assert_int(-1, (int)slong);

    // unsigned -> signed
    //
    // this case is implementation-defined.
    //
    assert_int(4294967295, (int)ulong);
}

int
main() {
    cast_to_greater_rank();
    cast_to_lower_rank();

    return summary();
}
