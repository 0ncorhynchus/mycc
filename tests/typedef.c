#include "test.h"

typedef int integer;

typedef enum Base Base;
enum Base {
    A,
    G,
    C,
    T,
};

typedef enum Real { Float, Double } Real;
typedef enum { Char, Short, Int, Long, LongLong } Integer;

int
main() {
    integer i = 1;

    assert_int(1, i);
    assert_int(sizeof(int), sizeof(integer));

    assert_int(sizeof(int), sizeof(Base));
    Base j = C;

    assert_int(sizeof(int), sizeof(Real));
    assert_int(0, Float);
    assert_int(1, Double);

    assert_int(sizeof(int), sizeof(Integer));
    assert_int(0, Char);
    assert_int(1, Short);
    assert_int(2, Int);
    assert_int(3, Long);
    assert_int(4, LongLong);

    Integer x = Int;
    Integer *y;
    y = &x;
    assert_int(Int, *y);

    return summary();
}
