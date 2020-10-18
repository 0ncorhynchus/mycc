#include "test.h"

typedef int Integer;

typedef enum Base Base;
enum Base {
    A,
    G,
    C,
    T,
};

int
main() {
    Integer i = 1;

    assert_int(1, i);
    assert_int(sizeof(int), sizeof(Integer));

    assert_int(sizeof(int), sizeof(Base));
    Base j = C;

    return summary();
}
