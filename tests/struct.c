#include "test.h"

struct Span {
    char *ptr;
    int len;
};

int
main() {
    assert_int(16, sizeof(struct Span));

    return summary();
}
