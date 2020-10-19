#include "test.h"

typedef struct Span Span;
struct Span {
    char *ptr;
    int len;
};

int
main() {
    assert_int(16, sizeof(struct Span));
    assert_int(16, sizeof(Span));

    return summary();
}
