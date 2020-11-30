#include "test.h"

typedef struct Span Span;
struct Span {
    char *ptr;
    int len;
};

struct Span2 {
    int len;
    char *ptr;
};

union Union {
    int integer;
    char character;
};

struct Anonymous {
    int i;
    struct {
        int j;
        int k;
    };
};

int
main() {
    assert_int(16, sizeof(struct Span));
    assert_int(16, sizeof(Span));

    Span span = {1, 1};
    assert_int(1, span.ptr);
    assert_int(1, span.len);

    span.ptr = 1;
    assert_int(1, span.ptr);
    span.len = 2;
    assert_int(2, span.len);

    assert_int(16, sizeof(struct Span2));

    struct Span2 span2 = {1, 1};
    assert_int(1, span2.ptr);
    assert_int(1, span2.len);

    span2.ptr = 1;
    assert_int(1, span2.ptr);
    span2.len = 2;
    assert_int(2, span2.len);

    union Union u = {257};
    assert_int(257, u.integer);
    assert_int(1, (int)u.character);

    struct Anonymous a = {0, 1, 2};
    assert_int(0, a.i);
    assert_int(1, a.j);
    assert_int(2, a.k);

    return summary();
}
