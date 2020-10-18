int
main() {
    int x = 3;
    int *y = &x;

    assert_int(3, *y);

    *y = 3;
    assert_int(3, x);

    int *p;
    alloc4(&p, 1, 2, 4, 8);
    int *q;

    q = p + 2;
    assert_int(4, *q);

    q = p + 3;
    assert_int(8, *q);

    q = q - 2;
    assert_int(2, *q);

    return summary();
}
