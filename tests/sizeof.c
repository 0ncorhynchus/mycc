int
main() {
    int x;
    int *y;
    int z[5];

    assert_int(4, sizeof(x));
    assert_int(8, sizeof(y));
    assert_int(4, sizeof(x + 3));
    assert_int(8, sizeof(y + 3));
    assert_int(4, sizeof(*y));
    assert_int(4, sizeof(1));
    /* assert_int(8, sizeof(sizeof(1))); */
    assert_int(20, sizeof(z));

    assert_int(4, sizeof(int));
    assert_int(8, sizeof(int *));
    assert_int(20, sizeof(int[5]));

    return summary();
}
