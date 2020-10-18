int
main() {
    int x = 0;

    if (0 == 0)
        x = 1;
    assert_int(1, x);

    if (0 == 1)
        x = 2;
    assert_int(1, x);

    if (0 == 0)
        x = 3;
    else
        x = 4;
    assert_int(3, x);

    if (0 == 1)
        x = 5;
    else
        x = 6;
    assert_int(6, x);

    int sum = 0;
    int i = 0;

    while (i < 10) {
        i = i + 1;
        sum = sum + i;
    }
    assert_int(10, i);
    assert_int(55, sum);

    sum = 0;
    for (i = 0; i < 10; i = i + 1) {
        sum = sum + i + 1;
    }
    assert_int(10, i);
    assert_int(55, sum);

    i = 0;
    for (; i < 10;)
        i = i + 1;
    assert_int(10, i);

    return summary();
}
