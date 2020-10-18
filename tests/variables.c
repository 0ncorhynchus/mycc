int spam;
int a = 3;
char b[] = "foobar";
int *c = &a;
char *d = b + 3;

int
main() {
    int foo;
    assert_int(1, foo = 1);

    int f00;
    assert_int(1, f00 = 1);

    int FOO;
    assert_int(1, FOO = 1);

    int _;
    assert_int(1, _ = 1);

    int bar;
    foo = bar = 1;
    assert_int(1, foo);
    assert_int(1, bar);
    foo = 1;
    bar = 2;
    assert_int(1, foo);
    assert_int(2, bar);

    assert_int(0, spam);
    spam = 1;
    assert_int(1, spam);

    assert_int(3, a);
    assert_int(3, *c);
    assert_int(7, sizeof(b));
    assert_int(102, b[0]);
    assert_int(98, *d);

    int x = 1;
    assert_int(1, x);

    int y[] = {1, 2, 3};

    assert_int(12, sizeof(y));
    assert_int(1, y[0]);
    assert_int(2, y[1]);
    assert_int(3, y[2]);

    int z[5] = {1, 2, 3};
    assert_int(20, sizeof(z));
    assert_int(1, z[0]);
    assert_int(2, z[1]);
    assert_int(3, z[2]);
    assert_int(0, z[3]);
    assert_int(0, z[4]);

    char w[] = "abc";
    assert_int(4, sizeof(w));
    assert_int(97, w[0]);
    assert_int(98, w[1]);
    assert_int(99, w[2]);

    return summary();
}
