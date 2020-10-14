int test_no;
int num_passed;
int num_failed;

int assert_int(int expected, int actual) {
    test_no = test_no + 1;
    if (expected == actual) {
        num_passed = num_passed + 1;
    } else {
        num_failed = num_failed + 1;
        printf("%03d: ", test_no);
        printf("Failed. %d expected, but got %d\n", expected, actual);
        printf("     expected: 0x%08X\n", expected);
        printf("     actual:   0x%08X\n", actual);
    }
    return 0;
}

int test_vars() {
    int foo;
    assert_int(1, foo = 1);
    int f00;
    assert_int(1, f00 = 1);
    int FOO;
    assert_int(1, FOO = 1);
    int _;
    assert_int(1, _ = 1);
    return 0;
}

int test_vars2() {
    int foo;
    int bar;
    foo = bar = 1;
    assert_int(1, foo);
    assert_int(1, bar);
    foo = 1;
    bar = 2;
    assert_int(1, foo);
    assert_int(2, bar);
    return 0;
}

int test_return() {
    return 1;
    return 2;
}

int test_if() {
    if (0 == 0)
        return 18;
    return 0;
}

int test_if2() {
    if (0 == 1)
        return 18;
    return 0;
}

int test_if3() {
    if (0 == 0)
        return 1;
    else
        return 0;
}

int test_if4() {
    if (0 == 1)
        return 1;
    else
        return 0;
}

int test_while_sum() {
    int sum;
    int i;
    sum = 0;
    i = 0;
    while (i < 10)
        sum = sum + (i = i + 1);
    return sum;
}

int test_for_sum() {
    int sum;
    int i;
    sum = 0;
    for (i = 0; i < 10; i = i + 1)
        sum = sum + i + 1;
    return sum;
}

int test_for() {
    int i;
    i = 0;
    for (; i < 10;)
        i = i + 1;
    return i;
}

int test_block() {
    {} // check compile is passed
    {
        1;
    }
    {
        1;
        2;
    }
    {
        1;
        2;
        return 3;
    }
}

int fibo(int threhold) {
    int i;
    int j;
    int tmp;
    i = 1;
    j = 1;
    while (i < threhold) {
        tmp = i;
        i = i + j;
        j = tmp;
    }
    return i;
}

int args6(int x, int y, int z, int p, int q, int r) { return r; }
// int args7(int x, int y, int z, int p, int q, int r, int m) { return r; }

int test_deref_addr() {
    int x;
    int *y;
    x = 3;
    y = &x;
    return *y;
}

int test_deref_addr2() {
    int x;
    int *y;
    y = &x;
    *y = 3;
    return x;
}

int test_implement_dependency_case() {
    int x;
    int y;
    int *z;
    x = 3;
    y = 5;
    z = &y + 2;
    return *z;
}

int test_alloc_alignment() {
    int *p;
    alloc4(&p, 1, 2, 4, 8);
    int *q;
    q = p + 2;
    return *q;
}

int test_alloc_alignment2() {
    int *p;
    alloc4(&p, 1, 2, 4, 8);
    int *q;
    q = p + 3;
    return *q;
}

int test_alloc_alignment3() {
    int *p;
    alloc4(&p, 1, 2, 4, 8);
    int *q;
    q = p + 3;
    q = q - 2;
    return *q;
}

int test_sizeof() {
    int x;
    int *y;

    assert_int(4, sizeof(x));
    assert_int(8, sizeof(y));
    assert_int(4, sizeof(x + 3));
    assert_int(8, sizeof(y + 3));
    assert_int(4, sizeof(*y));
    assert_int(4, sizeof(1));
    assert_int(4, sizeof(sizeof(1)));

    return 0;
}

int test_array() {
    int x[5];
    int a[2];
    int *p;

    assert_int(20, sizeof(x));

    *&a = 1;
    assert_int(1, *a);
    *(a + 1) = 2;
    assert_int(2, *(1 + a));
    *(1 + a) = 2;
    assert_int(2, *(a + 1));
    p = a;
    *p = 0;
    assert_int(0, *p);
    *a = 1;
    *(a + 1) - 2;
    p = a;
    assert_int(3, *p + *(p + 1));

    return 0;
}

int test_index_access() {
    int a[2];
    a[0] = 1;
    a[1] = 2;

    assert_int(1, a[0]);
    assert_int(2, a[1]);
    assert_int(2, a[1]);
    assert_int(2, +a[1]);
    assert_int(1, -a[1] + 3);
    assert_int(4, sizeof(a[1]));

    return 0;
}

int foo;
int test_global_var() {
    assert_int(0, foo);
    foo = 1;
    assert_int(1, foo);
    return 0;
}

int test_char() {
    char x[3];
    int y;
    x[0] = -1;
    x[1] = 2;
    y = 4;

    assert_int(3, x[0] + y);

    return 0;
}

int test_string() {
    char x[16];
    int i;
    for (i = 0; i < 16; i = i + 1) {
        x[i] = i;
    }
    for (i = 0; i < 16; i = i + 1) {
        assert_int(i, x[i]);
    }

    for (i = 15; i >= 0; i = i - 1) {
        x[i] = i;
    }
    for (i = 0; i < 16; i = i + 1) {
        assert_int(i, x[i]);
    }

    char *y;
    y = "abc";
    assert_int(97, y[0]);

    return 0;
}

int summary() {
    printf("SUMMARY:\n");
    printf("    %d tests passed.\n", num_passed);
    printf("    %d tests failed.\n", num_failed);
    return num_failed > 0;
}

int main() {
    test_no = 0;
    num_passed = 0;
    num_failed = 0;

    assert_int(0, 0);
    assert_int(42, 42);
    assert_int(21, 5 + 20 - 4);
    assert_int(41, 12 + 34 - 5);
    assert_int(47, 5 + 6 * 7);
    assert_int(15, 5 * (9 - 6));
    assert_int(4, (3 + 5) / 2);
    assert_int(15, 3 * +5);
    assert_int(10, -10 + 20);

    assert_int(0, 1 == (1 + 1));
    assert_int(1, 1 != (1 + 1));
    assert_int(1, 1 <= (1 + 1));
    assert_int(1, 1 < (1 + 1));
    assert_int(0, 1 >= (1 + 1));
    assert_int(0, 1 > (1 + 1));

    test_vars();
    test_vars2();
    assert_int(1, test_return());

    assert_int(18, test_if());
    assert_int(0, test_if2());
    assert_int(1, test_if3());
    assert_int(0, test_if4());

    assert_int(55, test_while_sum());
    assert_int(55, test_for_sum());
    assert_int(10, test_for());

    assert_int(3, test_block());

    assert_int(144, fibo(100));
    assert_int(36, many(1, 2, 3, 4, 5, 6, 7, 8));
    assert_int(6, args6(1, 2, 3, 4, 5, 6));

    assert_int(3, test_deref_addr());
    assert_int(3, test_deref_addr2());
    assert_int(3, test_implement_dependency_case());

    assert_int(4, test_alloc_alignment());
    assert_int(8, test_alloc_alignment2());
    assert_int(2, test_alloc_alignment3());

    test_sizeof();
    test_array();
    test_index_access();
    test_global_var();
    test_char();
    test_string();

    return summary();
}
