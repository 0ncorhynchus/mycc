int
fibo(int index) {
    int a = 1;
    int b = 0;
    int i;
    int tmp;
    for (i = 0; i < index; i = i + 1) {
        tmp = a;
        a = a + b;
        b = tmp;
    }
    return a;
}

void
foo() {
    return;
}

int
args6(int x, int y, int z, int p, int q, int r) {
    return r;
}
// int args7(int x, int y, int z, int p, int q, int r, int m) { return r; }

int
main() {
    assert_int(1, fibo(0));
    assert_int(1, fibo(1));
    assert_int(2, fibo(2));
    assert_int(3, fibo(3));
    assert_int(5, fibo(4));
    assert_int(8, fibo(5));
    assert_int(13, fibo(6));
    assert_int(21, fibo(7));
    assert_int(34, fibo(8));
    assert_int(55, fibo(9));

    assert_int(36, many(1, 2, 3, 4, 5, 6, 7, 8));
    assert_int(6, args6(1, 2, 3, 4, 5, 6));

    return summary();
}
