int main() {
    char x[3] = {-1, 2, 4};
    int y = 4;

    assert_int(3, x[0] + y);

    char z[16];
    int i;
    for (i = 0; i < 16; i = i + 1) {
        z[i] = i;
    }
    for (i = 0; i < 16; i = i + 1) {
        assert_int(i, z[i]);
    }

    for (i = 15; i >= 0; i = i - 1) {
        z[i] = i;
    }
    for (i = 0; i < 16; i = i + 1) {
        assert_int(i, z[i]);
    }

    char *w = "abc";
    assert_int(97, w[0]);
    assert_int(98, w[1]);
    assert_int(99, w[2]);
    assert_int(0, w[3]);

    return summary();
}
