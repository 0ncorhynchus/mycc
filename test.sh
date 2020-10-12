#!/bin/bash

compile() {
  output="$1"
  input="$2"

  echo "$input" | cc -c -x c -o "$output" -
}

assert() {
  expected="$1"
  input="$2"
  lib="$3" # optional

  ./mycc "$input" > tmp.s && cc -o tmp tmp.s $lib
  if [ "$?" -ne 0 ]; then
    echo >&2 "Failed to compile:"
    echo >&2 "    $input"
    exit 1
  fi

  ./tmp
  actual="$?"

  if [ "$actual" == "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert_main() {
  expected="$1"
  input="$2"
  lib="$3" # optional
  assert "$expected" "int main() { $input }" $lib
}

assert_expr() {
  expected="$1"
  input="$2"
  lib="$3" # optional
  assert_main "$expected" "return $input;" $lib
}

assert_expr 0 "0"
assert_expr 42 "42"
assert_expr 21 "5+20-4"
assert_expr 41 " 12 + 34 - 5"
assert_expr 47 "5+6*7"
assert_expr 15 "5*(9-6)"
assert_expr 4 "(3+5)/2"
assert_expr 15 "3*+5"
assert_expr 10 "-10+20"

assert_expr 0 "1 == (1+1)"
assert_expr 1 "1 != (1+1)"
assert_expr 1 "1 <= (1+1)"
assert_expr 1 "1 < (1+1)"
assert_expr 0 "1 >= (1+1)"
assert_expr 0 "1 > (1+1)"

assert_main 1 "int foo; return foo = 1;"
assert_main 1 "int f00; return f00 = 1;"
assert_main 1 "int FOO; return FOO = 1;"
assert_main 1 "int _; return _ = 1;"

assert_main 1 "int foo; int bar; foo = bar = 1; return foo;"
assert_main 1 "int foo; int bar; foo = bar = 1; return bar;"
assert_main 1 "int foo; int bar; foo = 1; bar = 2; return foo;"
assert_main 2 "int foo; int bar; foo = 1; bar = 2; return bar;"

assert_main 1 "return 1; return 2;"

assert_main 0 "if (0 == 0) 18;"
assert_main 18 "if (0 == 0) return 18;"
assert_main 0 "if (0 == 1) return 18;"
assert_main 1 "if (0 == 0) return 1; else return 0;"
assert_main 0 "if (0 == 1) return 1; else return 0;"

assert_main 55 "int sum; int i; sum = 0; i = 0; while(i < 10) sum = sum + (i = i + 1); return sum;"

assert_main 55 "int sum; int i; sum = 0; for (i = 0; i < 10; i = i + 1) sum = sum + i + 1; return sum;"
assert_main 10 "int i; i = 0; for (;i < 10;) i = i + 1; return i;"

assert_main 0 "{}"
assert_main 0 "{1;}"
assert_main 0 "{1;2;}"
assert_main 3 "{1;2;return 3;}"

assert_main 144 "int i; int j; int tmp; i = 1; j = 1; while(i < 100) {tmp = i; i = i + j; j = tmp;} return i;"

compile tmp.o 'int foo() { return 57; }'
assert_expr 57 'foo()' tmp.o

compile tmp.o 'int foo(int x) { return x; }'
assert_expr 57 'foo(57)' tmp.o

compile tmp.o 'int foo(int x, int y) { return x + y; }'
assert_expr 57 'foo(3, 54)' tmp.o
assert_expr 57 'foo(54, 3)' tmp.o

compile tmp.o 'int many(int x, int y, int z, int p, int q, int r, int m, int n) { return x + y + z + p + q + r + m + n; }'
assert_expr 36 'many(1, 2, 3, 4, 5, 6, 7, 8)' tmp.o

assert 0 'int foo() { return 0; } int main() { return foo(); }'
assert 1 'int foo(int i) { return i; } int main() { return foo(1); }'
assert 1 'int foo(int i, int j) { return i; } int main() { return foo(1, 2); }'
assert 2 'int foo(int i, int j) { return j; } int main() { return foo(1, 2); }'
assert 6 'int foo(int x, int y, int z, int p, int q, int r) { return r; } int main() { return foo(1, 2, 3, 4, 5, 6); }'
# assert 6 'int foo(int x, int y, int z, int p, int q, int r, int m) { return r; } int main() { return foo(1, 2, 3, 4, 5, 6, 7); }'

assert_main 3 "int x; int *y; x = 3; y = &x; return *y;"
assert_main 3 "int x; int y; int *z; x = 3; y = 5; z = &y + 2; return *z;" # implementation dependency

assert_main 3 "int x; int *y; y = &x; *y = 3; return x;"

compile tmp.o '#include <stdlib.h>
int alloc4(int **p, int a, int b, int c, int d) {
  *p = malloc(4 * sizeof(int));
  (*p)[0] = a;
  (*p)[1] = b;
  (*p)[2] = c;
  (*p)[3] = d;
  return 0;
}'
assert_main 4 'int *p; alloc4(&p, 1, 2, 4, 8); int *q; q = p + 2; return *q;' tmp.o
assert_main 8 'int *p; alloc4(&p, 1, 2, 4, 8); int *q; q = p + 3; return *q;' tmp.o
assert_main 2 'int *p; alloc4(&p, 1, 2, 4, 8); int *q; q = p + 3; q = q - 2; return *q;' tmp.o

assert_main 4 'int x; return sizeof(x);'
assert_main 8 'int *y; return sizeof(y);'
assert_main 4 'int x; return sizeof(x + 3);'
assert_main 8 'int *y; return sizeof(y + 3);'
assert_main 4 'int *y; return sizeof(*y);'
assert_expr 4 'sizeof(1)'
assert_expr 4 'sizeof(sizeof(1))'

assert_main 20 'int x[5]; return sizeof(x);'
assert_main 4 'int x[5]; int y; y = 4; return y;'
assert_main 1 'int a[2]; *&a = 1; return *a;'
assert_main 1 'int a[2]; *a = 1; return *a;'
assert_main 2 'int a[2]; *(a + 1) = 2; return *(a + 1);'
assert_main 0 'int a[2]; int *p; p = a; *p = 0; return *p;'
assert_main 3 'int a[2]; *a = 1; *(a + 1) = 2; int *p; p = a; return *p + *(p + 1);'

echo OK
