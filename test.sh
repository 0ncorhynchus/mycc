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
  assert "$expected" "main() { $input }" $lib
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

assert_expr 1 "foo = 1"
assert_expr 1 "f00 = 1"
assert_expr 1 "FOO = 1"
assert_expr 1 "_ = 1"

assert_main 1 "foo = bar = 1; return foo;"
assert_main 1 "foo = bar = 1; return bar;"

assert_main 1 "return 1; return 2;"

assert_main 0 "if (0 == 0) 18;"
assert_main 18 "if (0 == 0) return 18;"
assert_main 0 "if (0 == 1) return 18;"
assert_main 1 "if (0 == 0) return 1; else return 0;"
assert_main 0 "if (0 == 1) return 1; else return 0;"

assert_main 55 "sum = 0; i = 0; while(i < 10) sum = sum + (i = i + 1); return sum;"

assert_main 55 "sum = 0; for (i = 0; i < 10; i = i + 1) sum = sum + i + 1; return sum;"
assert_main 10 "i = 0; for (;i < 10;) i = i + 1; return i;"

assert_main 0 "{}"
assert_main 0 "{1;}"
assert_main 0 "{1;2;}"
assert_main 3 "{1;2;return 3;}"

assert_main 144 "i = 1; j = 1; while(i < 100) {tmp = i; i = i + j; j = tmp;} return i;"

compile tmp.o 'int foo() { return 57; }'
assert_expr 57 'foo()' tmp.o

compile tmp.o 'int foo(int x) { return x; }'
assert_expr 57 'foo(57)' tmp.o

compile tmp.o 'int foo(int x, int y) { return x + y; }'
assert_expr 57 'foo(3, 54)' tmp.o
assert_expr 57 'foo(54, 3)' tmp.o

compile tmp.o 'int many(int x, int y, int z, int p, int q, int r, int m, int n) { return x + y + z + p + q + r + m + n; }'
assert_expr 36 'many(1, 2, 3, 4, 5, 6, 7, 8)' tmp.o

assert 0 'foo() { return 0; } main() { return foo(); }'
assert 1 'foo(i) { return i; } main() { return foo(1); }'
assert 1 'foo(i, j) { return i; } main() { return foo(1, 2); }'
assert 2 'foo(i, j) { return j; } main() { return foo(1, 2); }'
assert 6 'foo(x, y, z, p, q, r) { return r; } main() { return foo(1, 2, 3, 4, 5, 6); }'
assert 6 'foo(x, y, z, p, q, r, m) { return r; } main() { return foo(1, 2, 3, 4, 5, 6, 7); }'

echo OK
