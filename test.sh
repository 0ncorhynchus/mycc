#!/bin/bash

assert() {
  expected="$1"
  input="$2"

  ./mycc "$input" > tmp.s
  cc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" == "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 "0;"
assert 42 "42;"
assert 21 "5+20-4;"
assert 41 " 12 + 34 - 5; "
assert 47 "5+6*7;"
assert 15 "5*(9-6);"
assert 4 "(3+5)/2;"
assert 15 "3*+5;"
assert 10 "-10+20;"

assert 0 "1 == (1+1);"
assert 1 "1 != (1+1);"
assert 1 "1 <= (1+1);"
assert 1 "1 < (1+1);"
assert 0 "1 >= (1+1);"
assert 0 "1 > (1+1);"

assert 1 "foo = 1;"
assert 1 "f00 = 1;"
assert 1 "FOO = 1;"
assert 1 "_ = 1;"
assert 1 "foo = bar = 1; foo;"
assert 1 "foo = bar = 1; bar;"

assert 1 "return 1; 2;"

assert 0 "if (0 == 0) 18;"
assert 18 "if (0 == 0) return 18;"
assert 0 "if (0 == 1) return 18;"
assert 1 "if (0 == 0) return 1; else return 0;"
assert 0 "if (0 == 1) return 1; else return 0;"

assert 55 "sum = 0; i = 0; while(i < 10) sum = sum + (i = i + 1); sum;"

assert 55 "sum = 0; for (i = 0; i < 10; i = i + 1) sum = sum + i + 1; sum;"
assert 10 "i = 0; for (;i < 10;) i = i + 1; i;"

echo OK
