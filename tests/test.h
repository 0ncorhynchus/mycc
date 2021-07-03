#pragma once

int
alloc4(int **p, int, int, int, int);

void
assert_int(int expected, int actual);

void
assert_unsigned_int(unsigned int expected, unsigned int actual);

long
assert_long(long expected, long actual);

int
summary();
