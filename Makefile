CFLAGS=-std=c11 -g -static -Wall

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

TEST_SRCS=$(filter-out tests/utils.c, $(wildcard tests/*.c))
TESTS=$(TEST_SRCS:.c=.out)
VTESTS=$(TEST_SRCS:.c=.exe)

mycc: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): mycc.h

.PHONY: test clean fmt validate_tests
clean:
	rm -f mycc $(OBJS) $(TESTS) $(VTESTS)

fmt:
	clang-format -i mycc.h $(SRCS) tests/test.h $(TEST_SRCS)

#
# Tests
#
test: $(TESTS)
	for t in $^; do echo; echo $$t; ./$$t || exit 1; done

tests/utils.o: tests/test.h

tests/%.out: tests/%.s tests/utils.o
	$(CC) -o $@ $^

tests/%.s: tests/%.c tests/test.h mycc
	cpp -undef -std=c11 $< | ./mycc - > $@ || exit 1

#
# Validation tests with cc
#
validate_tests: $(VTESTS)
	for t in $^; do echo; echo $$t; ./$$t || exit 1; done

tests/%.exe: tests/%.c tests/utils.o
	$(CC) -o $@ $^
