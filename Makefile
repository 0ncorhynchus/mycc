CFLAGS=-std=c11 -g -static -Wall

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

TEST_SRCS=$(filter-out tests/utils.c, $(wildcard tests/*.c))
TESTS=$(TEST_SRCS:.c=.out)

mycc: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): mycc.h

test: $(TESTS)
	for t in $^; do echo $$t; ./$$t || exit 1; done

tests/%.out: tests/%.s tests/utils.o
	$(CC) -o $@ $^

tests/%.s: tests/%.c mycc
	./mycc $< > $@ || exit 1

clean:
	rm -f mycc $(OBJS) $(TESTS)

fmt:
	clang-format -i mycc.h $(SRCS)

.PHONY: test clean fmt
