CFLAGS=-std=c11 -g -static -Wall
SRCS=$(wildcard *.c)
OBJS=$(filter-out test%.o, $(SRCS:.c=.o))

mycc: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): mycc.h

test: mycc test.out
	./test.out

test.out: test.s test_utils.c
	cc -o $@ $^

test.s: test.c mycc
	$(eval tmp := $(shell mktemp))
	./mycc $< > $(tmp)
	mv $(tmp) $@

clean:
	rm -f mycc *.s *.o *.out tmp*

fmt:
	clang-format -i mycc.h $(SRCS)

.PHONY: test clean fmt
