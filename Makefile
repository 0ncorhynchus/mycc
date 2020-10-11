CFLAGS=-std=c11 -g -static
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

mycc: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): mycc.h

test: mycc foo-test
	./test.sh
	./foo-test

clean:
	rm -f mycc *.o *~ tmp*

fmt:
	clang-format -i mycc.h $(SRCS)

foo.s: mycc
	./mycc 'foo();' > $@

foo-test: foo.o foo.s
	$(CC) -o $@ foo.s foo.o

.PHONY: test clean fmt
