CFLAGS=-std=c11 -g -static -Wall
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

mycc: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): mycc.h

test: mycc
	./test.sh

clean:
	rm -f mycc *.o *~ tmp*

fmt:
	clang-format -i mycc.h $(SRCS)

.PHONY: test clean fmt
