CC=gcc
CFLAGS=-Og -ggdb3 -g -rdynamic

cognac: cognac.h cognac.c parser.c runtime.h
	$(CC) $(CFLAGS) cognac.c parser.c -o cognac

runtime.h: runtime.c
	xxd -i runtime.c > runtime.h

parser.c: parse.peg
	packcc -o parser parse.peg

clean:
	rm parser.c parser.h cognac runtime.h
