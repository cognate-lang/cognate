CC=clang

cognac: lex.yy.c parser.tab.c runtime.o functions.o
	$(CC) lex.yy.c parser.tab.c cognac.c -Ofast -o cognac

%.o: %.c
	$(CC) -c -fblocks -Ofast -flto -o $@ $<

lex.yy.c: lexer.l
	flex lexer.l

parser.tab.c: parser.y
	bison parser.y -d

clean:
	rm -f lex.yy.c parser.tab.h parser.tab.c cognac runtime.o functions.o
