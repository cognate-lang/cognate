CC=clang

build: cognac runtime/functions.o runtime/runtime.o

cognac: compiler/lexer.c compiler/parser.c compiler/parser.h compiler/cognac.c compiler/builtins.c compiler/cognac.h
	$(CC) compiler/lexer.c compiler/parser.c compiler/cognac.c -Ofast -o cognac -lgc -Wall -Wextra -Werror -pedantic-errors

runtime/%.o: runtime/%.c runtime/runtime.h
	$(CC) -c -Wall -Wextra -Werror -pedantic-errors -fblocks -Ofast -flto -o $@ $<

compiler/lexer.c: compiler/lexer.l
	flex -o compiler/lexer.c compiler/lexer.l

compiler/parser.c compiler/parser.h: compiler/parser.y
	bison compiler/parser.y --defines=compiler/parser.h -o compiler/parser.c

clean:
	rm -f compiler/lexer.c compiler/parser.h compiler/parser.c cognac runtime/runtime.o runtime/functions.o

test: build
	./TEST
