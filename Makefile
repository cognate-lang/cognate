CC=clang

cognac: compiler/lexer.c compiler/parser.c runtime/runtime.o runtime/functions.o
	$(CC) compiler/lexer.c compiler/parser.c compiler/cognac.c -Ofast -o cognac

runtime/%.o: runtime/%.c
	$(CC) -c -fblocks -Ofast -flto -o $@ $<

compiler/lexer.c: compiler/lexer.l
	flex -o compiler/lexer.c compiler/lexer.l

compiler/parser.c: compiler/parser.y
	bison compiler/parser.y --defines=compiler/parser.h -o compiler/parser.c

clean:
	rm -f compiler/lexer.c compiler/parser.h compiler/parser.c cognac runtime/runtime.o runtime/functions.o
