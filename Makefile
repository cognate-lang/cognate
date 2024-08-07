CC=cc
CFLAGS=-Og -ggdb3 -g -rdynamic -Wall -Wpedantic
PREFIX=`echo ~`/.local
BINDIR=$(PREFIX)/bin
TESTS=$(basename $(wildcard tests/*.cog))

cognac: src/cognac.h src/cognac.c src/parser.c src/parser.h src/lexer.c src/runtime.h src/prelude.h src/builtins.c
	$(CC) $(CFLAGS) src/lexer.c src/parser.c src/cognac.c -o cognac -DCC=$(CC)

install: cognac
	mkdir -p $(BINDIR)
	cp cognac $(BINDIR)/cognac

uninstall:
	rm -rf $(BINDIR)/cognac

src/runtime.h: src/runtime.c
	xxd -i src/runtime.c > src/runtime.h

src/prelude.h: src/prelude.cog
	xxd -i src/prelude.cog src/prelude.h

src/lexer.c: src/lexer.l
	flex -o src/lexer.c src/lexer.l

src/parser.c src/parser.h: src/parser.y
	bison src/parser.y --defines=src/parser.h -o src/parser.c

clean:
	rm src/lexer.c src/parser.c src/parser.h cognac src/runtime.h

test: $(TESTS)

$(TESTS): cognac
	@rm -f $@.log $@.c $@
	./cognac $@.cog > $@.log
	./$@ >> $@.log
	@! grep "^FAIL" $@.log --color
