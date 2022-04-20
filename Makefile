CC=clang
CFLAGS=-Wall -Wextra -pedantic -fblocks -Ofast
PREFIX=`echo ~`/.local
INCLUDEDIR=$(PREFIX)/include
BINDIR=$(PREFIX)/bin
TESTS=block booleans filter for functions if io lists map maths parallel parsing regex stack strings symbols variables

build: cognac

io-disabled: cognac-io-disabled

install: build
	mkdir -p $(INCLUDEDIR) $(LIBDIR) $(BINDIR)
	cp src/cognate.h $(INCLUDEDIR)/cognate.h
	cp cognac        $(BINDIR)/cognac

uninstall:
	rm -rf $(INCLUDEDIR)/cognate.h $(BINDIR)/cognac

cognac: src/lexer.c src/parser.c src/parser.h src/cognac.c src/builtins.c src/cognac.h
	$(CC) src/lexer.c src/parser.c src/cognac.c -o cognac -DINCLUDEDIR=\"$(INCLUDEDIR)\" $(CFLAGS)

cognac-io-disabled: src/lexer.c src/parser.c src/parser.h src/cognac.c src/builtins.c src/cognac.h
	$(CC) src/lexer.c src/parser.c src/cognac.c -o cognac -DINCLUDEDIR=\"$(INCLUDEDIR)\" -DDISABLEIO $(CFLAGS)

src/lexer.c: src/lexer.l
	flex -o src/lexer.c src/lexer.l

src/parser.c src/parser.h: src/parser.y
	bison src/parser.y --defines=src/parser.h -o src/parser.c

clean:
	rm -f src/lexer.c src/parser.h src/parser.c cognac

test: build $(TESTS)
	@grep -E "^(PASS|FAIL)" tests/*.log --color
	@echo "****************************** TESTS THAT PASSED ******************************"
	@grep -c "^PASS" tests/*.log --color || true
	@echo "****************************** TESTS THAT FAILED ******************************"
	@! grep -c "^FAIL" tests/*.log --color

$(TESTS):
	cognac tests/$@.cog -run > tests/$@.log
