CC=clang
CFLAGS=-Wall -Wextra -Wpedantic -Og -g
PREFIX=`echo ~`/.local
BINDIR=$(PREFIX)/bin
TESTS=block booleans filter for functions if io lists map maths parsing regex stack strings symbols variables

build: cognac

install: build
	mkdir -p $(BINDIR)
	cp cognac $(BINDIR)/cognac

uninstall:
	rm -rf $(BINDIR)/cognac

cognac: src/lexer.c src/parser.c src/parser.h src/cognac.c src/builtins.c src/cognac.h src/
	xxd -i src/runtime.c > src/runtime.h
	$(CC) src/lexer.c src/parser.c src/cognac.c -o cognac $(CFLAGS)

src/lexer.c: src/lexer.l
	flex -o src/lexer.c src/lexer.l

src/parser.c src/parser.h: src/parser.y
	bison src/parser.y --defines=src/parser.h -o src/parser.c

clean:
	rm -f src/lexer.c src/parser.h src/parser.c cognac

test: $(TESTS)
	@grep -E "^(PASS|FAIL)" tests/*.log --color
	@echo "****************************** TESTS THAT PASSED ******************************"
	@grep -c "^PASS" tests/*.log --color || true
	@echo "****************************** TESTS THAT FAILED ******************************"
	@! grep -c "^FAIL" tests/*.log --color

$(TESTS): build
	@rm -f tests/$@.log
	./cognac tests/$@.cog -run > tests/$@.log
