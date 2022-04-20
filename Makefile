CC=clang
CFLAGS=-Wall -Wextra -pedantic -fblocks -Ofast -flto
PREFIX=`echo ~`/.local
INCLUDEDIR=$(PREFIX)/include
LIBDIR=$(PREFIX)/lib
BINDIR=$(PREFIX)/bin
TESTS=block booleans filter for functions if io lists map maths parallel parsing regex stack strings symbols variables

build: cognac libcognate.a

io-disabled: cognac-io-disabled libcognate.a

install: build
	mkdir -p $(INCLUDEDIR)/cognate $(LIBDIR) $(BINDIR)
	cp runtime/runtime.h $(INCLUDEDIR)/cognate/runtime.h
	cp libcognate.a      $(LIBDIR)/libcognate.a
	cp cognac            $(BINDIR)/cognac

uninstall:
	rm -rf $(INCLUDEDIR)/cognate $(LIBDIR)/libcognate.a $(BINDIR)/cognac

cognac: compiler/lexer.c compiler/parser.c compiler/parser.h compiler/cognac.c compiler/builtins.c compiler/cognac.h
	$(CC) compiler/lexer.c compiler/parser.c compiler/cognac.c -o cognac -DLIBDIR=\"$(LIBDIR)\" -DINCLUDEDIR=\"$(INCLUDEDIR)\" $(CFLAGS)

cognac-io-disabled: compiler/lexer.c compiler/parser.c compiler/parser.h compiler/cognac.c compiler/builtins.c compiler/cognac.h
	$(CC) compiler/lexer.c compiler/parser.c compiler/cognac.c -o cognac -DLIBDIR=\"$(LIBDIR)\" -DINCLUDEDIR=\"$(INCLUDEDIR)\" -DDISABLEIO $(CFLAGS)

libcognate.a: runtime/runtime.c runtime/functions.c runtime/gc.c runtime/runtime.h
	$(CC) -c $(CFLAGS) -o runtime.o runtime/runtime.c
	$(CC) -c $(CFLAGS) -o functions.o runtime/functions.c
	$(CC) -c $(CFLAGS) -o gc.o runtime/gc.c
	llvm-ar rvs libcognate.a runtime.o functions.o gc.o

compiler/lexer.c: compiler/lexer.l
	flex -o compiler/lexer.c compiler/lexer.l

compiler/parser.c compiler/parser.h: compiler/parser.y
	bison compiler/parser.y --defines=compiler/parser.h -o compiler/parser.c

clean:
	rm -f compiler/lexer.c compiler/parser.h compiler/parser.c cognac libcognate.a runtime.o functions.o gc.o

test: build $(TESTS)
	@grep -E "^(PASS|FAIL)" tests/*.log --color
	@echo "****************************** TESTS THAT PASSED ******************************"
	@grep -c "^PASS" tests/*.log --color || true
	@echo "****************************** TESTS THAT FAILED ******************************"
	@! grep -c "^FAIL" tests/*.log --color

$(TESTS):
	cognac tests/$@.cog -run > tests/$@.log
