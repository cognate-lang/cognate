CC=clang
CFLAGS=-Wall -Wextra -pedantic -fblocks -Ofast -flto
PREFIX=/usr/local
INCLUDEDIR=$(PREFIX)/include/cognate
LIBDIR=$(PREFIX)/lib
BINDIR=$(PREFIX)/bin

build: cognac libcognate.a

install: build
	mkdir -p $(INCLUDEDIR) $(LIBDIR) $(BINDIR)
	cp runtime/runtime.h $(INCLUDEDIR)/runtime.h
	cp libcognate.a      $(LIBDIR)/libcognate.a
	cp cognac            $(BINDIR)/cognac

uninstall:
	rm -rf $(INCLUDEDIR) $(LIBDIR)/libcognate.a $(BINDIR)/cognac

cognac: compiler/lexer.c compiler/parser.c compiler/parser.h compiler/cognac.c compiler/builtins.c compiler/cognac.h
	$(CC) compiler/lexer.c compiler/parser.c compiler/cognac.c -o cognac -DLIBDIR=\"$(LIBDIR)\" -DINCLUDEDIR=\"$(INCLUDEDIR)\" -lgc $(CFLAGS)

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
	rm -f compiler/lexer.c compiler/parser.h compiler/parser.c cognac libcognate.a runtime.o functions.o
