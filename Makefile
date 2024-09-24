CC=cc
CFLAGS=-Og -ggdb3 -g -rdynamic -Wall -Wpedantic
PREFIX=`echo ~`/.local
BINDIR=$(PREFIX)/bin
TESTS=$(basename $(wildcard tests/*.cog))

cognac: src/cognac.h src/cognac.c src/parser.c src/parser.h src/lexer.c src/runtime_bytes.h src/prelude.h src/builtins.c
	$(CC) $(CFLAGS) src/lexer.c src/parser.c src/cognac.c -o cognac -DCC=$(CC)

install: cognac
	mkdir -p $(BINDIR)
	cp cognac $(BINDIR)/cognac

uninstall:
	rm -rf $(BINDIR)/cognac

src/runtime_bytes.h: src/runtime.h
	xxd -i src/runtime.h > src/runtime_bytes.h

src/prelude.h: src/prelude.cog
	xxd -i src/prelude.cog > src/prelude.h

src/lexer.c: src/lexer.l
	flex -o src/lexer.c src/lexer.l

src/parser.c src/parser.h: src/parser.y
	bison src/parser.y --defines=src/parser.h -o src/parser.c

clean:
	rm src/lexer.c src/parser.c src/parser.h cognac src/runtime_bytes.h

test: $(TESTS)

$(TESTS): cognac
	@rm -f $@.log $@.c $@
	./cognac $@.cog > $@.log
	./$@ >> $@.log
	./cognac $@.cog -debug > $@-debug.log
	./$@ >> $@-debug.log
	./cognac $@.cog -GCTEST > $@-GCTEST.log
	./$@ >> $@-GCTEST.log
	./cognac $@.cog -NOINLINE > $@-NOINLINE.log
	./$@ >> $@-NOINLINE.log
	./cognac $@.cog -GCTEST -NOINLINE > $@-BOTH.log
	./$@ >> $@-BOTH.log
	@! grep "^FAIL" $@.log --color
	@! grep "^FAIL" $@-debug.log --color
	@! grep "^FAIL" $@-GCTEST.log --color
	@! grep "^FAIL" $@-NOINLINE.log --color
	@! grep "^FAIL" $@-BOTH.log --color
