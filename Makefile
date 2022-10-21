CC=gcc
CFLAGS=-Og -ggdb3 -g -rdynamic
TESTS=block booleans filter for functions if io lists map maths parsing regex stack strings symbols variables trig other-math

cognac: cognac.h cognac.c parser.c parser.h lexer.c runtime.h
	$(CC) $(CFLAGS) lexer.c parser.c cognac.c -o cognac

runtime.h: runtime.c
	xxd -i runtime.c > runtime.h

lexer.c: lexer.l
	flex -o lexer.c lexer.l

parser.c parser.h: parser.y
	bison parser.y --defines=parser.h -o parser.c

clean:
	rm lexer.c parser.c parser.h cognac runtime.h

test: $(TESTS)
	@grep -E "^(PASS|FAIL)" tests/*.log --color
	@echo "****************************** TESTS THAT PASSED ******************************"
	@grep -c "^PASS" tests/*.log --color || true
	@echo "****************************** TESTS THAT FAILED ******************************"
	@! grep -c "^FAIL" tests/*.log --color

$(TESTS): cognac
	@rm -f tests/$@.log tests/$@.c tests/$@
	./cognac tests/$@.cog > tests/$@.log
	./tests/$@ >> tests/$@.log
