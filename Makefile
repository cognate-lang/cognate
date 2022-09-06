CC=gcc
CFLAGS=-Og -ggdb3 -g -rdynamic
TESTS=block booleans filter for functions if io lists map maths parsing regex stack strings symbols variables trig other-math

cognac: cognac.h cognac.c parser.c runtime.h
	$(CC) $(CFLAGS) cognac.c parser.c -o cognac

runtime.h: runtime.c
	xxd -i runtime.c > runtime.h

parser.c: parse.peg
	packcc -o parser parse.peg

clean:
	rm parser.c parser.h cognac runtime.h

test: $(TESTS)
	@grep -E "^(PASS|FAIL)" tests/*.log --color
	@echo "****************************** TESTS THAT PASSED ******************************"
	@grep -c "^PASS" tests/*.log --color || true
	@echo "****************************** TESTS THAT FAILED ******************************"
	@! grep -c "^FAIL" tests/*.log --color

$(TESTS): cognac
	@rm -f tests/$@.log
	./cognac tests/$@.cog > tests/$@.log
	./a.out >> tests/$@.log
