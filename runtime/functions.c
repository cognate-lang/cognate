#include "runtime.h"
#include <math.h>
#include <regex.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/stat.h>
#include <pthread.h>
#include <setjmp.h>

ANY VAR(if)(BLOCK cond, ANY a, ANY b)
{
	cond();
	if (unbox_boolean(pop()))
		return a;
	else
		return b;
}

void VAR(while)(BLOCK cond, BLOCK body)
{
	cond();
	while (unbox_boolean(pop()))
	{
		body();
		cond();
	}
}

void VAR(do)(BLOCK blk) { blk(); }

void VAR(put)(ANY a)	 { fputs(show_object(a, 1), stdout); fflush(stdout); }
void VAR(print)(ANY a) { puts(show_object(a, 1)); }

void VAR(puts)(BLOCK b)		{ VAR(for)(VAR(list)(b), ^{VAR(put)(pop()); }); }
void VAR(prints)(BLOCK b) { VAR(for)(VAR(list)(b), ^{VAR(put)(pop()); }); putc('\n', stdout); }

NUMBER VAR(ADD)(NUMBER a, NUMBER b) { return a + b; } // Add cannot produce NaN.

NUMBER VAR(MUL)(NUMBER a, NUMBER b)
{
	const double r = a * b;
	if unlikely(is_nan(*(long*)&r))
		throw_error_fmt("multiplication by %.14g of %.14g yields invalid result", a, b);
	return r;
}
NUMBER VAR(SUB)(NUMBER a, NUMBER b)
{
	const double r = b - a;
	if unlikely(is_nan(*(long*)&r))
		throw_error_fmt("subtraction of %.14g from %.14g yields invalid result", a, b);
	return r;
}

NUMBER VAR(DIV)(NUMBER a, NUMBER b)
{
	const double r = b / a;
	if unlikely(is_nan(*(long*)&r))
		throw_error_fmt("division by %.14g of %.14g yields invalid result", a, b);
	return r;
}

NUMBER VAR(modulo)(NUMBER a, NUMBER b)
{
	const double r = b - a * floor(b / a);
	if unlikely(is_nan(*(long*)&r))
		throw_error_fmt("modulo by %.14g of %.14g yields invalid result", a, b);
	return r;
}

NUMBER VAR(random)(NUMBER low, NUMBER high, NUMBER step)
{
	if unlikely((high - low) * step < 0 || !step) goto invalid_range;
	else if ((high - low) / step < 1) return low;
	// This is not cryptographically secure btw.
	// Since RAND_MAX may only be 2^15, we need to do this:
	const long num
		= ((long)(short)rand())
		| ((long)(short)rand() << 15)
		| ((long)(short)rand() << 30)
		| ((long)(short)rand() << 45)
		| ((long)				rand() << 60);
	const double r = low + (NUMBER)(num % (unsigned long)((high - low) / step)) * step;
	if unlikely(is_nan(*(long*)&r)) goto invalid_range;
	return r;
invalid_range:
	throw_error_fmt("invalid range %.14g..%.14g step %.14g", low, high, step);
}

void VAR(clear)(void) { stack.cache = NIL_OBJ; stack.top=stack.start; }

BOOLEAN VAR(true) =  1;
BOOLEAN VAR(false) = 0;

BOOLEAN VAR(either)(BOOLEAN a, BOOLEAN b)    { return a || b; }
BOOLEAN VAR(both)(BOOLEAN a, BOOLEAN b)      { return a && b; }
BOOLEAN VAR(oneDASHof)(BOOLEAN a, BOOLEAN b) { return a ^ b;  }
BOOLEAN VAR(not)(BOOLEAN a)                  { return !a;     }


BOOLEAN VAR(EQ)(ANY a, ANY b)  { return compare_objects(a,b); }
BOOLEAN VAR(NEQ)(ANY a, ANY b) { return !compare_objects(a,b); }
BOOLEAN VAR(GT)(NUMBER a, NUMBER b)  { return a < b; }
BOOLEAN VAR(LT)(NUMBER a, NUMBER b)  { return a > b; }
BOOLEAN VAR(GTE)(NUMBER a, NUMBER b) { return a <= b; }
BOOLEAN VAR(LTE)(NUMBER a, NUMBER b) { return a >= b; }

BOOLEAN VAR(numberQMARK)(ANY a)  { return get_type(a)==number; }
BOOLEAN VAR(listQMARK)(ANY a)    { return get_type(a)==list;   }
BOOLEAN VAR(stringQMARK)(ANY a)  { return get_type(a)==string; }
BOOLEAN VAR(blockQMARK)(ANY a)   { return get_type(a)==block;  }
BOOLEAN VAR(booleanQMARK)(ANY a) { return get_type(a)==boolean;}
BOOLEAN VAR(symbolQMARK)(ANY a)  { return get_type(a)==symbol; }
BOOLEAN VAR(integerQMARK)(ANY a) { return VAR(numberQMARK)(a) && unbox_number(a) == floor(unbox_number(a)); }

BOOLEAN VAR(match)(ANY patt, ANY obj) { return match_objects(patt,obj); }

BLOCK VAR(case)(ANY patt, BLOCK if_match, BLOCK if_not_match)
{
	return Block_copy(^{if (match_objects(patt, peek())) if_match(); else if_not_match();});
}

ANY VAR(first)(LIST lst)
{
	// Returns the first element of a list. O(1).
	if unlikely(!lst) throw_error("empty list is invalid");
	return lst->object;
}

LIST VAR(rest)(LIST lst)
{
	// Returns the tail portion of a list. O(1).
	if unlikely(!lst) throw_error("empty list is invalid");
	return lst->next;
}

STRING VAR(head)(STRING str)
{
	if unlikely(!*str) throw_error("empty string is invalid");
	return gc_strndup((char*)str, mblen(str, MB_CUR_MAX));
}

STRING VAR(tail)(STRING str)
{
	if unlikely(!*str) throw_error("empty string is invalid");
	return str + mblen(str, MB_CUR_MAX);
}

LIST VAR(push)(ANY a, LIST b)
{
	// Pushes an object from the stack onto the list's first element. O(1).
	// TODO: Better name? Inconsistent with List where pushing to the stack adds to the END.
	cognate_list* lst = gc_new (cognate_list);
	*lst = (cognate_list) {.object = a, .next = b};
	return lst;
}

BOOLEAN VAR(emptyQMARK)(LIST lst)
{
	// Returns true is a list or string is empty. O(1).
	// Can be used to to write a Length function.
	return !lst;
}

LIST VAR(list)(BLOCK expr)
{
	flush_stack_cache();
	ANYPTR tmp_stack_start = stack.start;
	stack.start = stack.top;
	// Eval expr
	expr();
	// Move to a list.
	cognate_list* lst = NULL;
	flush_stack_cache();
	size_t len = stack_length();
	for (size_t i = 0; i < len; ++i)
	{
		cognate_list* l = gc_new(cognate_list);
		l->object = stack.start[i];
		l->next = lst;
		lst = l;
	}
	stack.top = stack.start;
	stack.start = tmp_stack_start;
	return lst;
}

STRING VAR(join)(NUMBER n)
{
	// Joins a string to the end of another string.
	// Define Prefix (Swap, Suffix);
	size_t n1 = n;
	if (n != n1) throw_error_fmt("cannot join %.14g strings", n);
	const char* strings[n1];
	size_t result_size = 1;
	for (size_t i = 0; i < n1; ++i)
	{
		const char* str = unbox_string(pop());
		strings[i] = str;
		result_size += strlen(str);
	}
	char* const result = gc_malloc(result_size);
	result[0] = '\0';
	for (size_t i = 0; i < n1; ++i)
	{
		strcat(result, strings[i]);
	}
	return result;
}

NUMBER VAR(stringDASHlength)(STRING str)
{
	size_t len = 0;
	for (; *str ; str += mblen(str, MB_CUR_MAX), ++len);
	return len;
}

STRING VAR(substring)(NUMBER startf, NUMBER endf, STRING str)
{
	// O(end).
	// Only allocates a new string if it has to.
	/* TODO: Would it be better to have a simpler and more minimalist set of string functions, like lists do?
	 * The only real difference between NULL terminated strings and linked lists is that appending to strings is harder.
	 * Maybe also a 'Join N Str1 Str2 Str3 ... StrN' function.
	 */
	size_t start	= startf;
	size_t end		= endf;
	if unlikely(start != startf || end != endf || start > end) goto invalid_range;
	size_t str_size = 0;
	end -= start;
	for (;start != 0; --start)
	{
		if unlikely(!*str) goto invalid_range;
		str += mblen(str, MB_CUR_MAX);
	}
	for (;end != 0; --end)
	{
		if unlikely(str[str_size] == '\0') goto invalid_range;
		str_size += mblen(str+str_size, MB_CUR_MAX);
	}
	if unlikely(str[str_size] == '\0')
	{
		// We don't need to make a new string here.
		return str;
	}
	return gc_strndup((char*)str, str_size + 1);
invalid_range:
	throw_error_fmt("invalid range %.14g..%.14g", startf, endf);
}


STRING VAR(input)(void)
{
	// Read user input to a string.
	size_t size = 0;
	char* buf;
	size_t chars = getline(&buf, &size, stdin);
	char* ret = gc_strndup(buf, chars-1); // Don't copy trailing newline.
	free(buf);
	return ret;
}

STRING VAR(read)(STRING filename)
{
	// Read a file to a string.
	FILE *fp = fopen(filename, "ro");
	if unlikely(fp == NULL) throw_error_fmt("cannot open file '%s'", filename);
	struct stat st;
	fstat(fileno(fp), &st);
	char* const text = gc_malloc (st.st_size + 1);
	if (fread(text, sizeof(char), st.st_size, fp) != (unsigned long)st.st_size)
		throw_error_fmt("error reading file '%s'", filename);
	fclose(fp);
	text[st.st_size-1] = '\0'; // Remove trailing eof.
	return text;
	// TODO: single line (or delimited) file read function for better IO performance
}

NUMBER VAR(number)(STRING str)
{
	// casts string to number.
	char* end;
	NUMBER num = strtod(str, &end);
	if (end == str || *end != '\0') goto cannot_parse;
	if unlikely(is_nan(*(long*)&num))
		goto cannot_parse;
	return num;
cannot_parse:
	throw_error_fmt("cannot parse '%.32s' to a number", str);
}

STRING VAR(path)(void)
{
	char buf[FILENAME_MAX];
	if (!getcwd(buf, FILENAME_MAX))
		throw_error("cannot get working directory");
	char* ret = gc_strdup(buf);
	return ret;
}

LIST VAR(stack)(void)
{
	LIST lst = NULL;
	flush_stack_cache();
	for (size_t i = 0; i + stack.start < stack.top; ++i)
	{
		cognate_list* tmp = gc_new (cognate_list);
		tmp -> object = stack.start[i];
		tmp -> next = lst;
		lst = tmp;
	}
	return lst;
}

void VAR(write)(STRING filename, ANY obj)
{
	// Write object to end of file, without a newline.
	FILE* const fp = fopen(filename, "a");
	if unlikely(fp == NULL) throw_error_fmt("cannot open file '%s'", filename);
	fputs(show_object(obj, 1), fp);
	fclose(fp);
}

LIST VAR(parameters)(void)
{
	return cmdline_parameters; // TODO should be a variable, and allow mutation and stuff
}

void VAR(stop)(void)
{
	// Don't check stack length, because it probably wont be empty.
	exit(EXIT_SUCCESS);
}

BOOLEAN VAR(matchDASHregex)(STRING reg_str, STRING str)
{
	// Returns true if string matches regex.
	static const char* old_str = NULL;
	static regex_t reg;
	if (old_str == NULL || strcmp(reg_str, old_str) != 0)
	{
		// Technically, the last regex to be used in the program will leak memory.
		// However, this is minor, since only a limited amount of memory can be leaked.
		regfree(&reg); // Apparently freeing an unallocated regex is fine.
		if unlikely(!*reg_str) throw_error("cannot match empty regex");
		const int status = regcomp(&reg, reg_str, REG_EXTENDED | REG_NEWLINE | REG_NOSUB);
		if unlikely(status)
		{
			char reg_err[256];
			regerror(status, &reg, reg_err, 256);
			throw_error_fmt("compile error (%s) in regex '%.32s'", reg_err, reg_str);
		}
		old_str = reg_str;
		// This should probably be strcpy, but I trust that reg_str is either
		// allocated with the garbage collector, or read only in the data segment.
	}
	const int found = regexec(&reg, str, 0, NULL, 0);
	if unlikely(found != 0 && found != REG_NOMATCH)
	{

		throw_error_fmt("match error with regex '%.32s' on string '%.32s'", str, reg_str);
		// If this error ever actually appears, use regerror to get the full text.
	}
	return !found;
}

NUMBER VAR(ordinal)(STRING str)
{
	if unlikely(!str[0] || strlen(str) > (size_t)mblen(str, MB_CUR_MAX))
		throw_error_fmt("Invalid string '%.32s' (should be length 1)", str);
	wchar_t chr = 0;
	mbtowc(&chr, str, MB_CUR_MAX);
	return chr;
}

STRING VAR(character)(NUMBER d)
{
	const wchar_t i = d;
	char* const str = gc_malloc (MB_CUR_MAX + 1);
	if unlikely(i != d || wctomb(str, i) == -1)
		throw_error_fmt("Cannot convert %.14g to UTF8 character", d);
	str[mblen(str, MB_CUR_MAX)] = '\0';
	return str;
}

NUMBER VAR(floor)(NUMBER a)
{
	return floor(a);
}

NUMBER VAR(round)(NUMBER a)
{
	return round(a);
}

NUMBER VAR(ceiling)(NUMBER a)
{
	return ceil(a);
}

void VAR(assert)(STRING name, BOOLEAN result)
{
	if unlikely(!result)
		throw_error_fmt("failed assertion '%s'", name);
}

void VAR(error)(STRING str)
{
	throw_error(str);
}

LIST VAR(map)(BLOCK blk, LIST lst)
{
	cognate_list start = {0};
	cognate_list* ptr = &start;
	for (; lst ; lst = lst->next)
	{
		push(lst->object);
		blk();
		cognate_list* new = gc_new(cognate_list);
		new->object = pop();
		new->next = NULL;
		ptr->next = new;
		ptr = new;
	}
	return start.next;

}

LIST VAR(filter)(BLOCK blk, LIST lst)
{
	cognate_list start = {0};
	cognate_list* ptr = &start;
	for (; lst ; lst = lst->next)
	{
		push(lst->object);
		blk();
		if (unbox_boolean(pop()))
		{
			cognate_list* new = gc_new(cognate_list);
			new->object = lst->object;
			new->next = NULL;
			ptr->next = new;
			ptr = new;
		}
	}
	return start.next;
}

void VAR(for)(LIST lst, BLOCK blk)
{
	for (; lst ; lst = lst->next)
	{
		push(lst->object);
		blk();
	}
}

LIST VAR(range)(NUMBER start, NUMBER end, NUMBER step)
{
	if ((end - start) * step < 0)
		throw_error_fmt("invalid range %.14g..%.14g step %.14g", start, end, step);
	end = start + step * (size_t)((end - start) / step) - step;
	LIST lst = NULL;
	for (; start * step <= end * step; end -= step)
	{
		cognate_list* node = gc_new(cognate_list);
		node->object = box_number(end);
		node->next = lst;
		lst = node;
	}
	return lst;
}

GROUP VAR(group)(BLOCK init)
{
	// Move the stack to temporary storage
	flush_stack_cache();
	ANYPTR tmp_stack_start = stack.start;
	stack.start = stack.top;
	// Allocate a list as the stack
	init();
	flush_stack_cache();
	const size_t len = stack_length();
	GROUP g = gc_malloc (sizeof g->len + len * sizeof g->items[0]);
	g->len = len;
	for (size_t i = 0; i < len; ++i) g->items[i].name = unbox_symbol(pop());
	stack.top = stack.start;
	stack.start = tmp_stack_start;
	for (size_t i = 0; i < len; ++i) g->items[i].object = pop();
	return g;
}

ANY VAR(the)(SYMBOL key, GROUP g)
{
	for (size_t i = 0; i < g->len; ++i)
		if (g->items[i].name == key) return g->items[i].object;
	throw_error_fmt("cannot find \\%.32s in record", key);
}

BOOLEAN VAR(has)(SYMBOL key, GROUP g)
{
	for (size_t i = 0; i < g->len; ++i)
		if (g->items[i].name == key) return 1;
	return 0;
}

ANY VAR(index)(NUMBER ind, LIST lst)
{
	size_t i = ind;
	if unlikely(i != ind) throw_error_fmt("cannot get index %.14g", ind);
	for (;lst;lst=lst->next)
	{
		if (!i--) return lst->object;
	}
	throw_error_fmt("index %zi is outside of array", (size_t)ind);
}

void VAR(wait)(NUMBER seconds)
{
	sleep(seconds);
}

BLOCK VAR(precompute)(BLOCK blk)
{
	flush_stack_cache();
	ANYPTR tmp_stack_start = stack.start;
	stack.start = stack.top;
	blk();
	const size_t len = stack_length();
	if (!len) return Block_copy(^{});
	ANYPTR ret_data = gc_malloc(len * sizeof *ret_data);
	for (size_t i = 0; i < len; ++i)
		ret_data[len] = stack.start[i];
	stack.top = stack.start;
	stack.start = tmp_stack_start;
	return Block_copy(^{
		for (size_t i = 0; i < len; ++i)
			push(ret_data[i]);
	});
}

static cognate_stack* parallel_precompute_helper(BLOCK blk)
{
	SET_FUNCTION_STACK_START();
	gc_init();
	init_stack();
	blk();
	// We might need to copy the stack somewhere.
	return &stack;
}

BLOCK VAR(parallelDASHprecompute)(BLOCK blk)
{
	/*
	 * A clever implementation of this would do what Go does and use threads
	 * managed by the runtime instead of the OS, which would startup much
	 * faster, and use much less memory. However, that would require writing
	 * a scheduler so TODO.
	 */
	pthread_t id;
	pthread_create(&id, NULL, (void*(*)(void *))parallel_precompute_helper, Block_copy(blk));
	return Block_copy(^{
		cognate_stack* s;
		pthread_join(id, (void*)&s);
		const size_t l = s->top - s->start;
		for (size_t i = 0; i < l; ++i) push(s->start[i]);
		if (s->cache != NIL_OBJ) push(s->cache);
	});
}

void VAR(lock)(BLOCK blk)
{
	static pthread_mutex_t mut;
	static _Bool needs_init = 1;
	if (needs_init) needs_init = 0, pthread_mutex_init(&mut, NULL);
	pthread_mutex_lock(&mut);
	blk();
	pthread_mutex_unlock(&mut);
}

STRING VAR(show)(ANY o)
{
	return show_object(o, 0);
}

static jmp_buf exit_loop;

void VAR(loop)(BLOCK b)
{
	jmp_buf a;
	memcpy(a, exit_loop, sizeof exit_loop);
	if (!setjmp(exit_loop)) for (;;) b();
	memcpy(exit_loop, a, sizeof a);
}

void VAR(break)(void)
{
	longjmp(exit_loop, 1);
}
