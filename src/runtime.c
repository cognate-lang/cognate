// ---------- RUNTIME HEADER ----------
#define _GNU_SOURCE
#define _FORTIFY_SOURCE 2

#include <stddef.h>
#include <stdio.h>
#include <Block.h>
#include <setjmp.h>
#include <string.h>
#include <sys/mman.h>
#include <limits.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <locale.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <time.h>
#include <math.h>
#include <sys/mman.h>
#include <math.h>
#include <regex.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/stat.h>
#include <setjmp.h>

#define INITIAL_READ_SIZE 64
#define STACK_MARGIN_KB		50

typedef unsigned long ANY;
typedef ANY* restrict ANYPTR;
typedef void(^BLOCK)();
typedef _Bool BOOLEAN;
typedef double NUMBER;
typedef const char* restrict STRING;
typedef const struct cognate_list* restrict LIST;
typedef const char* restrict SYMBOL;
typedef struct cognate_record* restrict RECORD;

typedef enum cognate_type
{
	NOTHING = 0,
	boolean = 1,
	string  = 2,
	list    = 3,
	record  = 4,
	block   = 5,
	symbol  = 6,
	number  = 8,
} cognate_type;

typedef struct cognate_record
{
	size_t id;
	ANY items[1];
} cognate_record;

typedef struct cognate_list
{
	LIST next;
	ANY object;
} cognate_list;

typedef struct cognate_stack
{
	ANYPTR start; // Pointer to start.
	ANYPTR top;   // Pointer to top.
	ANY cache;
	ANYPTR absolute_start; // For the garbage collector
} cognate_stack;

#define NAN_MASK 0x7ff8000000000000
#define PTR_MASK 0x0000ffffffffffff
#define TYP_MASK 0x0007000000000000
#define NIL_OBJ  0x7ff8000000000000

#define SET_FUNCTION_STACK_START() \
	function_stack_start = __builtin_frame_address(0); \
	function_stack_top = function_stack_start - function_stack_size;

#define VAR(name) ___##name
#define SYM(name) ____##name
#define CALL(name, args) VAR(name) args

#define PREDEF(name) __block BLOCK VAR(name) = ^{ throw_error("Function '"#name"' called before definition!'"); };

#define SET(name, val) \
	if unlikely(pure) throw_error("cannot mutate variable in pure function"); \
	VAR(name) = val;

#define unlikely(expr) (__builtin_expect((_Bool)(expr), 0))
#define likely(expr)	 (__builtin_expect((_Bool)(expr), 1))

#define ALLOC_RECORD(n) (gc_malloc(sizeof(size_t)+n*sizeof(ANY)))

static _Bool pure = 0;

static uintptr_t* restrict heap_start;
static uintptr_t* restrict heap_top;

static uint8_t* restrict bitmap;
static uint8_t* restrict free_start;

static size_t system_memory;

// Global variables
extern cognate_stack stack;
extern LIST cmdline_parameters;
extern const char* restrict word_name;
extern int line_num;

extern char *record_info[][64];

extern const char* restrict function_stack_top;
extern const char* restrict function_stack_start;
extern ptrdiff_t function_stack_size;

// Variables and	needed by functions.c defined in runtime.c
void init_stack(void);
void check_record_id(size_t, RECORD);
void set_function_stack_start(void);
void expand_stack(void);
char* show_object(const ANY object, const _Bool);
void _Noreturn __attribute__((format(printf, 1, 2))) throw_error_fmt(const char* restrict const, ...);
void _Noreturn throw_error(const char* restrict const);
_Bool compare_objects(ANY, ANY);
_Bool match_objects(ANY, ANY);
void destructure_lists(LIST, LIST);
void destructure_records(RECORD, RECORD);
void destructure_objects(ANY, ANY);

void* gc_malloc(size_t);
void gc_collect(void);
void gc_init(void);
char* gc_strdup(char*);
char* gc_strndup(char*, size_t);

#define gc_new(t) (t*) gc_malloc (sizeof(t))

// Variables and functions needed by compiled source file defined in runtime.c
cognate_type get_type(ANY);
_Bool is_nan(ANY);
NUMBER unbox_number(ANY);
ANY box_number(NUMBER);
BOOLEAN unbox_boolean(ANY);
ANY box_boolean(BOOLEAN);
STRING unbox_string(ANY);
ANY box_string(STRING);
LIST unbox_list(ANY);
ANY box_list(LIST);
RECORD unbox_record(ANY);
ANY box_record(RECORD);
SYMBOL unbox_symbol(ANY);
ANY box_symbol(SYMBOL);
BLOCK unbox_block(ANY);
ANY box_block(BLOCK);

void init(int, char **);
void cleanup(void);
void push(ANY);
ANY pop(void);
ANY peek(void);
void flush_stack_cache(void);
int stack_length(void);
void check_function_stack_size(void);
void set_word_name(const char* restrict const);
void set_line_num(int);

// Builtin functions needed by compiled source file defined in functions.c
ANY VAR(if)(BOOLEAN, ANY, ANY);
void VAR(when)(BOOLEAN, BLOCK);
void VAR(unless)(BOOLEAN, BLOCK);
void VAR(while)(BLOCK, BLOCK);
void VAR(do)(BLOCK);
void VAR(put)(ANY);
void VAR(print)(ANY);
NUMBER VAR(P)(NUMBER, NUMBER);
NUMBER VAR(M)(NUMBER, NUMBER);
NUMBER VAR(D)(NUMBER, NUMBER);
NUMBER VAR(S)(NUMBER, NUMBER);
NUMBER VAR(modulo)(NUMBER, NUMBER);
NUMBER VAR(sqrt)(NUMBER);
NUMBER VAR(random)(NUMBER, NUMBER);
void VAR(clear)(void);
extern BOOLEAN VAR(true);
extern BOOLEAN VAR(false);
BOOLEAN VAR(either)(BOOLEAN, BOOLEAN);
BOOLEAN VAR(both)(BOOLEAN, BOOLEAN);
BOOLEAN VAR(oneDof)(BOOLEAN, BOOLEAN);
BOOLEAN VAR(not)(BOOLEAN);
BOOLEAN VAR(EE)(ANY, ANY);
BOOLEAN VAR(SE)(ANY, ANY);
BOOLEAN VAR(L)(NUMBER, NUMBER);
BOOLEAN VAR(G)(NUMBER, NUMBER);
BOOLEAN VAR(LE)(NUMBER, NUMBER);
BOOLEAN VAR(GE)(NUMBER, NUMBER);
BOOLEAN VAR(match)(ANY, ANY);
BOOLEAN VAR(anyQ)(ANY);
BOOLEAN VAR(numberQ)(ANY);
BOOLEAN VAR(symbolQ)(ANY);
BOOLEAN VAR(listQ)(ANY);
BOOLEAN VAR(stringQ)(ANY);
BOOLEAN VAR(blockQ)(ANY);
BOOLEAN VAR(booleanQ)(ANY);
BOOLEAN VAR(integerQ)(ANY);
BOOLEAN VAR(zeroQ)(ANY);
ANY VAR(first)(LIST);
LIST VAR(rest)(LIST);
STRING VAR(head)(STRING);
STRING VAR(tail)(STRING);
LIST VAR(push)(ANY, LIST);
BOOLEAN VAR(emptyQ)(LIST);
LIST VAR(list)(BLOCK);
STRING VAR(join)(NUMBER);
NUMBER VAR(stringDlength)(STRING);
STRING VAR(substring)(NUMBER, NUMBER, STRING);
STRING VAR(input)(void);
STRING VAR(read)(STRING);
NUMBER VAR(number)(STRING);
STRING VAR(path)(void);
LIST VAR(stack)(void);
void VAR(write)(STRING, ANY);
LIST VAR(parameters)(void);
void VAR(stop)(void);
STRING VAR(show)(ANY);
BOOLEAN VAR(matchDregex)(STRING, STRING);
NUMBER VAR(ordinal)(STRING);
STRING VAR(character)(NUMBER);
NUMBER VAR(floor)(NUMBER);
NUMBER VAR(round)(NUMBER);
NUMBER VAR(ceiling)(NUMBER);
void VAR(assert)(STRING, BOOLEAN);
void VAR(error)(STRING);
LIST VAR(map)(BLOCK, LIST);
LIST VAR(filter)(BLOCK, LIST);
void VAR(for)(LIST, BLOCK);
LIST VAR(range)(NUMBER, NUMBER);
ANY VAR(index)(NUMBER, LIST);
void VAR(puts)(BLOCK);
void VAR(prints)(BLOCK);
BLOCK VAR(precompute)(BLOCK);
void VAR(wait)(NUMBER);
BLOCK VAR(case)(ANY, ANY, ANY);
LIST VAR(split)(STRING, STRING);
NUMBER VAR(length)(LIST);
LIST VAR(take)(NUMBER,LIST);
LIST VAR(discard)(NUMBER,LIST);
BLOCK VAR(remember)(BLOCK);

static const char *lookup_type(cognate_type);
static _Bool compare_lists(LIST, LIST);
static _Bool compare_records(RECORD, RECORD);
static _Bool match_records(RECORD, RECORD);
static _Bool match_lists(LIST, LIST);
static void handle_error_signal(int);
static void assert_impure();

cognate_stack stack;
LIST cmdline_parameters = NULL;

const char* restrict word_name = NULL;
int line_num = -1;

const char* restrict function_stack_top;
const char* restrict function_stack_start;
ptrdiff_t function_stack_size;

void init(int argc, char** argv)
{
	struct rlimit stack_limit;
	if unlikely(getrlimit(RLIMIT_STACK, &stack_limit) == -1)
		throw_error("cannot get return stack limit");
	function_stack_size = stack_limit.rlim_cur;
	SET_FUNCTION_STACK_START();
	// Set locale for strings.
	if unlikely(setlocale(LC_ALL, "") == NULL)
	{
		throw_error("cannot set locale");
	}

	system_memory = sysconf(_SC_PHYS_PAGES) * 4096;
	// Init GC
	gc_init();
	// Seed the random number generator properly.
	struct timespec ts;
	if unlikely(clock_gettime(CLOCK_REALTIME, &ts) == -1)
	{
		throw_error("cannot get system time");
	}
	srand(ts.tv_nsec ^ ts.tv_sec); // TODO make random more random.
	// Load parameters
	while (argc --> 1)
	{
		cognate_list* const tmp = gc_new (cognate_list);
		tmp->object = box_string(argv[argc]);
		tmp->next = cmdline_parameters;
		cmdline_parameters = tmp;
	}
	// Bind error signals.
	char signals[] = { SIGHUP, SIGSEGV, SIGINT, SIGQUIT, SIGILL, SIGABRT, SIGBUS, SIGFPE, SIGPIPE, SIGTERM, SIGCHLD };
	for (size_t i = 0; i < sizeof(signals); ++i) signal(signals[i], handle_error_signal);
	// Initialize the stack.
	init_stack();
}
void cleanup(void)
{
	if unlikely(stack.top != stack.start || stack.cache != NIL_OBJ)
	{
		word_name = NULL;
		throw_error_fmt("exiting with %ti object(s) on the stack", stack.top - stack.start + (stack.cache != NIL_OBJ));
	}
}

void check_function_stack_size(void)
{
	const char sp;
	if unlikely(&sp < function_stack_top + STACK_MARGIN_KB * 1024)
		throw_error_fmt("maximum recursion depth exceeded");
}

void set_word_name(const char* restrict const name) { word_name=name; } // Need this to avoid unsequenced evaluation error.
void set_line_num(int num) { line_num=num; } // Need this to avoid unsequenced evaluation error.

void assert_impure()
{
	if unlikely(pure) throw_error("invalid operation for pure function");
}

_Noreturn __attribute__((format(printf, 1, 2))) void throw_error_fmt(const char* restrict const fmt, ...)
{
	const _Bool debug = word_name && line_num != -1;
	int offset = -2;
	fputc('\n', stderr);
	if (debug)
	{
		int line_num_digits = 1;
		for (int tmp = line_num; tmp /= 10; ++line_num_digits);
		offset = strlen("Line: ... ") + line_num_digits + strlen(word_name);
		fprintf(stderr, "\033[0;2mLine %i: \033[0;1m... %c%s ...\n%*s\033[31;1m↳ ", line_num, toupper(*word_name), word_name + 1, offset, "");
	} else fputs("\033[31;1m", stderr);
	va_list args;
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	fputc('\n', stderr);
	if (errno) {
		const char* str = strerror(errno);
		fprintf(stderr, "%*s\033[0;2m%c%s\n", offset + 2, "", tolower(*str), str+1);
	}
	fputs("\033[0m", stderr);
	exit(EXIT_FAILURE);
}

_Noreturn void throw_error(const char* restrict const msg)
{
	const _Bool debug = word_name && line_num != -1;
	int offset = -2;
	fputc('\n', stderr);
	if (debug)
	{
		int line_num_digits = 1;
		for (int tmp = line_num; tmp /= 10; ++line_num_digits);
		offset = strlen("Line: ... ") + line_num_digits + strlen(word_name);
		fprintf(stderr, "\033[0;2mLine %i: \033[0;1m... %c%s ...\n%*s\033[31;1m↳ ",
			line_num, toupper(*word_name), word_name + 1, offset, "");
	} else fputs("\033[31;1m", stderr);
	fputs(msg, stderr);
	fputc('\n', stderr);
	if (errno)
	{
		const char* str = strerror(errno);
		fprintf(stderr, "%*s\033[0;2m%c%s\n", offset + 2, "", tolower(*str), str+1);
	}
	fputs("\033[0m", stderr);
	exit(EXIT_FAILURE);
}

void handle_error_signal(int sig)
{
	throw_error_fmt("recieved signal %i (%s)", sig, strsignal(sig));
}

char* show_object (const ANY object, const _Bool raw_strings)
{
	static char* buffer;
	static size_t depth = 0;
	if (depth++ == 0) buffer = (char*)heap_top;
	switch (get_type(object))
	{
		case number: sprintf(buffer, "%.14g", unbox_number(object));
						 buffer += strlen(buffer);
						 break;
		case string:
			if (raw_strings) strcpy(buffer, unbox_string(object));
			else
			{
				*buffer++ = '\'';
				for (const char* str = unbox_string(object) ; *str ; ++str)
				{
					char c = *str;
					if unlikely(c >= '\a' && c <= '\r')
					{
						*buffer++ = '\\';
						*buffer++ = "abtnvfr"[c-'\a'];
					}
					else if (c == '\\') { *buffer++ = '\\'; *buffer++ = '\\'; }
					else if (c == '\'') { *buffer++ = '\\'; *buffer++ = '\''; }
					else *(buffer++) = c;
				}
				*buffer++ = '\'';
				*buffer++ = '\0';
			}
			break;
		case list:
			*buffer++ = '(';
			for (LIST l = unbox_list(object) ; l ; l = l->next)
			{
				show_object(l->object, 0);
				if (!l->next) break;
				*buffer++ = ',';
				*buffer++ = ' ';
			}
			*buffer++ = ')';
			*buffer++ = '\0';
			break;
		case boolean: strcpy(buffer, unbox_boolean(object) ? "True" : "False");
						  buffer += strlen(buffer);
						  break;
		case symbol:  strcpy(buffer, unbox_symbol(object));
						  buffer += strlen(buffer);
						  break;
		case block:	  sprintf(buffer, "<block %p>", (void*)unbox_block(object));
						  buffer += strlen(buffer);
						  break;
		case record:
		{
			RECORD r = unbox_record(object);
			*buffer++ = '<';
			buffer += strlen(strcpy(buffer, record_info[r->id][0]));
			*buffer++ = ':';
			*buffer++ = ' ';
			for (size_t i = 0;;++i)
			{
				if (record_info[r->id][i+1])
					buffer+=strlen(strcpy(buffer, record_info[r->id][i+1]));
				else break;
				*buffer++ = ' ';
				show_object(r->items[i], 0);
				if (!record_info[r->id][i+2]) break;
				*buffer++ = ',';
				*buffer++ = ' ';
			}
			*buffer++ = '>';
			*buffer++ = '\0';
		}
		break;
		case NOTHING: __builtin_trap();
	}
	depth--;
	return strdup((char*)heap_top);
}

void init_stack(void)
{
	stack.absolute_start = stack.top = stack.start
		= mmap(0, system_memory/10, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
	stack.cache = NIL_OBJ;
}

void push(ANY object)
{
	if likely(stack.cache == NIL_OBJ) { stack.cache = object; return; }
	*stack.top++ = stack.cache;
	stack.cache = object;
}

ANY pop(void)
{
	if likely(stack.cache != NIL_OBJ) { const ANY a = stack.cache; stack.cache = NIL_OBJ; return a; }
	if unlikely(stack.top == stack.start) throw_error("stack underflow");
	return *--stack.top;
}

ANY peek(void)
{
	if likely(stack.cache != NIL_OBJ) return stack.cache;
	if unlikely(stack.top == stack.start) throw_error("stack underflow");
	return *(stack.top - 1);
}

void flush_stack_cache(void)
{
	if (stack.cache == NIL_OBJ) return;
	push(stack.cache);
	pop();
}

int stack_length(void)
{
	return stack.top - stack.start + (stack.cache != NIL_OBJ);
}

const char* lookup_type(cognate_type type)
{
	switch(type)
	{
		case NOTHING: return "NOTHING";
		case string:  return "string";
		case number:  return "number";
		case list:    return "list";
		case block:   return "block";
		case record:   return "record";
		case symbol:  return "symbol";
		default: __builtin_trap();
	}
}

static _Bool compare_lists(LIST lst1, LIST lst2)
{
	if (!lst1) return !lst2;
	if (!lst2) return 0;
	while (compare_objects(lst1->object, lst2->object))
	{
		if (!lst1->next) return !lst2->next;
		if (!lst2->next) return 0;
		lst1 = lst1 -> next;
		lst2 = lst2 -> next;
	}
	return 0;
}

static _Bool compare_records(RECORD r1, RECORD r2)
{
	if (r1->id != r2->id) return 0;
	for (size_t i = 0; record_info[r1->id][i+1]; ++i)
	{
		if (!compare_objects(r1->items[i], r2->items[i])) return 0;
	}
	return 1;
}

static _Bool match_records(RECORD patt, RECORD obj)
{
	if (patt->id != obj->id) return 0;
	for (size_t i = 0; record_info[patt->id][i+1]; ++i)
	{
		if (!match_objects(patt->items[i], obj->items[i])) return 0;
	}
	return 1;
}


_Bool compare_objects(ANY ob1, ANY ob2)
{
	if (get_type(ob1) != get_type(ob2)) return 0;
	switch (get_type(ob1))
	{
		case number:
			return fabs(unbox_number(ob1) - unbox_number(ob2))
				<= 0.5e-14 * fabs(unbox_number(ob1));
		case boolean: return unbox_boolean(ob1) == unbox_boolean(ob2);
		case string:  return !strcmp(unbox_string(ob1), unbox_string(ob2));
		case symbol:  return unbox_symbol(ob1) == unbox_symbol(ob2);
		case list:    return compare_lists(unbox_list(ob1), unbox_list(ob2));
		case record:  return compare_records(unbox_record(ob1), unbox_record(ob2));
		case block: throw_error("cannot compare blocks");
		default: __builtin_trap();
	}
}

_Bool match_lists(LIST lst1, LIST lst2)
{
	if (!lst1) return !lst2;
	if (!lst2) return 0;
	while (match_objects(lst1->object, lst2->object))
	{
		if (!lst1->next) return !lst2->next;
		if (!lst2->next) return 0;
		lst1 = lst1 -> next;
		lst2 = lst2 -> next;
	}
	return 0;
}

_Bool match_objects(ANY patt, ANY obj)
{
	if (get_type(patt) == block)
	{
		push(obj);
		unbox_block(patt)();
		return unbox_boolean(pop());
	}
	else if (get_type(patt) != get_type(obj)) return 0;
	switch (get_type(patt))
	{
		case number:
			return fabs(unbox_number(patt) - unbox_number(obj))
				<= 0.5e-14 * fabs(unbox_number(patt));
		case boolean: return unbox_boolean(patt) == unbox_boolean(obj);
		case string:  return !strcmp(unbox_string(patt), unbox_string(obj));
		case symbol:  return unbox_symbol(patt) == unbox_symbol(obj);
		case list:    return match_lists(unbox_list(patt), unbox_list(obj));
		case record:  return match_records(unbox_record(patt), unbox_record(obj));
		default: __builtin_trap();
	}
}

void destructure_lists(LIST patt, LIST obj)
{
	if (!patt) return;
	destructure_lists(patt->next, obj->next);
	destructure_objects(patt->object, obj->object);
}

void destructure_records(RECORD patt, RECORD obj)
{
	ssize_t i;
	for (i = 0; record_info[patt->id][i+1]; ++i);
	for (; i >= 0; --i) destructure_objects(patt->items[i], obj->items[i]);
}

void destructure_objects(ANY patt, ANY obj)
{
	if (get_type(patt) == block)
	{
		push(obj);
		return;
	}
	switch (get_type(patt))
	{
		case list:   destructure_lists(unbox_list(patt), unbox_list(obj));
		case record: destructure_records(unbox_record(patt), unbox_record(obj));
		default:;
	}

}

_Bool is_nan(ANY box)
{
	// Mostly works with -ffast-math
	return (box & NAN_MASK) == NAN_MASK;
}

cognate_type get_type(ANY box)
{
	if (is_nan(box)) return (TYP_MASK & box) >> 48;
	else return number;
}

NUMBER unbox_number(ANY box)
{
	if unlikely(is_nan(box))
		throw_error_fmt("expected a number but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return *(NUMBER*)&box;
}

ANY box_number(NUMBER num)
{
	return *(ANY*)&num;
}

BOOLEAN unbox_boolean(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)boolean << 48)
		throw_error_fmt("expected a boolean but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (STRING)(PTR_MASK & box);
}

ANY box_boolean(BOOLEAN b)
{
	return NAN_MASK | ((long)boolean << 48) | b;
}

STRING unbox_string(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)string << 48)
		throw_error_fmt("expected a string but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (STRING)(PTR_MASK & box);
}

ANY box_string(STRING s)
{
	return NAN_MASK | ((long)string << 48) | (long)s;
}

LIST unbox_list(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)list << 48)
		throw_error_fmt("expected a list but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (LIST)(PTR_MASK & box);
}

ANY box_list(LIST s)
{
	return NAN_MASK | ((long)list << 48) | (long)s;
}

RECORD unbox_record(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)record << 48)
		throw_error_fmt("expected a record but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (RECORD)(PTR_MASK & box);
}

ANY box_record(RECORD s)
{
	return NAN_MASK | ((long)record << 48) | (long)s;
}

SYMBOL unbox_symbol(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)symbol << 48)
		throw_error_fmt("expected a symbol but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (SYMBOL)(PTR_MASK & box);
}

ANY box_symbol(SYMBOL s)
{
	return NAN_MASK | ((long)symbol << 48) | (long)s;
}

BLOCK unbox_block(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)block << 48)
		throw_error_fmt("expected a block but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (BLOCK)(PTR_MASK & box);
}

ANY box_block(BLOCK s)
{
	return NAN_MASK | ((long)block << 48) | (long)Block_copy(s);
}

void check_record_id(size_t i, RECORD r)
{
	if unlikely(i != r->id)
		throw_error_fmt("expected a %.64s but got %.64s which is a %s", record_info[i][0], show_object(box_record(r), 0), record_info[r->id][0]);

}

#define PAGE_SIZE 4096

#define BITMAP_EMPTY 0x0
#define BITMAP_FREE  0x1
#define BITMAP_ALLOC 0x3

#define BITMAP_INDEX(ptr) bitmap[ptr - heap_start]

/*
 * Cognate's Garbage Collector
 *
 * This is a simplistic tracing conservative gc, using a bitmap allocator.
 * It has no dependencies on malloc() or free().
 * Well under 100 SLOC is also ridiculously small for a garbage collector.
 *
 * TODO:
 *	- Remove consequetive GC_FREEs after doing gc as then we don't have to
 *		memset the bitmap when we allocate.
 *	- Use a free list / free stack for allocating as this will improve
 *		performance
 *	- Make the bitmap only use 2 bits per long like it should, instead of a byte.
 *	- Use vector intrinsics to speed up bitmap checking/modifying.
 *
 * FIXME:
 *	- Doesn't work under valgrind.
 *	- An object will be deallocated if only referenced from other threads(!)
 *		> Collector either needs to iterate over parent threads' stacks, or a
 *		> thread-global collector iterates over all threads stacks. Second
 *		> method seems simpler and uses less memory but would also require
 *		> locks on gc_malloc().
 */

void gc_init(void)
{
	bitmap = free_start   = mmap(0, (size_t)(system_memory*0.9/8), PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
	heap_start = heap_top = mmap(0, (size_t)(system_memory*0.9),   PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
	if (heap_start == MAP_FAILED || bitmap == MAP_FAILED)
		throw_error("memory map failure - are you trying to use valgrind?");
	BITMAP_INDEX(heap_start) = BITMAP_FREE;
}

static void __attribute__((unused)) show_heap_usage(void)
{
	printf("%p -> %p\n", (void*)heap_start, (void*)heap_top);
	for (uintptr_t* i = heap_start; i < heap_top; ++i)
	{
		switch(BITMAP_INDEX(i))
		{
			case BITMAP_ALLOC: putc('#', stdout); break;
			case BITMAP_FREE:  putc('-', stdout); break;
			default:           putc('?', stdout);
		}
	}
	putc('\n', stdout);
}

__attribute__((malloc, hot, assume_aligned(sizeof(uint64_t)), alloc_size(1), returns_nonnull))
void* gc_malloc(size_t bytes)
{
	static int byte_count = 0;
	byte_count += bytes;
	if unlikely(byte_count > 1024l * 1024l * 10) gc_collect(), byte_count = 0;
	const size_t longs = (bytes + 7) / sizeof(uintptr_t);
	free_start = memchr(free_start, BITMAP_FREE, LONG_MAX);
	for (uint8_t* restrict free_end; unlikely(free_end = memchr(free_start + 1, BITMAP_ALLOC, longs - 1)); )
		free_start = memchr(free_end + 1, BITMAP_FREE, LONG_MAX);
	memset(free_start + 1, BITMAP_EMPTY, longs - 1);
	uint8_t* restrict free_end = free_start + longs;
	uintptr_t* buf = heap_start + (free_start - bitmap);
	*free_start = BITMAP_ALLOC;
	if unlikely(heap_top < buf + longs) heap_top = buf + longs;
	if (*free_end == BITMAP_EMPTY) *free_end = BITMAP_FREE;
	free_start = free_end;
	return buf;
}

static void gc_collect_root(uintptr_t object)
{
	uintptr_t* restrict ptr = (uintptr_t*)(object & PTR_MASK);
	if likely((object != (uintptr_t)ptr && !is_nan(object))
	 || ptr < heap_start || ptr >= heap_top
	 || BITMAP_INDEX(ptr) != BITMAP_FREE) return;
	/* Cognate does not use pointers to the middle of gc objects,
	 * but if in future it does, then the garbage collector will
	 * need to iterate back through the bitmap if ptr is empty. */
	BITMAP_INDEX(ptr) = BITMAP_ALLOC;
	// No need to optimize the bitmap addressing, since it's O(n) anyways.
	for (uintptr_t* p = ptr + 1; BITMAP_INDEX(p) == BITMAP_EMPTY; ++p)
		gc_collect_root(*p);
	gc_collect_root(*ptr);
}

__attribute__((noinline)) void gc_collect(void)
{
	for (uintptr_t* restrict p = (uintptr_t*)bitmap;
	    (uint8_t*)p < bitmap + (heap_top - heap_start); ++p)
		*p &= 0x5555555555555555;
	for (uintptr_t* root = stack.absolute_start; root < stack.top; ++root)
		gc_collect_root(*root);
	gc_collect_root(stack.cache);
	jmp_buf a;
	setjmp(a);
	uintptr_t* sp = (uintptr_t*)&sp + 1;
	for (uintptr_t* root = sp; root <= (uintptr_t*)function_stack_start; ++root)
		gc_collect_root(*root);
	free_start = bitmap;
}

char* gc_strdup(char* src)
{
	const size_t len = strlen(src);
	return memcpy(gc_malloc(len + 1), src, len + 1);
}

char* gc_strndup(char* src, size_t bytes)
{
	const size_t len = strlen(src);
	if (len < bytes) bytes = len;
	char* dest = gc_malloc(bytes + 1);
	dest[bytes] = '\0';
	return memcpy(dest, src, bytes);
}

ANY VAR(if)(BOOLEAN cond, ANY a, ANY b)
{
	return cond ? a : b;
}

void VAR(when)(BOOLEAN cond, BLOCK expr)
{
	if (cond) expr();
}

void VAR(unless)(BOOLEAN cond, BLOCK expr)
{
	if (!cond) expr();
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

void VAR(put)(ANY a)	 { assert_impure(); fputs(show_object(a, 1), stdout); fflush(stdout); }
void VAR(print)(ANY a) { assert_impure(); puts(show_object(a, 1)); }

void VAR(puts)(BLOCK b)		{ assert_impure(); VAR(for)(VAR(list)(b), ^{VAR(put)(pop()); }); }
void VAR(prints)(BLOCK b) { assert_impure(); VAR(for)(VAR(list)(b), ^{VAR(put)(pop()); }); putc('\n', stdout); }

NUMBER VAR(P)(NUMBER a, NUMBER b) { return a + b; } // Add cannot produce NaN.

NUMBER VAR(M)(NUMBER a, NUMBER b)
{
	const double r = a * b;
	if unlikely(is_nan(*(long*)&r))
		throw_error_fmt("multiplication by %.14g of %.14g yields invalid result", a, b);
	return r;
}
NUMBER VAR(D)(NUMBER a, NUMBER b)
{
	const double r = b - a;
	if unlikely(is_nan(*(long*)&r))
		throw_error_fmt("subtraction of %.14g from %.14g yields invalid result", a, b);
	return r;
}

NUMBER VAR(S)(NUMBER a, NUMBER b)
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

NUMBER VAR(sqrt)(NUMBER a)
{
	return sqrt(a);
}

NUMBER VAR(random)(NUMBER low, NUMBER high)
{
	NUMBER step = 1;
	if unlikely((high - low) < 0) goto invalid_range;
	else if (high - low < 1) return low;
	// This is not cryptographically secure btw.
	// Since RAND_MAX may only be 2^15, we need to do this:
	const long num
		= ((long)(short)rand())
		| ((long)(short)rand() << 15)
		| ((long)(short)rand() << 30)
		| ((long)(short)rand() << 45)
		| ((long)				rand() << 60);
	const double r = low + (NUMBER)(num % (unsigned long)(high - low));
	if unlikely(is_nan(*(long*)&r)) goto invalid_range;
	return r;
invalid_range:
	throw_error_fmt("invalid range %.14g..%.14g", low, high);
}

void VAR(clear)(void) { stack.cache = NIL_OBJ; stack.top=stack.start; }

BOOLEAN VAR(true) =  1;
BOOLEAN VAR(false) = 0;

BOOLEAN VAR(either)(BOOLEAN a, BOOLEAN b) { return a || b; }
BOOLEAN VAR(both)(BOOLEAN a, BOOLEAN b)   { return a && b; }
BOOLEAN VAR(oneDof)(BOOLEAN a, BOOLEAN b) { return a ^ b;  }
BOOLEAN VAR(not)(BOOLEAN a)               { return !a;     }


BOOLEAN VAR(EE)(ANY a, ANY b)  { return compare_objects(a,b); }
BOOLEAN VAR(SE)(ANY a, ANY b) { return !compare_objects(a,b); }
BOOLEAN VAR(G)(NUMBER a, NUMBER b)  { return a < b; }
BOOLEAN VAR(L)(NUMBER a, NUMBER b)  { return a > b; }
BOOLEAN VAR(GE)(NUMBER a, NUMBER b) { return a <= b; }
BOOLEAN VAR(LE)(NUMBER a, NUMBER b) { return a >= b; }

BOOLEAN VAR(numberQ)(ANY a)  { return get_type(a)==number; }
BOOLEAN VAR(listQ)(ANY a)    { return get_type(a)==list;   }
BOOLEAN VAR(stringQ)(ANY a)  { return get_type(a)==string; }
BOOLEAN VAR(anyQ)(ANY a)     { (void)a; return 1; }
BOOLEAN VAR(blockQ)(ANY a)   { return get_type(a)==block;  }
BOOLEAN VAR(booleanQ)(ANY a) { return get_type(a)==boolean;}
BOOLEAN VAR(symbolQ)(ANY a)  { return get_type(a)==symbol; }
BOOLEAN VAR(integerQ)(ANY a) { return VAR(numberQ)(a) && unbox_number(a) == floor(unbox_number(a)); }
BOOLEAN VAR(zeroQ)(ANY a)    { return VAR(numberQ)(a) && unbox_number(a) == 0; }

BOOLEAN VAR(match)(ANY patt, ANY obj) { return match_objects(patt,obj); }

BLOCK VAR(case)(ANY patt, ANY if_match, ANY if_not_match)
{
	_Bool block1 = get_type(if_match) == block;
	_Bool block2 = get_type(if_not_match) == block;
	if (block1 && block2)
	{
		const BLOCK b1 = unbox_block(if_match);
		const BLOCK b2 = unbox_block(if_not_match);
		return Block_copy(^{
			if (match_objects(patt, peek()))
			{
				destructure_objects(patt, pop());
				b1();
			}
			else b2();
		});
	}
	if (block1)
	{
		const BLOCK b1 = unbox_block(if_match);
		return Block_copy(^{
			ANY a = pop();
			if (match_objects(patt, a))
			{
				destructure_objects(patt, a);
				b1();
			}
			else push(if_not_match);
		});
	}
	if (block2)
	{
		const BLOCK b2 = unbox_block(if_not_match);
		return Block_copy(^{
			if (match_objects(patt, peek())) { pop(); push(if_match); }
			else b2();
		});
	}
	else return Block_copy(^{
		ANY a = pop();
		if (match_objects(patt, a)) push(if_match);
		else push(if_not_match);
	});
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

BOOLEAN VAR(emptyQ)(LIST lst)
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

NUMBER VAR(stringDlength)(STRING str)
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
	assert_impure();
	size_t size = 0;
	char* buf;
	size_t chars = getline(&buf, &size, stdin);
	char* ret = gc_strndup(buf, chars-1); // Don't copy trailing newline.
	free(buf);
	return ret;
}

STRING VAR(read)(STRING filename)
{
	assert_impure();
	// Read a file to a string.
	FILE *fp = fopen(filename, "ro");
	if unlikely(fp == NULL) throw_error_fmt("cannot open file '%s'", filename);
	struct stat st;
	fstat(fileno(fp), &st);
	char* const text = gc_malloc (st.st_size + 1);
	if (fread(text, sizeof(char), st.st_size, fp) != (unsigned long)st.st_size)
		throw_error_fmt("error reading file '%s'", filename);
	fclose(fp);
	text[st.st_size] = '\0'; // Remove trailing eof.
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
	assert_impure();
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
	assert_impure();
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
	assert_impure();
	// Don't check stack length, because it probably wont be empty.
	exit(EXIT_SUCCESS);
}

BOOLEAN VAR(matchDregex)(STRING reg_str, STRING str)
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
		errno = 0; // Hmmm
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
	flush_stack_cache();
	ANYPTR tmp_stack_start = stack.start;
	stack.start = stack.top;
	cognate_list start = {0};
	cognate_list* ptr = &start;
	for (; lst ; lst = lst->next)
	{
		push(lst->object);
		blk();
		flush_stack_cache();
		while (stack.top != stack.start)
		{
			cognate_list* new = gc_new(cognate_list);
			new->object = pop();
			new->next = NULL;
			ptr->next = new;
			ptr = new;
		}
	}
	stack.top = stack.start;
	stack.start = tmp_stack_start;
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

LIST VAR(range)(NUMBER start, NUMBER end)
{
	if (end < start)
		throw_error_fmt("invalid range %.14g..%.14g", start, end);
	end = start + (size_t)(end - start) - 1;
	LIST lst = NULL;
	for (; start <= end; end--)
	{
		cognate_list* node = gc_new(cognate_list);
		node->object = box_number(end);
		node->next = lst;
		lst = node;
	}
	return lst;
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
	assert_impure();
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

STRING VAR(show)(ANY o)
{
	return show_object(o, 0);
}

LIST VAR(split)(STRING sep, STRING str)
{
	if (!*sep) throw_error("empty separator");
	LIST lst = NULL;
	char *p = strtok(gc_strdup((char*)str), sep);
	while (p != NULL)
	{
		p = gc_strdup(p);
		cognate_list* node = gc_new(cognate_list);
		node->object = box_string(p);
		node->next = lst;
		p = strtok(NULL, sep);
		lst = node;
	}
	cognate_list* prev = NULL;
	cognate_list* curr = (cognate_list*)lst;
	while (curr)
	{
		cognate_list* next = (cognate_list*)curr->next;
		curr->next = prev;
		prev = curr;
		curr = next;
	}
	return prev;
}

NUMBER VAR(length)(LIST lst) {
	size_t len = 0;
	for (; lst; lst = lst->next)
		len++;
	return len;
}


LIST VAR(take)(NUMBER n, LIST l) {
	if unlikely(n != (unsigned long)n) throw_error_fmt("cannot take %.14g elements", n);
	LIST r = NULL;
	while (n --> 0)
	{
		if unlikely(!l) throw_error("list too small");
		cognate_list* a = gc_new(cognate_list);
		a->object = l->object;
		a->next = r;
		r = a;
		l = l->next;
	}
	cognate_list* prev = NULL;
	cognate_list* curr = (cognate_list*)r;
	while (curr)
	{
		cognate_list* next = (cognate_list*)curr->next;
		curr->next = prev;
		prev = curr;
		curr = next;
	}
	return prev;
}

LIST VAR(discard)(NUMBER n, LIST l) {
	if unlikely(n != (unsigned long)n) throw_error_fmt("cannot discard %.14g elements", n);
	for (;n-->0;l=l->next)
		if unlikely(!l) throw_error("list too small");
	return l;
}

BLOCK VAR(remember)(BLOCK b)
{
	// Only works for 1 -> 1 functions
	struct memolist {
		struct memolist* next;
		ANY input;
		ANY output;
	};
	__block struct memolist* memo = NULL;
	return Block_copy(^{
		ANY a = pop();
		for (struct memolist* l = memo ; l ; l = l->next)
			if (l->input == a)
			{
				push(l->output);
				return;
			}
		ANY* temp = stack.start;
		stack.start = stack.top;
		push(a);
		b();
		stack.start = temp;
		struct memolist* new = gc_malloc(sizeof *new);
		new->input = a;
		new->output = peek();
		new->next = memo;
		memo = new;
	});
}

BLOCK VAR(pure)(BLOCK b)
{
	return Block_copy(^{
		pure = 1;
		b();
		pure = 0;
	});
}
// ---------- ACTUAL PROGRAM ----------

