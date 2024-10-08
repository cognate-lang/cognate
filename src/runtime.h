// ---------- RUNTIME HEADER ----------
#define _GNU_SOURCE

#ifndef __TINYC__
#define _FORTIFY_SOURCE 2
#endif

#include <stddef.h>
#include <wchar.h>
#include <wctype.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <locale.h>
#include <signal.h>
#include <sys/resource.h>
#include <time.h>
#include <sys/mman.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/stat.h>
#include <setjmp.h>
#include <stdbool.h>

#ifdef __TINYC__
#define __nmatch
#endif

#include <regex.h>

#define KILOBYTE 1024l
#define MEGABYTE 1024l*KILOBYTE
#define GIGABYTE 1024l*MEGABYTE
#define TERABYTE 1024l*GIGABYTE
#define ALLOC_SIZE 100l*GIGABYTE
#define ALLOC_START (void*)(42l * TERABYTE)

#ifdef GCTEST
#define GC_FIRST_THRESHOLD 16
#define GC_MUTABLE_THRESHOLD 16
#define GC_THRESHOLD_RATIO 2
#define GC_MAX_HEAPS 300
#else
#define GC_FIRST_THRESHOLD MEGABYTE
#define GC_MUTABLE_THRESHOLD MEGABYTE
#define GC_THRESHOLD_RATIO 8
#define GC_MAX_HEAPS 8
#endif

#define NIL       ((uint64_t)0x7ffc000000000000) // NaN
#define PTR_MASK  ((uint64_t)0x0000fffffffffff8) // 48 bit aligned pointers
#define TYPE_MASK ((uint64_t)0xffff000000000007) // Everything left

#define UNALIGNED_PTR_MASK  ((uint64_t)0x0000ffffffffffff) // 48 bit unaligned pointers

#define MEM_PROT PROT_READ|PROT_WRITE
#define MEM_FLAGS MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE

typedef uint64_t ANY;
typedef uint64_t cognate_type;
typedef ANY* ANYPTR;
typedef ANY* BOX;
typedef struct cognate_block* BLOCK;
typedef char BOOLEAN;
typedef double NUMBER;
typedef const char* STRING;
typedef const struct cognate_list* LIST;
typedef const char* SYMBOL;
typedef struct cognate_file* IO;
typedef struct cognate_table* TABLE;

typedef struct cognate_block
{
	void (*fn)(ANY*);
	ANY env[0];
} cognate_block;

#define NUMBER_TYPE  ( NIL | 0x0000000000000008 ) // Use NaN boxing, so the value here is irrelevant.
#define STRING_TYPE  ( NIL | 0x0002000000000000 ) // Use a higher bit to signify strings, since they're not necessarily aligned.
#define SYMBOL_TYPE  ( NIL | 0x0001000000000000 ) // Same with symbols.
#define BOOLEAN_TYPE ( NIL | 0x0000000000000001 )
#define BOX_TYPE     ( NIL | 0x0000000000000002 )
#define LIST_TYPE    ( NIL | 0x0000000000000003 )
#define TABLE_TYPE   ( NIL | 0x0000000000000004 )
#define IO_TYPE      ( NIL | 0x0000000000000005 )
#define BLOCK_TYPE   ( NIL | 0x0000000000000006 )

typedef struct cognate_object
{
	union
	{
		BOX box;
		BOOLEAN boolean;
		STRING string;
		LIST list;
		BLOCK block;
		SYMBOL symbol;
		NUMBER number;
		IO io;
		TABLE table;
		void* ptr;
	};
	cognate_type type;
} cognate_object;

typedef struct cognate_table
{
	ANY key;
	ANY value;
	TABLE left;
	TABLE right;
	size_t level;
} cognate_table;

typedef struct cognate_list
{
	LIST next;
	ANY object;
} cognate_list;

typedef struct cognate_file
{
	STRING path;
	STRING mode;
	FILE* file;
} cognate_file;

typedef struct cognate_stack
{
	ANYPTR start; // Pointer to start.
	ANYPTR top;   // Pointer to top.
	ANYPTR absolute_start; // For the garbage collector
} cognate_stack;

#ifdef DEBUG

typedef struct backtrace
{
	const struct backtrace* next;
	const char* name;
	const size_t line;
	const size_t col;
	const char* file;
	const char* line_str;
} backtrace;

typedef struct var_info
{
	const struct var_info* next;
	const char* name;
	const ANY value;
} var_info;

#endif

#define unlikely(expr) (__builtin_expect((_Bool)(expr), 0))
#define likely(expr)	 (__builtin_expect((_Bool)(expr), 1))

typedef struct gc_heap {
	uintptr_t* start;
	uint8_t* bitmap;
	size_t alloc;
} gc_heap;

static gc_heap mutable_space[2];
static gc_heap space[GC_MAX_HEAPS];
static int gc_num_heaps = 1;

static bool mz = 0;

static _Bool pure = 0;

// Global variables
static cognate_stack stack;
static LIST cmdline_parameters = NULL;
static void* general_purpose_buffer = NULL;
#ifdef DEBUG
static const backtrace* trace = NULL;
static const var_info* vars = NULL;
#endif

extern int main(int, char**);

static const char* function_stack_start;

static TABLE memoized_regexes = NULL;

const SYMBOL SYMstart = "start";
const SYMBOL SYMend = "end";
const SYMBOL SYMcurrent = "current";
const SYMBOL SYMread = "read";
const SYMBOL SYMwrite = "write";
const SYMBOL SYMappend = "append";
const SYMBOL SYMreadHwrite = "read-write";
const SYMBOL SYMreadHappend = "read-append";
const SYMBOL SYMreadHwriteHexisting = "read-write-existing";

// Variables and	needed by functions.c defined in runtime.c
static void init_stack(void);
static void init_general_purpose_buffer(void);
static STRING show_object(const ANY object, char*, LIST);
static void _Noreturn __attribute__((format(printf, 1, 2))) throw_error_fmt(const char* const, ...);
static void _Noreturn throw_error(const char* const);
static ptrdiff_t compare_objects(ANY, ANY);
//static _Bool match_objects(ANY, ANY);
static void destructure_lists(LIST, LIST);
static void destructure_objects(ANY, ANY);
#ifdef DEBUG
static void print_backtrace(int, const backtrace*, int);
#endif

static void* gc_malloc(size_t);
static void* gc_malloc_mutable(size_t);
static void* gc_malloc_on(gc_heap*, size_t);
static void maybe_gc_collect(void);
static void gc_collect(gc_heap*, gc_heap*);
static void gc_collect_cascade(int);
static void gc_collect_mutable(void);
static void gc_init(void);
static char* gc_strdup(char*);
static char* gc_strndup(char*, size_t);
static void gc_mark_ptr(void*);
static void gc_mark_any(ANY*);
static bool any_is_ptr(ANY);
static void gc_bitmap_or(gc_heap*, size_t, uint8_t);
static void gc_bitmap_set(gc_heap*, size_t, uint8_t);
static uint8_t gc_bitmap_get(gc_heap*, size_t);

// Variables and functions needed by compiled source file defined in runtime.c
static NUMBER unbox_NUMBER(ANY);
static BOX unbox_BOX(ANY);
static ANY box_BOX(BOX);
static ANY box_NUMBER(NUMBER);
static BOOLEAN unbox_BOOLEAN(ANY);
static ANY box_BOOLEAN(BOOLEAN);
static STRING unbox_STRING(ANY);
static ANY box_STRING(STRING);
static LIST unbox_LIST(ANY);
static ANY box_LIST(LIST);
static SYMBOL unbox_SYMBOL(ANY);
static ANY box_SYMBOL(SYMBOL);
static BLOCK unbox_BLOCK(ANY);
static ANY box_BLOCK(BLOCK);
static IO unbox_IO(ANY);
static ANY box_IO(IO);
static TABLE unbox_TABLE(ANY);
static ANY box_TABLE(TABLE);

static NUMBER early_NUMBER(BOX);
static BOX early_BOX(BOX);
static BOOLEAN early_BOOLEAN(BOX);
static STRING early_STRING(BOX);
static LIST early_LIST(BOX);
static SYMBOL early_SYMBOL(BOX);
static BLOCK early_BLOCK(BOX);
static IO early_IO(BOX);
static TABLE early_TABLE(BOX);
static ANY early_ANY(BOX);

static NUMBER radians_to_degrees(NUMBER);
static NUMBER degrees_to_radians(NUMBER);

static void cleanup(void);
static void push(ANY);
static ANY pop(void);
static ANY peek(void);
static int stack_length(void);

static TABLE mktable(ANY, ANY, TABLE, TABLE, size_t);

// Builtin functions needed by compiled source file defined in functions.c
static TABLE ___insert(ANY, ANY, TABLE);
static LIST ___empty(void);
static ANY ___if(BOOLEAN, ANY, ANY);
static void ___put(ANY);
static void ___print(ANY);
static NUMBER ___P(NUMBER, NUMBER);
static NUMBER ___M(NUMBER, NUMBER);
static NUMBER ___H(NUMBER, NUMBER);
static NUMBER ___S(NUMBER, NUMBER);
static NUMBER ___C(NUMBER, NUMBER);
static NUMBER ___modulo(NUMBER, NUMBER);
static NUMBER ___sqrt(NUMBER);
static NUMBER ___random(NUMBER, NUMBER);
static void ___clear(void);
static BOOLEAN ___true(void);
static BOOLEAN ___false(void);
static BOOLEAN ___or(BOOLEAN, BOOLEAN);
static BOOLEAN ___and(BOOLEAN, BOOLEAN);
static BOOLEAN ___xor(BOOLEAN, BOOLEAN);
static BOOLEAN ___not(BOOLEAN);
static BOOLEAN ___EE(ANY, ANY);
static BOOLEAN ___L(NUMBER, NUMBER);
static BOOLEAN ___G(NUMBER, NUMBER);
static BOOLEAN ___LE(NUMBER, NUMBER);
static BOOLEAN ___GE(NUMBER, NUMBER);
//static BOOLEAN ___match(ANY, ANY);
static BOOLEAN ___numberQ(ANY);
static BOOLEAN ___symbolQ(ANY);
static BOOLEAN ___listQ(ANY);
static BOOLEAN ___stringQ(ANY);
static BOOLEAN ___blockQ(ANY);
static BOOLEAN ___booleanQ(ANY);
static BOOLEAN ___integerQ(ANY);
static BOOLEAN ___ioQ(ANY);
static BOOLEAN ___zeroQ(ANY);
static BOOLEAN ___tableQ(ANY);
static ANY ___first(ANY);
static ANY ___rest(ANY);
static STRING ___first_STRING(STRING);
static STRING ___rest_STRING(STRING);
static ANY ___first_LIST(LIST);
static LIST ___rest_LIST(LIST);
static STRING ___head(STRING);
static STRING ___tail(STRING);
static LIST ___push(ANY, LIST);
static BOOLEAN ___emptyQ(ANY);
static LIST ___list(BLOCK);
static STRING ___join(STRING, STRING);
static STRING ___substring(NUMBER, NUMBER, STRING);
static STRING ___input(void);
static IO ___open(SYMBOL, STRING);
static void ___close(IO);
static NUMBER ___number(STRING);
static STRING ___path(void);
static LIST ___stack(void);
static LIST ___parameters(void);
static void ___stop(void);
static STRING ___show(ANY);
static STRING ___show_NUMBER(NUMBER);
static STRING ___show_LIST(LIST);
static STRING ___show_BLOCK(BLOCK);
static STRING ___show_TABLE(TABLE);
static STRING ___show_IO(IO);
static STRING ___show_STRING(STRING);
static STRING ___show_SYMBOL(SYMBOL);
static STRING ___show_BOOLEAN(BOOLEAN);
static STRING ___show_BOX(BOX);

static BOOLEAN ___regex(STRING, STRING);
static BOOLEAN ___regexHmatch(STRING, STRING);
static NUMBER ___ordinal(STRING);
static STRING ___character(NUMBER);
static NUMBER ___floor(NUMBER);
static NUMBER ___round(NUMBER);
static NUMBER ___ceiling(NUMBER);
static NUMBER ___abs(NUMBER);
static void ___error(STRING);
//static BLOCK ___precompute(BLOCK);
static void ___wait(NUMBER);
static LIST ___split(STRING, STRING);
//static BLOCK ___remember(BLOCK);

static NUMBER ___sind(NUMBER);
static NUMBER ___cosd(NUMBER);
static NUMBER ___tand(NUMBER);
static NUMBER ___sin(NUMBER);
static NUMBER ___cos(NUMBER);
static NUMBER ___tan(NUMBER);

static NUMBER ___exp(NUMBER);
static NUMBER ___log(NUMBER, NUMBER);
static NUMBER ___ln(NUMBER);

static NUMBER ___asind(NUMBER);
static NUMBER ___acosd(NUMBER);
static NUMBER ___atand(NUMBER);
static NUMBER ___asin(NUMBER);
static NUMBER ___acos(NUMBER);
static NUMBER ___atan(NUMBER);

static NUMBER ___sinhd(NUMBER);
static NUMBER ___coshd(NUMBER);
static NUMBER ___tanhd(NUMBER);
static NUMBER ___sinh(NUMBER);
static NUMBER ___cosh(NUMBER);
static NUMBER ___tanh(NUMBER);

static const char *lookup_type(cognate_type);
static ptrdiff_t compare_lists(LIST, LIST);
static ptrdiff_t compare_tables(TABLE, TABLE);
static _Bool match_lists(LIST, LIST);
static void handle_error_signal(int, siginfo_t*, void *);
static void assert_impure(void);

#ifdef DEBUG
static _Bool debug = 0;
static size_t next_count = 0;
static size_t debug_lineno = 0;
#endif

static int _argc;
static char** _argv;

static void fn0(void);

int main(int argc, char** argv)
{
	function_stack_start = __builtin_frame_address(0);
	_argc = argc;
	_argv = argv;
	// Set locale for strings.
	if unlikely(setlocale(LC_ALL, "") == NULL)
	{
		throw_error("Cannot set locale");
	}
	// Init GC
	gc_init();
	// Seed the random number generator properly.
	struct timespec ts;
	if unlikely(clock_gettime(CLOCK_REALTIME, &ts) == -1)
	{
		throw_error("Cannot get system time");
	}
	srand(ts.tv_nsec ^ ts.tv_sec); // TODO make random more random.
	// Load parameters
	while (argc --> 1) cmdline_parameters = ___push(box_STRING(argv[argc]), cmdline_parameters);
	// Bind error signals.
	struct sigaction error_signal_action;
   error_signal_action.sa_sigaction = handle_error_signal;
   sigemptyset(&error_signal_action.sa_mask);
   error_signal_action.sa_flags = SA_SIGINFO;
	char signals[] = { SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGABRT, SIGBUS, SIGFPE, SIGPIPE, SIGTERM, SIGCHLD, SIGSEGV };
	for (size_t i = 0; i < sizeof(signals); ++i)
		if (sigaction(signals[i], &error_signal_action, NULL) == -1) throw_error("couldn't install signal handler");
	// Allocate buffer for object printing
	init_general_purpose_buffer();
	// Initialize the stack.
	init_stack();
#ifdef DEBUG
	if (getenv("COG_DEBUG")) debug=1;
#endif
	fn0();
	cleanup();
}

static void cleanup(void)
{
	if unlikely(stack.top != stack.start)
		throw_error_fmt("Exiting with %ti object(s) on the stack", stack.top - stack.start);
}

#ifdef DEBUG

#define BACKTRACE_PUSH(NAME, LINE, COL, FILE, LINE_STR, ID) \
	const backtrace _trace_##LINE##_##COL##_##ID = (backtrace) {.name = NAME, .line = (LINE), .col = (COL), .file = (FILE), .line_str=(LINE_STR), .next=trace}; \
	trace = &_trace_##LINE##_##COL##_##ID;

#define BACKTRACE_POP() \
	trace = trace->next;

/*
#define VARS_PUSH(NAME, CNAME, VALUE) \
	const var_info _varinfo_##CNAME = (var_info) {.name = NAME, .value = VALUE, .next=vars}; \
	vars = &_varinfo_##CNAME;

#define VARS_POP() \
	vars = vars->next;

static void debugger_step()
{
	if likely(!debug) return;
	if (next_count > 0)
	{
		next_count--;
		return;
	}
	print_backtrace(1, trace);
ask:
	fputs("\033[0;33m<DEBUG>\033[0m ", stderr);
	char buf[257] = {0};
	fgets(buf, 256, stdin);
	if (feof(stdin)) exit(EXIT_SUCCESS);
	if (*buf == '\n') goto ask;
	char op[65] = "\0";
	unsigned long int_arg = 0;
	char str_arg[129] = {0};
	sscanf(buf, "%64s %lu", op, &int_arg);
	sscanf(buf, "%64s %128s", op, str_arg);
	switch (*op)
	{
		case 'h': case 'H':
			// Help
			fputs("Usage:\n"
					"\tq       \t\033[0;1mquit\033[0m the debugger\n"
					"\th       \tshow this \033[0;1mhelp\033[0m message\n"
					"\ts [n]   \tshow (n items of) the \033[0;1mstack\033[0m\n"
					"\tc       \t\033[0;1mcontinue\033[0m execution\n"
					"\tr       \t\033[0;1mrestart\033[0m the program\n"
					"\tn [n]   \tgo to \033[0;1mnext\033[0m (n) tokens\n"
					"\tt [n]   \tprint (n items of) a back\033[0;1mtrace\033[0m\n"
					"\tl       \t\033[0;1mlist\033[0m the source program\n"
					"\tb [n]   \tset \033[0;1mbreakpoint\033[0m on line n\n"
					"\td [n]   \t\033[0;1mdelete\033[0m breakpoint on line n\n"
					"\tv [name]\tshow \033[0;1mvariable\033[0m of name\n", stderr);
			break;
		case 'r': case 'R':
			// Restart
			debug = 0;
			trace = NULL;
			vars = NULL;
			fn0(NULL);
			exit(EXIT_SUCCESS);
		case 's': case 'S':
			// Stack
			for (ANY* a = stack.top - 1;  a >= stack.start; --a)
			{
				fputs(show_object(*a, 0, NULL), stderr);
				fputc('\n', stderr);
			}
			break;
		case 'c': case 'C':
			// Continue
			debug = 0;
			return;
		case 'n': case 'N':
			// Next
			if (int_arg)
				next_count = int_arg - 1;
			return;
		case 't': case 'T':
			// Trace
			if (int_arg)
				print_backtrace(int_arg, trace);
			else print_backtrace(5, trace);
			break;
		case 'l': case 'L':
			// List TODO handle argument
			// TODO highlight current identifier like in traces
			for (size_t i = 0; source_file_lines[i]; ++i)
			{
				int broken = breakpoints[i];
				fprintf(stderr, "\033[0;2m[%3zi] %s\033[0m ", i+1, broken?"\033[0;33m*":" ");
				if (trace->line == i+1)
				{
					size_t len = strlen(trace->name);
					char* ln = source_file_lines[i];
					fprintf(stderr, "%.*s\033[0;1m%.*s\033[0;0m%s",
						(int)(trace->col - len - 1), ln,
						(int)len, ln + trace->col - len - 1,
						ln + trace->col - 1);
				}
				else fputs(source_file_lines[i], stderr);
				fputc('\n', stderr);
			}
			break;
		case 'q': case 'Q':
			// Quit
			fputs("Exiting...\n", stderr);
			exit (EXIT_SUCCESS);
		case 'b': case 'B':
			//if (int_arg > source_line_num) // TODO
			//	fprintf(stderr, "Line %zi is beyond end of file.\n", int_arg);
			if (int_arg) breakpoints[int_arg-1] = 1;
			else breakpoints[trace->line-1] = 1;
			break;
		case 'v': case 'V':;
			char* s = str_arg;
			for (size_t i = 0; i < strlen(s); ++i)
				s[i] = tolower(s[i]);
			if (!*s)
			{
				fputs("Usage: v [NAME]\n", stderr);
				break;
			}
			for (const var_info* v = vars; v; v = v->next)
			{
				if (!strcmp(v->name, str_arg))
				{
					fprintf(stderr, "%c%s = %s\n", toupper(*s), s+1, show_object(v->value, 0, NULL));
					goto ask;
				}
			}
			fprintf(stderr, "No variable '%c%s' found\nNote: debug variables are dynamically scoped\n", toupper(*s), s+1);
			break;
		case 'd': case 'D':
			// Delete breakpoint
			if (!int_arg) fputs("Usage: d [LINE]\n", stderr);
			else breakpoints[int_arg-1] = 0;
			break;
		default:
			fprintf(stderr, "Invalid command '%s'\n", op);
			break;
	}
	goto ask;
}

static void check_breakpoint(size_t line)
{
	debug |= unlikely(breakpoints[line-1]);
}

*/

static void print_backtrace(int n, const backtrace* b, int last_spaces)
{
	if (!b || !n || !b->name || !b->line_str) return;
	int len = strlen(b->name);
	char* ln = strdup(b->line_str);
	char* tabs = ln;
	ssize_t col = b->col;
	while (*ln)
	{
		if (*ln != ' ' && *ln != '\t') break;
		ln++;
		col--;
	}
	for  ( ; *tabs ; tabs++) if (*tabs == '\t') *tabs = ' ';
	char pos[128];
	sprintf(pos, "[%s %zi:%zi]", b->file, b->line, b->col);
	int spaces = (strlen(pos)) + col - len/2 - 1;
	if (last_spaces)
	{
		fputs("\033[31;1m", stderr);
		if (last_spaces + 2 < spaces)
		{
			for (int i = 0 ; i < last_spaces+1 ; ++i) fputs(" ", stderr);
			fputs("\\", stderr);
			for (int i = last_spaces+1 ; i < spaces-2 ; ++i) fputs("_", stderr);
			fputs("\n", stderr);
			for (int i = 0 ; i < spaces-1 ; ++i) fputs(" ", stderr);
			fputs("\\\n", stderr);
		}
		else if (last_spaces > spaces + 2)
		{
			for (int i = 0 ; i < spaces+2 ; ++i) fputs(" ", stderr);
			for (int i = spaces+2 ; i < last_spaces-1 ; ++i) fputs("_", stderr);
			fputs("/\n", stderr);
			for (int i = 0 ; i < spaces+1 ; ++i) fputs(" ", stderr);
			fputs("/\n", stderr);
		}
		else
		{
			for (int i = 0 ; i < spaces-1 ; ++i) fputs(" ", stderr);
			if (last_spaces < spaces) fputs("\\\n", stderr);
			else if (last_spaces > spaces) fputs("  /\n", stderr);
			else fputs(" |\n", stderr);
		}
		fputs("\033[0m", stderr);
	}
	fprintf(stderr, "\033[0;2m%s\033[0m %.*s\033[31;1m%.*s\033[0m%s\n",
			pos,
			(int)(col - len - 1), ln,
			len, ln + col - len - 1,
			ln + col - 1);
	if (n <= 1)
	{
		for (int i = 0 ; i < spaces ; ++i) fputs(" ", stderr);
		fputs("\033[31;1m^\033[0m\n", stderr);
	}
	else print_backtrace(n - 1, b->next, spaces);
}
#endif

static _Noreturn __attribute__((format(printf, 1, 2))) void throw_error_fmt(const char* const fmt, ...)
{
	char buf[1024];
	fputs("\n\n\033[31;1m    ", stderr);
	va_list args;
	va_start(args, fmt);
	vsprintf(buf, fmt, args);
	va_end(args);
	fputs(buf, stderr);
	fputs("\n", stderr);
#ifndef DEBUG
	fputs("\n\033[0m", stderr);
#else
	print_backtrace(10, trace, strlen(buf)/2 + 4);
	/*
	if (isatty(fileno(stdin)))
	{
		debug = 1;
		debugger_step();
	} else print_backtrace(5, trace);
	*/
#endif
	exit(EXIT_FAILURE);
}

static _Noreturn void throw_error(const char* const msg)
{
	throw_error_fmt("%s", msg);
}

static void handle_error_signal(int sig, siginfo_t *info, void *ucontext)
{
	if (sig == SIGSEGV)
	{
		char* addr = (char*)info->si_addr;
		if (addr >= (char*)stack.absolute_start && addr <= (char*)stack.absolute_start + ALLOC_SIZE)
			throw_error_fmt("Stack overflow (%zu items on the stack)", stack.top - stack.absolute_start);
		else
			throw_error("Memory error");
	}
	else throw_error_fmt("Received signal %i (%s)", sig, strsignal(sig));
}

static cognate_type type_of(ANY a)
{
	if ((NIL & a) != NIL) return NUMBER_TYPE;
	if ((STRING_TYPE & a) == STRING_TYPE) return STRING_TYPE;
	if ((SYMBOL_TYPE & a) == SYMBOL_TYPE) return SYMBOL_TYPE;
	return (cognate_type)(a & TYPE_MASK);
}

static void assert_impure(void)
{
	if unlikely(pure) throw_error("Invalid operation for pure function");
}

TABLE table_skew(TABLE T)
{
	// input: T, a node representing an AA tree that needs to be rebalanced.
 	// output: Another node representing the rebalanced AA tree.
	if (!T) return NULL;
	else if (!T->left) return T;
	else if (T->left->level == T->level)
		return
			mktable(T->left->key, T->left->value, T->left->left,
				mktable(T->key, T->value, T->left->right, T->right, T->level),
				T->left->level);
	/*
	else if (T->right && T->right->left && T->right && T->right->left->level == T->right->level)
	{
		TABLE c = (TABLE)T->right;
		T->right = table_skew(c);
	}
	*/
	return T;
}

TABLE table_split(TABLE T)
{
	// input: T, a node representing an AA tree that needs to be rebalanced.
   // output: Another node representing the rebalanced AA tree.
	if (!T) return NULL;
	else if (!T->right || !T->right->right) return T;
	else if (T->level == T->right->right->level)
		return
			mktable(T->right->key, T->right->value,
				mktable(T->key, T->value, T->left, T->right->left, T->level),
				T->right->right, T->right->level);
	else return T;
}

static char* show_table_helper(TABLE d, char* buffer, LIST checked)
{
	if (!d) return buffer;

	buffer = show_table_helper(d->left, buffer, checked);

	buffer = (char*)show_object(d->key, buffer, checked);
	*buffer++ = ':';
	buffer = (char*)show_object(d->value, buffer, checked);
	*buffer++ = ' ';

	buffer = show_table_helper(d->right, buffer, checked);

	return buffer;
}

static char* show_table(TABLE d, char* buffer, LIST checked)
{
	*buffer++ = '{';
	*buffer++ = ' ';
	buffer = show_table_helper(d, buffer, checked);
	*buffer++ = '}';
	*buffer = '\0';
	return buffer;
}

static char* show_io(IO i, char* buffer)
{
	if (i->file != NULL)
		return buffer + sprintf(buffer, "{ %s OPEN mode '%s' }", i->path, i->mode);
	else
		return buffer + sprintf(buffer, "{ %s CLOSED }", i->path);
}

static char* show_string(STRING s, char* buffer)
{
	*buffer++ = '"';
	for (const char* str = s ; *str ; ++str)
	{
		char c = *str;
		if unlikely(c >= '\a' && c <= '\r')
		{
			*buffer++ = '\\';
			*buffer++ = "abtnvfr"[c-'\a'];
		}
		else if (c == '\\') { *buffer++ = '\\'; *buffer++ = '\\'; }
		else if (c == '"')  { *buffer++ = '\\'; *buffer++ = '"';  }
		else *buffer++ = c;
	}
	*buffer++ = '"';
	*buffer = '\0';
	return buffer;
}

static char* show_number(NUMBER n, char* buffer)
{
	return buffer + sprintf(buffer, "%.14g", n);
}

static char* show_list(LIST l, char* buffer, LIST checked)
{
	*buffer++ = '(';
	for ( ; l ; l = l->next)
	{
		buffer = (char*)show_object(l->object, buffer, checked);
		if (!l->next) break;
		//*buffer++ = ',';
		*buffer++ = ' ';
	}
	*buffer++ = ')';
	*buffer = '\0';
	return buffer;
}

static char* show_boolean(BOOLEAN b, char* buffer)
{
	return buffer + sprintf(buffer, "%s", b ? "True" : "False");
}

static char* show_symbol(SYMBOL s, char* buffer)
{
	return buffer + sprintf(buffer, "%s", s);
}

static char* show_block(BLOCK b, char* buffer)
{
	void (*fn)(ANY*) = b->fn;
	return buffer + sprintf(buffer, "<block %p>", *(void**)&fn);
}

static char* show_box(BOX b, char* buffer, LIST checked)
{
	bool found = false;
	for (LIST l = checked ; l ; l = l->next)
		if ((BOX)(l->object & PTR_MASK) == b)
		{
			*buffer++ = '.';
			*buffer++ = '.';
			*buffer++ = '.';
			goto end;
		}
	checked = (cognate_list*)___push(box_BOX(b), checked);
	*buffer++ = '[';
	buffer = (char*)show_object(*b, buffer, checked);
	*buffer++ = ']';
	checked = (cognate_list*)___rest_LIST(checked);
	end:
	*buffer = '\0';
	return buffer;
}

static STRING show_object (const ANY object, char* buffer, LIST checked)
{
	switch (type_of(object))
	{
		case NIL: throw_error("This shouldn't happen"); break;
		case NUMBER_TYPE:  buffer = show_number (*(NUMBER*)&object,             buffer);           break;
		case IO_TYPE:      buffer = show_io     ((IO)      (object & PTR_MASK), buffer);           break;
		case BOOLEAN_TYPE: buffer = show_boolean((BOOLEAN) (object & PTR_MASK), buffer);           break;
		case STRING_TYPE:  buffer = show_string ((STRING)  (object & UNALIGNED_PTR_MASK), buffer); break;
		case SYMBOL_TYPE:  buffer = show_symbol ((SYMBOL)  (object & UNALIGNED_PTR_MASK), buffer); break;
		case BLOCK_TYPE:   buffer = show_block  ((BLOCK)   (object & PTR_MASK), buffer);           break;
		case TABLE_TYPE:   buffer = show_table  ((TABLE)   (object & PTR_MASK), buffer, checked);  break;
		case LIST_TYPE:    buffer = show_list   ((LIST)    (object & PTR_MASK), buffer, checked);  break;
		case BOX_TYPE:     buffer = show_box    ((BOX)     (object & PTR_MASK), buffer, checked);  break;
	}
	return buffer;
}

static void init_general_purpose_buffer(void)
{
	general_purpose_buffer = mmap(ALLOC_START, ALLOC_SIZE, MEM_PROT, MEM_FLAGS, -1, 0);
}

static void init_stack(void)
{
	stack.absolute_start = stack.top = stack.start
		= mmap(ALLOC_START, ALLOC_SIZE, MEM_PROT, MEM_FLAGS, -1, 0);
}

__attribute__((hot))
static void push(ANY object)
{
	*stack.top++ = object;
}

__attribute__((hot))
static ANY pop(void)
{
	if unlikely(stack.top == stack.start) throw_error("Stack underflow");
	return *--stack.top;
}

__attribute__((hot))
static ANY peek(void)
{
	if unlikely(stack.top == stack.start) throw_error("Stack underflow");
	return *(stack.top - 1);
}

static int stack_length(void)
{
	return stack.top - stack.start;
}

static const char* lookup_type(cognate_type type)
{
	switch(type)
	{
		case NIL:          return "nil";
		case BOX_TYPE:     return "box";
		case STRING_TYPE:  return "string";
		case NUMBER_TYPE:  return "number";
		case LIST_TYPE:    return "list";
		case BLOCK_TYPE:   return "block";
		case SYMBOL_TYPE:  return "symbol";
		case BOOLEAN_TYPE: return "boolean";
		default:           return NULL;
	}
}

static ptrdiff_t compare_lists(LIST lst1, LIST lst2)
{
	if (lst1 == lst2) return 0;
	if (!lst1) return -!!lst2;
	if (!lst2) return 1;
	ptrdiff_t diff;
	while (!(diff = compare_objects(lst1->object, lst2->object)))
	{
		if (!lst1->next) return -!!lst2->next;
		if (!lst2->next) return 1;
		lst1 = lst1 -> next;
		lst2 = lst2 -> next;
	}
	return diff;
}

static ptrdiff_t compare_tables(TABLE t1, TABLE t2)
{
	if (!t1) return -!!t2;
	if (!t2) return 1;

	ptrdiff_t diff;

	if ((diff = compare_objects(t1->key, t2->key))) return diff;
	if ((diff = compare_objects(t1->value, t2->value))) return diff;
	if ((diff = compare_tables(t1->left, t2->left))) return diff;

	return compare_tables(t1->right, t2->right);
}

static ptrdiff_t compare_blocks(BLOCK b1, BLOCK b2)
{
	return b1 - b2;
}

#define FLOAT_MAX_ULPS 4 // Arbitrarily chosen - higher is more tolerant

static ptrdiff_t compare_numbers(NUMBER n1, NUMBER n2)
{
	// Floating point maths is rather difficult.
	// This solution is usually better than the naive comparison that most languages use.
	// But if correctness is important one should implement their own comparison operator.
	// Def Exact== as ( Let N1 ; Let N2 ; Both <= N1 N2 and <= N2 N1 );
	ptrdiff_t diff = *(uint64_t*)&n1 - *(uint64_t*)&n2;
	return diff / (FLOAT_MAX_ULPS + 1);
}

static ptrdiff_t compare_strings(STRING s1, STRING s2)
{
	return strcmp(s1, s2);
}

static ptrdiff_t compare_io(IO i1, IO i2)
{
	return i1->file - i2->file;
}

static ptrdiff_t compare_booleans(BOOLEAN b1, BOOLEAN b2)
{
	return (ptrdiff_t)b1 - (ptrdiff_t)b2;
}

static ptrdiff_t compare_boxes(BOX b1, BOX b2)
{
	return b1 - b2;
}

static ptrdiff_t compare_symbols(SYMBOL s1, SYMBOL s2)
{
	return s1 - s2;
}

static ptrdiff_t compare_objects(ANY ob1, ANY ob2)
{
	// TODO this function should be overloaded
	cognate_type t1 = type_of(ob1);
	cognate_type t2 = type_of(ob2);
	if (t1 != t2) return (ptrdiff_t)t1 - (ptrdiff_t)t2;
	//if (memcmp(&ob1, &ob2, sizeof ob1) == 0) return 0;
	switch (t1)
	{
		case NUMBER_TYPE:  return compare_numbers(*(NUMBER*)&ob1, *(NUMBER*)&ob2);
		case STRING_TYPE:  return compare_strings((STRING)(ob1 & UNALIGNED_PTR_MASK), (STRING)(ob2 & UNALIGNED_PTR_MASK));
		case LIST_TYPE:    return compare_lists((LIST)(ob1 & PTR_MASK), (LIST)(ob2 & PTR_MASK));
		case BLOCK_TYPE:   return compare_blocks((BLOCK)(ob1 & PTR_MASK), (BLOCK)(ob2 & PTR_MASK));
		case TABLE_TYPE:   return compare_tables((TABLE)(ob1 & PTR_MASK), (TABLE)(ob2 & PTR_MASK));
		case IO_TYPE:      return compare_io((IO)(ob1 & PTR_MASK), ((IO)(ob2 & PTR_MASK)));
		case BOOLEAN_TYPE: return compare_booleans((BOOLEAN)(ob1 & PTR_MASK), (BOOLEAN)(ob2 & PTR_MASK));
		case BOX_TYPE:     return compare_boxes((BOX)(ob1 & PTR_MASK), (BOX)(ob2 & PTR_MASK));
		case SYMBOL_TYPE:  return compare_symbols((SYMBOL)(ob1 & UNALIGNED_PTR_MASK), (SYMBOL)(ob2 & UNALIGNED_PTR_MASK));
		default:           return 0; // really shouldn't happen
		/* NOTE
		 * The garbage collector *will* reorder objects in memory,
		 * which means that the relative addresses of blocks and boxes
		 * *will* change - which is why these can't be used to index tables
		 * for example.
		 */
	}
}

static void call_block(BLOCK b)
{
	b->fn((ANY*)&b->env);
}



/*
static _Bool match_lists(LIST lst1, LIST lst2)
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

static _Bool match_objects(ANY patt, ANY obj)
{
	cognate_type T = patt.type;
	if (T == block)
	{
		push (obj);
		call_block(unbox_BLOCK(patt));
		return unbox_BOOLEAN(pop());
	}
	else if (T != obj.type) return 0;
	switch (T)
	{
		case number:
			return fabs(unbox_NUMBER(patt) - unbox_NUMBER(obj))
				<= 0.5e-14 * fabs(unbox_NUMBER(patt));
		case boolean: return unbox_BOOLEAN(patt) == unbox_BOOLEAN(obj);
		case string:  return !strcmp(unbox_STRING(patt), unbox_STRING(obj));
		case symbol:  return unbox_SYMBOL(patt) == unbox_SYMBOL(obj);
		case list:    return match_lists(unbox_LIST(patt), unbox_LIST(obj));
		case box:     return match_objects(*unbox_BOX(patt), *unbox_BOX(obj));
		default:      return 0; // really shouldn't happen
	}
}


static void destructure_lists(LIST patt, LIST obj)
{
	if (!patt) return;
	destructure_lists(patt->next, obj->next);
	destructure_objects(patt->object, obj->object);
}

static void destructure_objects(ANY patt, ANY obj)
{
	if (patt.type == block)
	{
		push(obj);
		return;
	}
	switch (patt.type)
	{
		case list:   destructure_lists(unbox_LIST(patt), unbox_LIST(obj)); break;
		case box:    destructure_objects(*unbox_BOX(patt), *unbox_BOX(obj)); break;
		default:;
	}

}
*/

static _Noreturn void type_error(char* expected, ANY got)
{
	char* s = "a";
	switch (expected[0])
		case 'a': case 'e': case 'i': case 'o': case 'u': case 'h':
			s = "an";
	throw_error_fmt("Expected %s %s but got %.64s", s, expected, ___show(got));
}

__attribute__((hot))
static NUMBER unbox_NUMBER(ANY b)
{
	if likely((b & NIL) != NIL)
		return *(NUMBER*)&b;
	type_error("number", b);
	#ifdef __TINYC__
	return 0.0;
	#endif
}

__attribute__((hot))
static ANY box_NUMBER(NUMBER num)
{
	return *(ANY*)&num;
}

__attribute__((hot))
static BOX unbox_BOX(ANY b)
{
	if likely((b & TYPE_MASK) == BOX_TYPE)
		return (BOX)(b & PTR_MASK);
	type_error("box", b);
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static ANY box_BOX(BOX b)
{
	return BOX_TYPE | (ANY)b;
}

__attribute__((hot))
static BOOLEAN unbox_BOOLEAN(ANY b)
{
	if likely((b & TYPE_MASK) == BOOLEAN_TYPE)
		return (BOOLEAN)(b & PTR_MASK);
	type_error("boolean", b);
	#ifdef __TINYC__
	return 0;
	#endif
}

__attribute__((hot))
static ANY box_BOOLEAN(BOOLEAN b)
{
	return BOOLEAN_TYPE | (ANY)(0x8 * (b != false));
}

__attribute__((hot))
static STRING unbox_STRING(ANY b)
{
	if likely((b & STRING_TYPE) == STRING_TYPE)
		return (STRING)(b & UNALIGNED_PTR_MASK);
	type_error("string", b);
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static ANY box_STRING(STRING s)
{
	return STRING_TYPE | (ANY)s;
}

__attribute__((hot))
static LIST unbox_LIST(ANY b)
{
	if likely((b & TYPE_MASK) == LIST_TYPE)
		return (LIST)(b & PTR_MASK);
	type_error("list", b);
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static ANY box_LIST(LIST l)
{
	return LIST_TYPE | (ANY)l;
}

__attribute__((hot))
static SYMBOL unbox_SYMBOL(ANY b)
{
	if likely((b & SYMBOL_TYPE) == SYMBOL_TYPE)
		return (SYMBOL)(b & UNALIGNED_PTR_MASK);
	type_error("list", b);
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static ANY box_SYMBOL(SYMBOL s)
{
	return SYMBOL_TYPE | (ANY)s;
}

__attribute__((hot))
static BLOCK unbox_BLOCK(ANY b)
{
	if likely((b & TYPE_MASK) == BLOCK_TYPE)
		return (BLOCK)(b & PTR_MASK);
	type_error("block", b);
	#ifdef __TINYC__
	return NULL;
	#endif
}

/*
BLOCK block_copy(BLOCK b)
{
	size_t i = 0;
	for (; b->env[i] ; ++i);
	BLOCK B = gc_malloc(sizeof(b->fn) + (i + 1) * (sizeof b->env[0]) + i * sizeof(ANY));
	uint64_t* buf = (uint64_t*)B + i + 2;
	for (size_t i = 0; b->env[i] ; ++i)
	{
		B->env[i] = buf++;
		*(uint64_t*)(B->env[i]) = *(uint64_t*)(b->env[i]);
	}
	B->env[i] = NULL;
	B->fn = b->fn;
	return B;
}
*/
__attribute__((hot))
static ANY box_BLOCK(BLOCK b)
{
	return BLOCK_TYPE | (ANY)b;
}

__attribute__((hot))
static ANY box_IO(IO i)
{
	return IO_TYPE | (ANY)i;
}


__attribute__((hot))
static IO unbox_IO(ANY b)
{
	if likely((b & TYPE_MASK) == IO_TYPE)
		return (IO)(b & PTR_MASK);
	type_error("io", b);
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static ANY box_TABLE(TABLE d)
{
	return TABLE_TYPE | (ANY)d;
}

__attribute__((hot))
static TABLE unbox_TABLE(ANY b)
{
	if likely((b & TYPE_MASK) == TABLE_TYPE)
		return (TABLE)(b & PTR_MASK);
	type_error("table", b);
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static TABLE early_TABLE(BOX box)
{
	ANY a = *box;
	if likely (a != NIL) return (TABLE) (a & PTR_MASK);
	throw_error("Used before definition");
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static LIST early_LIST(BOX box)
{
	ANY a = *box;
	if likely (a != NIL) return (LIST) (a & PTR_MASK);
	throw_error("Used before definition");
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static NUMBER early_NUMBER(BOX box)
{
	ANY a = *box;
	if likely (a != NIL) return *(NUMBER*)&a;
	throw_error("Used before definition");
	#ifdef __TINYC__
	return 0;
	#endif
}

__attribute__((hot))
static BOOLEAN early_BOOLEAN(BOX box)
{
	ANY a = *box;
	if likely (a != NIL) return (BOOLEAN) (a & PTR_MASK);
	throw_error("Used before definition");
	#ifdef __TINYC__
	return 0;
	#endif
}

__attribute__((hot))
static SYMBOL early_SYMBOL(BOX box)
{
	ANY a = *box;
	if likely (a != NIL) return (SYMBOL) (a & PTR_MASK);
	throw_error("Used before definition");
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static STRING early_STRING(BOX box)
{
	ANY a = *box;
	if likely (a != NIL) return (STRING) (a & UNALIGNED_PTR_MASK);
	throw_error("Used before definition");
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static BLOCK early_BLOCK(BOX box)
{
	ANY a = *box;
	if likely (a != NIL) return (BLOCK) (a & PTR_MASK);
	throw_error("Used before definition");
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static IO early_IO(BOX box)
{
	ANY a = *box;
	if likely (a != NIL) return (IO) (a & PTR_MASK);
	throw_error("Used before definition");
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static BOX early_BOX(BOX box)
{
	ANY a = *box;
	if likely (a != NIL) return (BOX) (a & PTR_MASK);
	throw_error("Used before definition");
	#ifdef __TINYC__
	return NULL;
	#endif
}

__attribute__((hot))
static ANY early_ANY(BOX box)
{
	ANY a = *box;
	if likely (a != NIL) return a;
	throw_error("Used before definition");
	#ifdef __TINYC__
	return NIL;
	#endif
}

#define EMPTY    0x0 // 0000
#define ALLOC    0x1 // 0001
#define PTR      0x2 // 0010
#define ALLOCPTR 0x3 // 0011
#define FORWARD  0x7 // 0111

static void gc_mark_ptr(void* ptr)
{
	//printf("OR[%i] %i -> ", (uintptr_t*)ptr - space[0].start, gc_bitmap_get(&space[0], (uintptr_t*)ptr - space[0].start));
	gc_bitmap_or(&space[0], (uintptr_t*)ptr - space[0].start, PTR);
	//printf("%i\n", gc_bitmap_get(&space[0], (uintptr_t*)ptr - space[0].start));
}

static void gc_mark_mutable_ptr(void* ptr)
{
	gc_bitmap_or(&mutable_space[mz], (uintptr_t*)ptr - mutable_space[mz].start, PTR);
}

static void gc_mark_mutable_any(ANY* a)
{
	if (any_is_ptr(*a)) gc_mark_mutable_ptr(a);
}

static void gc_mark_any(ANY* a)
{
	if (any_is_ptr(*a)) gc_mark_ptr(a);
}

static void gc_bitmap_or(gc_heap* heap, size_t index, uint8_t value)
{
	//printf("bitmap at %p\n", heap->bitmap);
	heap->bitmap[index / 2] |= (uint8_t)(value << (4 * (index & 0x1)));  // set to value
}

static void gc_bitmap_set(gc_heap* heap, size_t index, uint8_t value)
{
	//printf("bitmap at %p\n", heap->bitmap);
	heap->bitmap[index / 2] &= (uint8_t)(~(0xf << (4 * (index & 0x1)))); // set to 00
	heap->bitmap[index / 2] |= (uint8_t)(value << (4 * (index & 0x1)));  // set to value
}

static uint8_t gc_bitmap_get(gc_heap* heap, size_t index)
{
	return (heap->bitmap[index / 2] >> (4 * (index & 0x1))) & 0xf;
}

static void gc_init_heap(gc_heap* heap)
{
	heap->bitmap = mmap(ALLOC_START, ALLOC_SIZE/16, MEM_PROT, MEM_FLAGS, -1, 0);
	heap->start  = mmap(ALLOC_START, ALLOC_SIZE,    MEM_PROT, MEM_FLAGS, -1, 0);
	heap->alloc  = 0;
	gc_bitmap_set(heap, 0, ALLOC);
}

static void gc_init(void)
{
	gc_init_heap(&mutable_space[0]);
	gc_init_heap(&mutable_space[1]);
	gc_init_heap(&space[0]);
}


__attribute__((assume_aligned(sizeof(uint64_t)), returns_nonnull))
static void* gc_malloc_on(gc_heap* heap, size_t sz)
{
	void* buf = heap->start + heap->alloc;
	heap->alloc += (sz + 7) / 8;
	gc_bitmap_set(heap, heap->alloc, ALLOC);
	return buf;
}

static void* __attribute__((noinline)) gc_malloc_mutable(size_t sz)
{
	asm("");
	maybe_gc_collect();
	return gc_malloc_on(&mutable_space[mz], sz);
}

static void* __attribute__((noinline)) gc_malloc(size_t sz)
{
	asm("");
	maybe_gc_collect();
	return gc_malloc_on(&space[0], sz);
}

static bool is_gc_ptr(gc_heap* heap, uintptr_t object)
{
	uintptr_t diff = (uintptr_t*)(object & PTR_MASK) - heap->start;
	return diff < heap->alloc;
}

static void gc_collect_root(uintptr_t* addr, gc_heap* source, gc_heap* dest)
{
	if (!is_gc_ptr(source, *addr)) return;
	struct action {
		uintptr_t from;
		uintptr_t* to;
	};
	struct action* act_stk_start = (struct action*)source->start + source->alloc;
	struct action* act_stk_top = act_stk_start;
	*act_stk_top++ = (struct action) { .from=*addr, .to=addr };
	while (act_stk_top-- != act_stk_start)
	{
		uintptr_t from = act_stk_top->from;
		uintptr_t* to = act_stk_top->to;
		const uintptr_t extra_bits = from & ~PTR_MASK;
		uintptr_t index = (uintptr_t*)(from & PTR_MASK) - source->start;
		ptrdiff_t offset = 0;
		while (!(gc_bitmap_get(source, index) & ALLOC)) index--, offset++; // Ptr to middle of object
		uint8_t alloc_mode = gc_bitmap_get(source, index);
		if (alloc_mode == FORWARD && is_gc_ptr(dest, source->start[index]))
			*to = extra_bits | (uintptr_t)((uintptr_t*)source->start[index] + offset);
		else
		{
			uintptr_t* buf = dest->start + dest->alloc; // Buffer in newspace
			size_t sz = 0;
			uint8_t bits = alloc_mode;
			for ( ; (sz==0) || !((bits = gc_bitmap_get(source, index + sz)) & ALLOC) ; sz++ )
			{
				gc_bitmap_set(dest, dest->alloc + sz, bits);
				uintptr_t from = source->start[index + sz];
				if ((bits & PTR) && is_gc_ptr(source, from))
					*act_stk_top++ = (struct action) { .from=from, .to=buf+sz };
				else buf[sz] = from;
			}
			dest->alloc += sz;
			source->start[index] = (uintptr_t)buf; // Set forwarding address
			gc_bitmap_set(source, index, FORWARD);
			*to = extra_bits | (uintptr_t)(buf + offset);
		}
	}
}

static void gc_clear_heap(gc_heap* heap)
{
	memset(heap->bitmap, 0x0, heap->alloc / 2 + 1);
	heap->alloc = 0;
	gc_bitmap_set(heap, 0, ALLOC);
}

static void gc_collect_from_heap(gc_heap* roots, gc_heap* source, gc_heap* dest)
{
	asm("");
	for (size_t i = 0 ; i < roots->alloc; ++i)
		if (gc_bitmap_get(roots, i) & PTR) gc_collect_root(roots->start + i, source, dest);

	gc_bitmap_set(dest, dest->alloc, ALLOC);
}

static bool any_is_ptr(ANY a)
{
	switch (type_of(a))
	{
		case NIL: case NUMBER_TYPE: case BOOLEAN_TYPE: case SYMBOL_TYPE: return false;
		default: return true;
	}
}

__attribute__((noinline))
static void gc_collect_from_stacks(gc_heap* source, gc_heap* dest)
{
	asm("");
	for (ANY* root = stack.absolute_start; root < stack.top; ++root)
		if (any_is_ptr(*root)) gc_collect_root((uintptr_t*)root, source, dest);

	jmp_buf a;
	if (setjmp(a)) return;

	for (uintptr_t* root = (uintptr_t*)&a; root < (uintptr_t*)function_stack_start; ++root)
		gc_collect_root(root, source, dest); // Watch me destructively modify the call stack

	gc_collect_root((uintptr_t*)&memoized_regexes, source, dest);

	gc_bitmap_set(dest, dest->alloc, ALLOC);

	longjmp(a, 1);
}

static void maybe_gc_collect(void)
{
	size_t threshold = GC_FIRST_THRESHOLD;
	for (int i = 0 ; space[i].alloc > threshold ; ++i, threshold *= GC_THRESHOLD_RATIO)
		gc_collect_cascade(i);

	static size_t mutable_space_alloc = 0;
	if (mutable_space[mz].alloc - mutable_space_alloc > GC_MUTABLE_THRESHOLD)
	{
		gc_collect_mutable();
		mutable_space_alloc = mutable_space[mz].alloc;
	}
}

static size_t gc_heap_usage(void)
{
	size_t n = 0;
	for (int i = 0 ; i < gc_num_heaps ; ++i) n += space[i].alloc;
	return n + mutable_space[mz].alloc + mutable_space[!mz].alloc;
}

__attribute__((noinline))
static void gc_collect_cascade(int n)
{
	asm("");
/*
	clock_t start = clock();
	size_t original_heap = gc_heap_usage();
*/

	if unlikely(n + 1 == gc_num_heaps)
	{
		if unlikely(gc_num_heaps == GC_MAX_HEAPS) throw_error("GC heap exhausted");
		gc_init_heap(&space[n+1]);
		gc_num_heaps++;
	}
	gc_collect_from_heap(&mutable_space[mz], &space[n], &space[n+1]);
	gc_collect_from_stacks(&space[n], &space[n+1]);
	gc_clear_heap(&space[n]);
/*
	clock_t end = clock();
	float mseconds = (float)(end - start) * 1000 / CLOCKS_PER_SEC;
	printf("cascade %i->%i took %lfms (%zu -> %zu)\n", n, n+1, mseconds, original_heap, gc_heap_usage());
*/
}

static void gc_collect_mutable(void)
{
	gc_collect_from_stacks(&mutable_space[mz], &mutable_space[!mz]); // Mutable memory gc
	for (int i = 0 ; i < gc_num_heaps ; ++i)
		gc_collect_from_heap(&space[i], &mutable_space[mz], &mutable_space[!mz]); // Mutable memory can be referenced by main memory. TODO combine this with main memory gc
	gc_clear_heap(&mutable_space[mz]);
	mz = !mz;
}

static char* gc_strdup(char* src)
{
	const size_t len = strlen(src);
	return memcpy(gc_malloc(len + 1), src, len + 1);
}

static char* gc_strndup(char* src, size_t bytes)
{
	const size_t len = strlen(src);
	if (len < bytes) bytes = len;
	char* dest = gc_malloc(bytes + 1);
	dest[bytes] = '\0';
	return memcpy(dest, src, bytes);
}

static ANY ___if(BOOLEAN cond, ANY a, ANY b)
{
	return cond ? a : b;
}

static void ___put(ANY a)             { assert_impure(); fputs(___show(a), stdout); fflush(stdout);         }
static void ___put_NUMBER(NUMBER a)   { assert_impure(); fputs(___show_NUMBER(a), stdout); fflush(stdout);  }
static void ___put_LIST(LIST a)       { assert_impure(); fputs(___show_LIST(a), stdout); fflush(stdout);    }
static void ___put_TABLE(TABLE a)     { assert_impure(); fputs(___show_TABLE(a), stdout); fflush(stdout);   }
static void ___put_IO(IO a)           { assert_impure(); fputs(___show_IO(a), stdout); fflush(stdout);      }
static void ___put_BLOCK(BLOCK a)     { assert_impure(); fputs(___show_BLOCK(a), stdout); fflush(stdout);   }
static void ___put_STRING(STRING a)   { assert_impure(); fputs(___show_STRING(a), stdout); fflush(stdout);  }
static void ___put_SYMBOL(SYMBOL a)   { assert_impure(); fputs(___show_SYMBOL(a), stdout); fflush(stdout);  }
static void ___put_BOOLEAN(BOOLEAN a) { assert_impure(); fputs(___show_BOOLEAN(a), stdout); fflush(stdout); }
static void ___put_BOX(BOX a)         { assert_impure(); fputs(___show_BOX(a), stdout);                     }

static void ___print(ANY a)             { assert_impure(); puts(___show(a));         }
static void ___print_NUMBER(NUMBER a)   { assert_impure(); puts(___show_NUMBER(a));  }
static void ___print_LIST(LIST a)       { assert_impure(); puts(___show_LIST(a));    }
static void ___print_TABLE(TABLE a)     { assert_impure(); puts(___show_TABLE(a));   }
static void ___print_IO(IO a)           { assert_impure(); puts(___show_IO(a));      }
static void ___print_BLOCK(BLOCK a)     { assert_impure(); puts(___show_BLOCK(a));   }
static void ___print_STRING(STRING a)   { assert_impure(); puts(___show_STRING(a));  }
static void ___print_SYMBOL(SYMBOL a)   { assert_impure(); puts(___show_SYMBOL(a));  }
static void ___print_BOOLEAN(BOOLEAN a) { assert_impure(); puts(___show_BOOLEAN(a)); }
static void ___print_BOX(BOX a)         { assert_impure(); puts(___show_BOX(a));     }

static NUMBER ___P(NUMBER a, NUMBER b) { return a + b; } // Add cannot produce NaN.
static NUMBER ___M(NUMBER a, NUMBER b) { return a * b; }
static NUMBER ___H(NUMBER a, NUMBER b) { return b - a; }
static NUMBER ___S(NUMBER a, NUMBER b) { return b / a; }
static NUMBER ___C(NUMBER a, NUMBER b) { return pow(b, a); }
static NUMBER ___modulo(NUMBER a, NUMBER b) { return b - a * floor(b / a); }
static NUMBER ___sqrt(NUMBER a) { return sqrt(a); }
static NUMBER ___random(NUMBER low, NUMBER high)
{
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
	return r;
invalid_range:
	throw_error_fmt("Invalid range %.14g..%.14g", low, high);
	#ifdef __TINYC__
	return 0;
	#endif
}

static void ___clear(void) { stack.top=stack.start; }

static BOOLEAN ___true(void)  { return true; }
static BOOLEAN ___false(void) { return false; }
static BOOLEAN ___or(BOOLEAN a, BOOLEAN b)  { return a || b; }
static BOOLEAN ___and(BOOLEAN a, BOOLEAN b) { return a && b; }
static BOOLEAN ___xor(BOOLEAN a, BOOLEAN b) { return a ^ b;  }
static BOOLEAN ___not(BOOLEAN a)            { return a ? false : true; }
static BOOLEAN ___EE(ANY a, ANY b) { return 0 == compare_objects(a,b); }
static BOOLEAN ___G(NUMBER a, NUMBER b)  { return a < b; }
static BOOLEAN ___L(NUMBER a, NUMBER b)  { return a > b; }
static BOOLEAN ___GE(NUMBER a, NUMBER b) { return a <= b; }
static BOOLEAN ___LE(NUMBER a, NUMBER b) { return a >= b; }
static BOOLEAN ___numberQ(ANY a)  { return (a & NIL) != NIL; }
static BOOLEAN ___listQ(ANY a)    { return (a & TYPE_MASK)   == LIST_TYPE;    }
static BOOLEAN ___stringQ(ANY a)  { return (a & STRING_TYPE) == STRING_TYPE;  }
static BOOLEAN ___blockQ(ANY a)   { return (a & TYPE_MASK)   == BLOCK_TYPE;   }
static BOOLEAN ___booleanQ(ANY a) { return (a & TYPE_MASK)   == BOOLEAN_TYPE; }
static BOOLEAN ___symbolQ(ANY a)  { return (a & SYMBOL_TYPE) == SYMBOL_TYPE;  }
static BOOLEAN ___ioQ(ANY a)      { return (a & TYPE_MASK)   == IO_TYPE;      }
static BOOLEAN ___tableQ(ANY a)   { return (a & TYPE_MASK)   == TABLE_TYPE;   }
static BOOLEAN ___integerQ(ANY a) { return ___numberQ(a) && unbox_NUMBER(a) == floor(unbox_NUMBER(a)); }
static BOOLEAN ___zeroQ(ANY a)    { return ___numberQ(a) && unbox_NUMBER(a) == 0; }

static NUMBER  ___numberX(NUMBER a)  { return a; }
static LIST    ___listX(LIST a)      { return a; }
static STRING  ___stringX(STRING a)  { return a; }
static ANY     ___anyX(ANY a)        { return a; }
static BLOCK   ___blockX(BLOCK a)    { return a; }
static BOOLEAN ___booleanX(BOOLEAN a){ return a; }
static SYMBOL  ___symbolX(SYMBOL a)  { return a; }
static IO      ___ioX(IO a)          { return a; }
static TABLE   ___tableX(TABLE a)    { return a; }

//static BOOLEAN ___match(ANY patt, ANY obj) { return match_objects(patt,obj); }

static ANY ___first_LIST(LIST lst)
{
	// Returns the first element of a list. O(1).
	if unlikely(!lst) throw_error("empty list is invalid");
	return lst->object;
}

static LIST ___rest_LIST(LIST lst)
{
	// Returns the tail portion of a list. O(1).
	if unlikely(!lst) throw_error("empty list is invalid");
	return lst->next;
}

static STRING ___first_STRING(STRING str)
{
	if unlikely(!*str) throw_error("empty string is invalid");
	return gc_strndup((char*)str, mblen(str, MB_CUR_MAX));
}

static STRING ___rest_STRING(STRING str)
{
	if unlikely(!*str) throw_error("empty string is invalid");
	return str + mblen(str, MB_CUR_MAX);
}

static ANY ___first(ANY a)
{
	switch(type_of(a))
	{
		case LIST_TYPE:   return ___first_LIST((LIST)(a & PTR_MASK));
		case STRING_TYPE: return box_STRING(___first_STRING((STRING)(a & UNALIGNED_PTR_MASK)));
		default: type_error("string or list", a);
	}
#ifdef __TINYC__
	return NIL;
#endif
}

static ANY ___rest(ANY a)
{
	switch(type_of(a))
	{
		case LIST_TYPE:   return box_LIST(___rest_LIST((LIST)(a & PTR_MASK)));
		case STRING_TYPE: return box_STRING(___rest_STRING((STRING)(a & UNALIGNED_PTR_MASK)));
		default: type_error("string or list", a);
	}
#ifdef __TINYC__
	return NIL;
#endif
}

static LIST ___push(ANY a, LIST b)
{
	// Pushes an object from the stack onto the list's first element. O(1).
	// TODO: Better name? Inconsistent with List where pushing to the stack adds to the END.
	cognate_list* lst = gc_malloc (sizeof *lst);
	*lst = (cognate_list) {.object = a, .next = b};
	gc_mark_ptr((void*)&lst->next);
	gc_mark_any(&lst->object);
	return lst;
}

static BOOLEAN ___emptyQ_LIST(LIST l)
{
	return !l;
}

static BOOLEAN ___emptyQ_STRING(STRING s)
{
	return !*s;
}

static BOOLEAN ___emptyQ_TABLE(TABLE t)
{
	return !t;
}

static BOOLEAN ___emptyQ(ANY a)
{
	// Returns true is a list or string is empty. O(1).
	// Can be used to to write a Length function.
	switch (type_of(a))
	{
		case LIST_TYPE: return ___emptyQ_LIST(unbox_LIST(a));
		case STRING_TYPE: return ___emptyQ_STRING(unbox_STRING(a));
		case TABLE_TYPE: return ___emptyQ_TABLE(unbox_TABLE(a));
		default: type_error("List or String or Table", a);
	}
	#ifdef __TINYC__
	return 0;
	#endif
}

static LIST ___list(BLOCK expr)
{
	ANYPTR tmp_stack_start = stack.start;
	stack.start = stack.top;
	// Eval expr
	call_block(expr);
	// Move to a list.
	LIST lst = NULL;
	size_t len = stack_length();
	for (size_t i = 0; i < len; ++i)
		lst = ___push(stack.start[i], lst);
	stack.top = stack.start;
	stack.start = tmp_stack_start;
	return lst;
}

static STRING ___substring(NUMBER startf, NUMBER endf, STRING str)
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
	throw_error_fmt("Invalid range %.14g..%.14g", startf, endf);
	#ifdef __TINYC__
	return NULL;
	#endif
}


static STRING ___input(void)
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

static NUMBER ___number(STRING str)
{
	// casts string to number.
	char* end;
	NUMBER num = strtod(str, &end);
	if (end == str || *end != '\0') goto cannot_parse;
	return num;
cannot_parse:
	throw_error_fmt("Cannot parse '%.32s' to a number", str);
	#ifdef __TINYC__
	return 0;
	#endif
}

static STRING ___path(void)
{
	assert_impure();
	char buf[FILENAME_MAX];
	if (!getcwd(buf, FILENAME_MAX))
		throw_error("cannot get working directory");
	char* ret = gc_strdup(buf);
	return ret;
}

static LIST ___stack(void)
{
	LIST lst = NULL;
	for (size_t i = 0; i + stack.start < stack.top; ++i)
	{
		cognate_list* tmp = gc_malloc (sizeof *tmp);
		tmp -> object = stack.start[i];
		tmp -> next = lst;
		lst = tmp;
		gc_mark_ptr((void*)&tmp->next);
		gc_mark_any((void*)&tmp->object);
	}
	return lst;
}

static LIST ___parameters(void)
{
	return cmdline_parameters; // TODO should be a variable, and allow mutation and stuff
}

static void ___stop(void)
{
	assert_impure();
	// Don't check stack length, because it probably wont be empty.
	exit(EXIT_SUCCESS);
}


static NUMBER ___ordinal(STRING str)
{
	if unlikely(!str[0] || strlen(str) > (size_t)mblen(str, MB_CUR_MAX))
		throw_error_fmt("Invalid string '%.32s' (should be length 1)", str);
	wchar_t chr = 0;
	mbtowc(&chr, str, MB_CUR_MAX);
	return chr;
}

static STRING ___character(NUMBER d)
{
	const wchar_t i = d;
	char* const str = gc_malloc (MB_CUR_MAX + 1);
	if unlikely(i != d || wctomb(str, i) == -1)
		throw_error_fmt("Cannot convert %.14g to UTF8 character", d);
	str[mblen(str, MB_CUR_MAX)] = '\0';
	return str;
}

static NUMBER ___floor(NUMBER a)
{
	return floor(a);
}

static NUMBER ___round(NUMBER a)
{
	return round(a);
}

static NUMBER ___ceiling(NUMBER a)
{
	return ceil(a);
}

static NUMBER ___abs(NUMBER a)
{
	return fabs(a);
}

static void ___error(STRING str)
{
	throw_error(str);
}

static void ___wait(NUMBER seconds)
{
	assert_impure();
	sleep(seconds);
}

/*
static BLOCK ___precompute(BLOCK blk)
{
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
*/

static STRING ___show(ANY o)
{
	if ((o & STRING_TYPE) == STRING_TYPE || (o & SYMBOL_TYPE) == SYMBOL_TYPE)
		return (STRING)(o & UNALIGNED_PTR_MASK);
	show_object(o, general_purpose_buffer, NULL);
	return general_purpose_buffer;
}

static LIST ___split(STRING sep, STRING str)
{
	if (!*sep) throw_error("Seperator cannot be empty");
	LIST lst1 = NULL;
	size_t len = strlen(sep);
	char* found;
	while ((found = strstr(str, sep)))
	{
		found = strstr(str, sep);
		if (found != str)
		{
			char* item = gc_malloc(found - str + 1);
			memcpy(item, str, found - str);
			item[found - str] = '\0';
			lst1 = ___push(box_STRING(item), lst1);
		}
		str = found + len;
	}
	if (*str) lst1 = ___push(box_STRING(str), lst1);
	LIST lst = NULL;
	for (; lst1 ; lst1 = lst1->next) lst = ___push(lst1->object, lst);
	return lst;
}

static STRING ___uppercase(STRING str)
{
	char* converted = gc_strdup((char*)str);
	int len = 0;
	for (char* c = converted; *c; c += len)
	{
		wchar_t chr = 0;
		len = mblen(c, MB_CUR_MAX);
		mbtowc(&chr, c, len);
		chr = towupper(chr);
		wctomb(c, chr);
	}
	return converted;
}

static STRING ___lowercase(STRING str)
{
	char* converted = gc_strdup((char*)str);
	int len = 0;
	for (char* c = converted; *c; c += len)
	{
		wchar_t chr = 0;
		len = mblen(c, MB_CUR_MAX);
		mbtowc(&chr, c, len);
		chr = towlower(chr);
		wctomb(c, chr);
	}
	return converted;
}

/*
static BLOCK ___remember(BLOCK b)
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
*/

/*
static BLOCK ___pure(BLOCK b)
{
	return Block_copy(^{
		pure = 1;
		b();
		pure = 0;
	});
}
*/


static BOX ___box(ANY a) // boxes seem to break the GC sometimes TODO
{
	ANY* b = gc_malloc_mutable(sizeof *b);
	*b = a;
	gc_mark_mutable_any(b);
	return b;
}

static ANY ___unbox(BOX b)
{
	return *b;
}

static void ___set(BOX b, ANY a)
{
	*b = a;
	gc_mark_mutable_any(b);
}

/* math */

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// helper for math functions
static inline NUMBER radians_to_degrees(NUMBER a)
{
	return a * (180 / M_PI);
}

static inline NUMBER degrees_to_radians(NUMBER a)
{
	return a * (M_PI / 180);
}

static NUMBER ___sind(NUMBER a)
{
	double rad = degrees_to_radians(a);
	double sinrad = sin(rad);
	return sinrad;
}

static NUMBER ___cosd(NUMBER a)
{
	double rad = degrees_to_radians(a);
	double cosrad = cos(rad);
	return cosrad;
}

static NUMBER ___tand(NUMBER a)
{
	double rad = degrees_to_radians(a);
	double tanrad = tan(rad);
	return tanrad;
}

static NUMBER ___sin(NUMBER a)
{
	return sin(a);
}

static NUMBER ___cos(NUMBER a)
{
	return cos(a);
}

static NUMBER ___tan(NUMBER a)
{
	return tan(a);
}

static NUMBER ___exp(NUMBER a)
{
	return exp(a);
}

static NUMBER ___log(NUMBER a, NUMBER b)
{
	/* This uses the following formula:
	   log_x(y) =
	   	    log_e(y) / log_e(x)
	*/
	const double top = log(b);
	const double bottom = log(a);
	return top / bottom;
}

static NUMBER ___ln(NUMBER a)
{
	return log(a);
}


static NUMBER ___asind(NUMBER a)
{
	return radians_to_degrees(asin(a));
}

static NUMBER ___acosd(NUMBER a)
{
	return radians_to_degrees(acos(a));
}

static NUMBER ___atand(NUMBER a)
{
	return radians_to_degrees(atan(a));
}

static NUMBER ___asin(NUMBER a)
{
	return asin(a);
}

static NUMBER ___acos(NUMBER a)
{
	return acos(a);
}

static NUMBER ___atan(NUMBER a)
{
  	return atan(a);
}

static NUMBER ___sinhd(NUMBER a)
{
	return sinh(degrees_to_radians(a));
}

static NUMBER ___coshd(NUMBER a)
{
	return cosh(degrees_to_radians(a));
}

static NUMBER ___tanhd(NUMBER a)
{
	return tanh(degrees_to_radians(a));
}

static NUMBER ___sinh(NUMBER a)
{
	return sinh(a);
}

static NUMBER ___cosh(NUMBER a)
{
	return cosh(a);
}

static NUMBER ___tanh(NUMBER a)
{
	return tanh(a);
}

static IO ___open(SYMBOL m, STRING path)
{
	assert_impure();
	char* mode;
	if (m == SYMread) mode = "r";
	else if (m == SYMwrite) mode = "w";
	else if (m == SYMappend) mode = "a";
	else if (m == SYMreadHappend) mode = "a+";
	else if (m == SYMreadHwrite) mode = "w+";
	else if (m == SYMreadHwriteHexisting) mode = "r+";
	else throw_error("Expected one of \\read, \\write, \\append, \\read-write, \\read-append, \\read-write-existing");
	FILE* fp = fopen(path, mode);
	if unlikely(!fp) throw_error_fmt("Cannot open file '%s'", path);
	IO io = gc_malloc(sizeof *io);
	io->path = path;
	io->mode = mode;
	io->file = fp;
	gc_mark_ptr((void*)&io->path);
	//gc_mark_ptr((void*)&io->mode);
	//gc_mark_ptr((void*)&io->file);
	return io;
}

static STRING ___readHfile(IO io)
{
	assert_impure();
	// Read a file to a string.
	FILE *fp = io->file;
	fseek(fp, 0, SEEK_SET); // seek to beginning
	if unlikely(!io->mode) throw_error_fmt("File '%s' is not open", io->path);
	if unlikely(fp == NULL) throw_error_fmt("Cannot open file '%s'", io->path);
	struct stat st;
	fstat(fileno(fp), &st);
	char* const text = gc_malloc (st.st_size + 1);
	if (fread(text, sizeof(char), st.st_size, fp) != (unsigned long)st.st_size)
		throw_error_fmt("Error reading file '%s'", io->path);
	text[st.st_size] = '\0'; // Remove trailing eof.
	return text;
}

static STRING ___readHline(IO io)
{
	assert_impure();
	char* buf = (char*)general_purpose_buffer;
	fgets(buf, INT_MAX, io->file);
	return gc_strdup(buf); // this can only GC once so won't overwrite the buffer.
}

static void ___close(IO io)
{
	assert_impure();
	fclose(io->file);
	io->file = NULL;
}

static BOOLEAN ___openQ(IO io)
{
	return io->file ? true : false;
}

static STRING ___fileHname(IO io)
{
	return io->path;
}

static STRING ___fileHmode(IO io)
{
	return io->mode; // TODO symbol
}

static void ___write(STRING s, IO io)
{
	fputs(s, io->file);
}

static void ___seek(SYMBOL ref, NUMBER n, IO io)
{
	int pos;
	if (ref == SYMstart) pos = SEEK_SET;
	else if (ref == SYMend) pos = SEEK_END;
	else if (ref == SYMcurrent) pos = SEEK_CUR;
	else throw_error_fmt("Expected one of \\start, \\end, \\current");
	long offset = n;
	if unlikely(offset != n || fseek(io->file, offset, pos))
		throw_error_fmt("Can't seek to position %.14g relative to %s", n, ref);
}

static void invalid_jump(ANY* env)
{
	throw_error("Cannot resume expired continuation");
}

static void oh_no(ANY* env)
{
#ifdef DEBUG
	// Remove all the now-invalid backtraces.
	while ((char*)trace < *(char**)(1 + (jmp_buf*)env)) trace = trace->next;
#endif
	longjmp(*(jmp_buf*)env, 1);
}

__attribute__((returns_twice))
static void ___begin(BLOCK f)
{
#ifdef DEBUG
	BLOCK a = gc_malloc(sizeof *a + sizeof(jmp_buf) + sizeof(char*));
	// Add the address of the stack pointer so we know which backtraces to pop.
	char c;
	*(char**)(1 + (jmp_buf*)&a->env) = &c;
#else
	BLOCK a = gc_malloc(sizeof *a + sizeof(jmp_buf));
#endif
	for (uintptr_t* p = (uintptr_t*)&a->env ; (char*)p < (char*)&a->env + sizeof(jmp_buf) ; ++p)
		gc_mark_ptr(p);
	if (!setjmp(*(jmp_buf*)&a->env))
	{
		a->fn = oh_no;
		push(box_BLOCK(a));
		call_block(f);
		a->fn = invalid_jump;
	}
}

static LIST ___empty (void)
{
	return NULL;
}

static TABLE ___table (BLOCK expr)
{
	ANYPTR tmp_stack_start = stack.start;
	stack.start = stack.top;
	// Eval expr
	call_block(expr);
	// Move to a table.
	TABLE d = NULL;
	size_t len = stack_length();
	if unlikely(len & 1) throw_error("Table initialiser must be key-value pairs");
	for (size_t i = 0; i < len; i += 2)
	{
		ANY key = stack.start[i+1];
		ANY value = stack.start[i];
		d = ___insert(key, value, d);
	}
	stack.top = stack.start;
	stack.start = tmp_stack_start;
	return d;
}

static TABLE mktable(ANY key, ANY value, TABLE left, TABLE right, size_t level)
{
	TABLE t = gc_malloc(sizeof *t);
	t->key = key;
	t->value = value;
	t->left = left;
	t->right = right;
	t->level = level;
	gc_mark_ptr((void*)&t->left);
	gc_mark_ptr((void*)&t->right);
	gc_mark_any(&t->key);
	gc_mark_any(&t->value);
	return t;
}

static TABLE ___insert(ANY key, ANY value, TABLE d)
{
	cognate_type t = TYPE_MASK & key;
	if unlikely(t == IO_TYPE || t == BLOCK_TYPE || t == BOX_TYPE) throw_error_fmt("Can't index a table with %s", ___show(key));
	if (!d) return mktable(key, value, NULL, NULL, 1);
	ptrdiff_t diff = compare_objects(d->key, key);
	if (diff == 0) return mktable(key, value, d->left, d->right, d->level);
	if (diff > 0)
		return
			table_split(table_skew(
				mktable(d->key, d->value, ___insert(key, value, d->left), d->right, d->level)));
	else //if (diff < 0)
		return
			table_split(table_skew(
				mktable(d->key, d->value, d->left, ___insert(key, value, d->right), d->level)));
}

static ANY ___D(ANY key, TABLE d)
{
	cognate_type t = TYPE_MASK & key;
	if unlikely(t == IO_TYPE || t == BLOCK_TYPE || t == BOX_TYPE) throw_error_fmt("Can't index a table with %s", ___show(key));
	while (d)
	{
		ptrdiff_t diff = compare_objects(d->key, key);
		if (diff == 0) return d->value;
		else if (diff > 0) d = d->left;
		else d = d->right;
	}

	throw_error_fmt("%s is not in table", ___show(key));
	#ifdef __TINYC__
	return NIL;
	#endif
}

static BOOLEAN ___has(ANY key, TABLE d)
{
	cognate_type t = TYPE_MASK & key;
	if unlikely(t == IO_TYPE || t == BLOCK_TYPE || t == BOX_TYPE) throw_error_fmt("Can't index a table with %s", ___show(key));
	while (d)
	{
		ptrdiff_t diff = compare_objects(d->key, key);
		if (diff == 0) return true;
		else if (diff > 0) d = d->left;
		else d = d->right;
	}

	return false;
}

static TABLE ___remove(ANY key, TABLE T)
{
	// input: X, the key to delete, and T, the root of the tree from which it should be deleted.
   // output: T, balanced, without the value X.
	cognate_type t = TYPE_MASK & key;
	if unlikely(t == IO_TYPE || t == BLOCK_TYPE || t == BOX_TYPE) throw_error_fmt("Can't index a table with %s", ___show(key));
	if (!T) throw_error_fmt("Key %s not in table", ___show(key));
	ptrdiff_t diff = compare_objects(T->key, key);
	TABLE T2 = NULL;
	// This part is fairly intuitive - if this breaks it's probably not here:
	if (diff < 0)
		T2 = mktable(T->key, T->value, T->left, ___remove(key, T->right), T->level);
	else if (diff > 0)
		T2 = mktable(T->key, T->value, ___remove(key, T->left), T->right, T->level);
	else if (!T->left && !T->right) return NULL;
	else if (!T->left) // T->right not null
	{
		TABLE L = T->right;
		while (L->left) L = L->left; // successor
		T2 = mktable(L->key, L->value, T->left, ___remove(L->key, T->right), L->level);
	}
	else // left and right not null
	{
		TABLE L = T->left;
		while (L->right) L = L->right; // predecessor
		T2 = mktable(L->key, L->value, ___remove(L->key, T->left), T->right, L->level);
	}

	// below here idk really what's going on, but it seems to work:

	if (T2->left && T2->right)
	{
		long llevel = T2->left->level;
		long rlevel = T2->right->level;
		long should_be = 1 + (llevel < rlevel ? llevel : rlevel);

		if (should_be < T2->level)
		{
			T2->level = should_be;
			if (should_be < T2->right->level)
				T2->right->level = should_be;
		}
	}

	// This part makes at least vague sense:

	/* Commented these bits out because the GC can't handle them
	 * Probably worth implementing a recursive skew and split at some point.
	 * IDK how balanced the table is gonna be after this
	if (T2->right)
	{
		T2->right->right = table_skew(T2->right->right);
		T2->right = table_skew(T2->right);
	}
	*/
	T2 = table_skew(T2);
	//if (T2->right) T2->right = table_split(T2->right);
	T2 = table_split(T2);

	return T2;
}

static LIST values_helper(TABLE T, LIST L)
{
	if (!T) return L;
	return values_helper(T->left, ___push(T->value, values_helper(T->right, L)));
}

static LIST ___values(TABLE T)
{
	return values_helper(T, NULL);
}

static LIST keys_helper(TABLE T, LIST L)
{
	if (!T) return L;
	return keys_helper(T->left, ___push(T->key, keys_helper(T->right, L)));
}

static LIST ___keys(TABLE T)
{
	return keys_helper(T, NULL);
}

static NUMBER ___length_TABLE(TABLE T)
{
	if (!T) return 0;
	return 1 + ___length_TABLE(T->left) + ___length_TABLE(T->right);
}

static NUMBER ___length_LIST(LIST l)
{
	size_t len = 0;
	while (l)
	{
		len++;
		l = l->next;
	}
	return (NUMBER)len;
}

static NUMBER ___length_STRING(STRING str)
{
	size_t len = 0;
	for (; *str ; str += mblen(str, MB_CUR_MAX), ++len);
	return len;
}

static NUMBER ___length(ANY a)
{
	switch(type_of(a))
	{
		case LIST_TYPE:   return ___length_LIST(unbox_LIST(a));
		case STRING_TYPE: return ___length_STRING(unbox_STRING(a));
		case TABLE_TYPE:  return ___length_TABLE(unbox_TABLE(a));
		default: type_error("list or string or table", a);
	}
#ifdef __TINYC__
	return 0;
#endif
}

static STRING ___show_NUMBER(NUMBER a)
{
	show_number(a, (char*)general_purpose_buffer);
	return gc_strdup((char*)general_purpose_buffer);
}

static STRING ___show_TABLE(TABLE a)
{
	show_table(a, (char*)general_purpose_buffer, NULL);
	return gc_strdup((char*)general_purpose_buffer);
}

static STRING ___show_IO(IO a)
{
	show_io(a, (char*)general_purpose_buffer);
	return gc_strdup((char*)general_purpose_buffer);
}

static STRING ___show_STRING(STRING s)
{
	return s;
}

static STRING ___show_BOOLEAN(BOOLEAN b)
{
	return b ? "True" : "False";
}

static STRING ___show_SYMBOL(SYMBOL s)
{
	return s;
}

static STRING ___show_BLOCK(BLOCK b)
{
	show_block(b, (char*)general_purpose_buffer);
	return gc_strdup((char*)general_purpose_buffer);
}

static STRING ___show_BOX(BOX b)
{
	show_box(b, (char*)general_purpose_buffer, NULL);
	return gc_strdup((char*)general_purpose_buffer);
}

static STRING ___show_LIST(LIST l)
{
	show_list(l, (char*)general_purpose_buffer, NULL);
	return gc_strdup((char*)general_purpose_buffer);
}



static regex_t* memoized_regcomp(STRING reg_str)
{
	regex_t* reg;
	if (___has(box_STRING(reg_str), memoized_regexes)) reg = (regex_t*)unbox_STRING(___D(box_STRING(reg_str), memoized_regexes));
	else
	{
		reg = gc_malloc(sizeof *reg);
		const int status = regcomp(reg, reg_str, REG_EXTENDED | REG_NEWLINE);
		errno = 0; // Hmmm
		if unlikely(status)
		{
			char reg_err[256];
			regerror(status, reg, reg_err, 256);
			throw_error_fmt("Compile error (%s) in regex '%.32s'", reg_err, reg_str);
		}
		memoized_regexes = ___insert(box_STRING(reg_str), box_STRING((char*)reg), memoized_regexes);
	}

	return reg;
}

static BOOLEAN ___regex(STRING reg_str, STRING str)
{
	regex_t* reg = memoized_regcomp(reg_str);
	const int found = regexec(reg, str, 0, NULL, 0);
	if unlikely(found != 0 && found != REG_NOMATCH)
		throw_error_fmt("Regex failed matching string '%.32s'", str);

	return found != REG_NOMATCH;
}

static BOOLEAN ___regexHmatch(STRING reg_str, STRING str)
{
	regex_t* reg = memoized_regcomp(reg_str);

	size_t groups = reg->re_nsub + 1;
	regmatch_t matches[groups];
	const int found = regexec(reg, str, groups, matches, 0);
	if unlikely(found != 0 && found != REG_NOMATCH) throw_error_fmt("Regex failed matching string '%.32s'", str);

	if (found == 0) {
		for (unsigned int g = 1; g < groups ; g++)
		{
			size_t from = matches[g].rm_so;
			if (from == (size_t)-1)
			{
				groups = g;
				break;
			}
		}

		for (unsigned int g = groups-1; g > 0; g--)
		{
			size_t from = matches[g].rm_so;
			size_t to = matches[g].rm_eo;
			char* item = gc_strndup((char*)str, to);
			push(box_STRING(item + from));
		}
	}
	return found != REG_NOMATCH;
}

static LIST ___append_LIST(LIST l1, LIST l2)
{
	if (!l2) return l1;
	else return ___push(___first_LIST(l2), ___append_LIST(l1, ___rest_LIST(l2)));
}

static STRING ___append_STRING(STRING s1, STRING s2)
{
	size_t len1 = strlen(s1);
	size_t len2 = strlen(s2);
	char* output = gc_malloc(len1 + len2 + 1);
	strcpy(output, s2);
	strcpy(output+len2, s1);
	output[len1+len2] = '\0';
	return (STRING)output;
}

static ANY ___append(ANY a1, ANY a2)
{
	cognate_type t = type_of(a1);
	switch (t)
	{
		case LIST_TYPE:
			return box_LIST(___append_LIST(unbox_LIST(a1), unbox_LIST(a2)));
		case STRING_TYPE:
			return box_STRING(___append_STRING(unbox_STRING(a1), unbox_STRING(a2)));
		default: type_error("List or String", a1);
	}
	#ifdef __TINYC__
	return NIL;
	#endif
}

static BOOLEAN ___numberQ_NUMBER(NUMBER _)    { return true;  }
static BOOLEAN ___numberQ_LIST(LIST _)        { return false; }
static BOOLEAN ___numberQ_BOX(BOX _)          { return false; }
static BOOLEAN ___numberQ_TABLE(TABLE _)      { return false; }
static BOOLEAN ___numberQ_IO(IO _)            { return false; }
static BOOLEAN ___numberQ_BOOLEAN(BOOLEAN _)  { return false; }
static BOOLEAN ___numberQ_STRING(STRING _)    { return false; }
static BOOLEAN ___numberQ_SYMBOL(SYMBOL _)    { return false; }
static BOOLEAN ___numberQ_BLOCK(BLOCK _)      { return false; }

static BOOLEAN ___listQ_NUMBER(NUMBER _)      { return false; }
static BOOLEAN ___listQ_LIST(LIST _)          { return true;  }
static BOOLEAN ___listQ_BOX(BOX _)            { return false; }
static BOOLEAN ___listQ_TABLE(TABLE _)        { return false; }
static BOOLEAN ___listQ_IO(IO _)              { return false; }
static BOOLEAN ___listQ_BOOLEAN(BOOLEAN _)    { return false; }
static BOOLEAN ___listQ_STRING(STRING _)      { return false; }
static BOOLEAN ___listQ_SYMBOL(SYMBOL _)      { return false; }
static BOOLEAN ___listQ_BLOCK(BLOCK _)        { return false; }

static BOOLEAN ___boxQ_NUMBER(NUMBER _)       { return false; }
static BOOLEAN ___boxQ_LIST(LIST _)           { return false; }
static BOOLEAN ___boxQ_BOX(BOX _)             { return true;  }
static BOOLEAN ___boxQ_TABLE(TABLE _)         { return false; }
static BOOLEAN ___boxQ_IO(IO _)               { return false; }
static BOOLEAN ___boxQ_BOOLEAN(BOOLEAN _)     { return false; }
static BOOLEAN ___boxQ_STRING(STRING _)       { return false; }
static BOOLEAN ___boxQ_SYMBOL(SYMBOL _)       { return false; }
static BOOLEAN ___boxQ_BLOCK(BLOCK _)         { return false; }

static BOOLEAN ___tableQ_NUMBER(NUMBER _)     { return false; }
static BOOLEAN ___tableQ_LIST(LIST _)         { return false; }
static BOOLEAN ___tableQ_BOX(BOX _)           { return false; }
static BOOLEAN ___tableQ_TABLE(TABLE _)       { return true;  }
static BOOLEAN ___tableQ_IO(IO _)             { return false; }
static BOOLEAN ___tableQ_BOOLEAN(BOOLEAN _)   { return false; }
static BOOLEAN ___tableQ_STRING(STRING _)     { return false; }
static BOOLEAN ___tableQ_SYMBOL(SYMBOL _)     { return false; }
static BOOLEAN ___tableQ_BLOCK(BLOCK _)       { return false; }

static BOOLEAN ___ioQ_NUMBER(NUMBER _)        { return false; }
static BOOLEAN ___ioQ_LIST(LIST _)            { return false; }
static BOOLEAN ___ioQ_BOX(BOX _)              { return false; }
static BOOLEAN ___ioQ_TABLE(TABLE _)          { return false; }
static BOOLEAN ___ioQ_IO(IO _)                { return true;  }
static BOOLEAN ___ioQ_BOOLEAN(BOOLEAN _)      { return false; }
static BOOLEAN ___ioQ_STRING(STRING _)        { return false; }
static BOOLEAN ___ioQ_SYMBOL(SYMBOL _)        { return false; }
static BOOLEAN ___ioQ_BLOCK(BLOCK _)          { return false; }

static BOOLEAN ___booleanQ_NUMBER(NUMBER _)   { return false; }
static BOOLEAN ___booleanQ_LIST(LIST _)       { return false; }
static BOOLEAN ___booleanQ_BOX(BOX _)         { return false; }
static BOOLEAN ___booleanQ_TABLE(TABLE _)     { return false; }
static BOOLEAN ___booleanQ_IO(IO _)           { return false; }
static BOOLEAN ___booleanQ_BOOLEAN(BOOLEAN _) { return true;  }
static BOOLEAN ___booleanQ_STRING(STRING _)   { return false; }
static BOOLEAN ___booleanQ_SYMBOL(SYMBOL _)   { return false; }
static BOOLEAN ___booleanQ_BLOCK(BLOCK _)     { return false; }

static BOOLEAN ___stringQ_NUMBER(NUMBER _)    { return false; }
static BOOLEAN ___stringQ_LIST(LIST _)        { return false; }
static BOOLEAN ___stringQ_BOX(BOX _)          { return false; }
static BOOLEAN ___stringQ_TABLE(TABLE _)      { return false; }
static BOOLEAN ___stringQ_IO(IO _)            { return false; }
static BOOLEAN ___stringQ_BOOLEAN(BOOLEAN _)  { return false; }
static BOOLEAN ___stringQ_STRING(STRING _)    { return true;  }
static BOOLEAN ___stringQ_SYMBOL(SYMBOL _)    { return false; }
static BOOLEAN ___stringQ_BLOCK(BLOCK _)      { return false; }

static BOOLEAN ___symbolQ_NUMBER(NUMBER _)    { return false; }
static BOOLEAN ___symbolQ_LIST(LIST _)        { return false; }
static BOOLEAN ___symbolQ_BOX(BOX _)          { return false; }
static BOOLEAN ___symbolQ_TABLE(TABLE _)      { return false; }
static BOOLEAN ___symbolQ_IO(IO _)            { return false; }
static BOOLEAN ___symbolQ_BOOLEAN(BOOLEAN _)  { return false; }
static BOOLEAN ___symbolQ_STRING(STRING _)    { return false; }
static BOOLEAN ___symbolQ_SYMBOL(SYMBOL _)    { return true;  }
static BOOLEAN ___symbolQ_BLOCK(BLOCK _)      { return false; }

static BOOLEAN ___blockQ_NUMBER(NUMBER _)     { return false; }
static BOOLEAN ___blockQ_LIST(LIST _)         { return false; }
static BOOLEAN ___blockQ_BOX(BOX _)           { return false; }
static BOOLEAN ___blockQ_TABLE(TABLE _)       { return false; }
static BOOLEAN ___blockQ_IO(IO _)             { return false; }
static BOOLEAN ___blockQ_BOOLEAN(BOOLEAN _)   { return false; }
static BOOLEAN ___blockQ_STRING(STRING _)     { return false; }
static BOOLEAN ___blockQ_SYMBOL(SYMBOL _)     { return false; }
static BOOLEAN ___blockQ_BLOCK(BLOCK _)       { return true;  }

static BOOLEAN ___EE_NUMBER(NUMBER n1, NUMBER n2)    { return !compare_numbers(n1, n2); }
static BOOLEAN ___EE_LIST(LIST l1, LIST l2)          { return !compare_lists(l1, l2); }
static BOOLEAN ___EE_BOX(BOX b1, BOX b2)             { return !compare_boxes(b1, b2); }
static BOOLEAN ___EE_TABLE(TABLE t1, TABLE t2)       { return !compare_tables(t1, t2); }
static BOOLEAN ___EE_IO(IO i1, IO i2)                { return !compare_io(i1, i2); }
static BOOLEAN ___EE_BOOLEAN(BOOLEAN b1, BOOLEAN b2) { return !compare_booleans(b1, b2); }
static BOOLEAN ___EE_STRING(STRING s1, STRING s2)    { return !compare_strings(s1, s2); }
static BOOLEAN ___EE_SYMBOL(SYMBOL s1, SYMBOL s2)    { return !compare_symbols(s1, s2); }
static BOOLEAN ___EE_BLOCK(BLOCK b1, BLOCK b2)       { return !compare_blocks(b1, b2); }

// ---------- ACTUAL PROGRAM ----------
