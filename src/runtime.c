// ---------- RUNTIME HEADER ----------
#define _GNU_SOURCE
#define _FORTIFY_SOURCE 2

#include <stddef.h>
#include <stdio.h>
#include <ctype.h>
#include <Block.h>
#include <assert.h>
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
typedef ANY* restrict BOX;
typedef void(^BLOCK)();
typedef _Bool BOOLEAN;
typedef double NUMBER;
typedef const char* restrict STRING;
typedef const struct cognate_list* restrict LIST;
typedef const char* restrict SYMBOL;
typedef struct cognate_record* restrict RECORD;

typedef enum cognate_type
{
	box     = 1,
	boolean = 2,
	string  = 3,
	list    = 4,
	record  = 5,
	block   = 6,
	symbol  = 7,
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

#ifdef DEBUG

typedef struct backtrace
{
	const struct backtrace* restrict next;
	const char* name;
	const size_t line;
	const size_t col;
} backtrace;

typedef struct var_info
{
	const struct var_info* restrict next;
	const char* name;
	const ANY value;
} var_info;

#endif

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
	if unlikely(pure) throw_error("Cannot mutate variable in pure function"); \
	VAR(name) = val;

#define unlikely(expr) (__builtin_expect((_Bool)(expr), 0))
#define likely(expr)	 (__builtin_expect((_Bool)(expr), 1))

#define ALLOC_RECORD(n) (gc_malloc(sizeof(size_t)+n*sizeof(ANY)))

static ANY* space[2] = {NULL,NULL};
static char* bitmap[2] = {NULL,NULL};
static size_t alloc[2] = {0, 0};
static _Bool z = 0;

static _Bool pure = 0;

static size_t system_memory;

// Global variables
static cognate_stack stack;
static LIST cmdline_parameters = NULL;
#ifdef DEBUG
static const backtrace* trace = NULL;
static const var_info* vars = NULL;
#endif

extern char *record_info[][64];
extern char *source_file_lines[];
extern _Bool breakpoints[];
extern const size_t source_line_num;

extern int main(int, char**);

static const char* restrict function_stack_top;
static const char* restrict function_stack_start;
static rlim_t function_stack_size;

// Variables and	needed by functions.c defined in runtime.c
static void init_stack(void);
static void check_record_id(size_t, RECORD);
static void set_function_stack_start(void);
static void expand_stack(void);
static char* show_object(const ANY object, const _Bool);
static void _Noreturn __attribute__((format(printf, 1, 2))) throw_error_fmt(const char* restrict const, ...);
static void _Noreturn throw_error(const char* restrict const);
static _Bool compare_objects(ANY, ANY);
static _Bool match_objects(ANY, ANY);
static void destructure_lists(LIST, LIST);
static void destructure_records(RECORD, RECORD);
static void destructure_objects(ANY, ANY);
#ifdef DEBUG
static void print_backtrace(int, const backtrace*, int);
#endif

static void* gc_malloc(size_t);
static void gc_collect(void);
static void gc_init(void);
static char* gc_strdup(char*);
static char* gc_strndup(char*, size_t);

#define gc_new(t) (t*) gc_malloc (sizeof(t))

// Variables and functions needed by compiled source file defined in runtime.c
static cognate_type get_type(ANY);
static _Bool is_nan(ANY);
static NUMBER unbox_number(ANY);
static BOX unbox_box(ANY);
static ANY box_box(BOX);
static ANY box_number(NUMBER);
static BOOLEAN unbox_boolean(ANY);
static ANY box_boolean(BOOLEAN);
static STRING unbox_string(ANY);
static ANY box_string(STRING);
static LIST unbox_list(ANY);
static ANY box_list(LIST);
static RECORD unbox_record(ANY);
static ANY box_record(RECORD);
static SYMBOL unbox_symbol(ANY);
static ANY box_symbol(SYMBOL);
static BLOCK unbox_block(ANY);
static ANY box_block(BLOCK);

static void init(int, char **);
static void cleanup(void);
static void push(ANY);
static ANY pop(void);
static ANY peek(void);
static void flush_stack_cache(void);
static int stack_length(void);
static void check_function_stack_size(void);

// Builtin functions needed by compiled source file defined in functions.c
static ANY VAR(if)(BOOLEAN, ANY, ANY);
static void VAR(when)(BOOLEAN, BLOCK);
static void VAR(unless)(BOOLEAN, BLOCK);
static void VAR(while)(BLOCK, BLOCK);
static void VAR(until)(BLOCK, BLOCK);
static void VAR(do)(BLOCK);
static void VAR(put)(ANY);
static void VAR(print)(ANY);
static NUMBER VAR(P)(NUMBER, NUMBER);
static NUMBER VAR(M)(NUMBER, NUMBER);
static NUMBER VAR(D)(NUMBER, NUMBER);
static NUMBER VAR(S)(NUMBER, NUMBER);
static NUMBER VAR(modulo)(NUMBER, NUMBER);
static NUMBER VAR(sqrt)(NUMBER);
static NUMBER VAR(random)(NUMBER, NUMBER);
static void VAR(clear)(void);
static BOOLEAN VAR(true);
static BOOLEAN VAR(false);
static BOOLEAN VAR(either)(BOOLEAN, BOOLEAN);
static BOOLEAN VAR(both)(BOOLEAN, BOOLEAN);
static BOOLEAN VAR(oneDof)(BOOLEAN, BOOLEAN);
static BOOLEAN VAR(not)(BOOLEAN);
static BOOLEAN VAR(EE)(ANY, ANY);
static BOOLEAN VAR(SE)(ANY, ANY);
static BOOLEAN VAR(L)(NUMBER, NUMBER);
static BOOLEAN VAR(G)(NUMBER, NUMBER);
static BOOLEAN VAR(LE)(NUMBER, NUMBER);
static BOOLEAN VAR(GE)(NUMBER, NUMBER);
static BOOLEAN VAR(match)(ANY, ANY);
static BOOLEAN VAR(anyQ)(ANY);
static BOOLEAN VAR(numberQ)(ANY);
static BOOLEAN VAR(symbolQ)(ANY);
static BOOLEAN VAR(listQ)(ANY);
static BOOLEAN VAR(stringQ)(ANY);
static BOOLEAN VAR(blockQ)(ANY);
static BOOLEAN VAR(booleanQ)(ANY);
static BOOLEAN VAR(integerQ)(ANY);
static BOOLEAN VAR(zeroQ)(ANY);
static ANY VAR(first)(LIST);
static LIST VAR(rest)(LIST);
static STRING VAR(head)(STRING);
static STRING VAR(tail)(STRING);
static LIST VAR(push)(ANY, LIST);
static BOOLEAN VAR(emptyQ)(LIST);
static LIST VAR(list)(BLOCK);
static STRING VAR(join)(NUMBER);
static NUMBER VAR(stringDlength)(STRING);
static STRING VAR(substring)(NUMBER, NUMBER, STRING);
static STRING VAR(input)(void);
static STRING VAR(read)(STRING);
static NUMBER VAR(number)(STRING);
static STRING VAR(path)(void);
static LIST VAR(stack)(void);
static void VAR(write)(STRING, ANY);
static LIST VAR(parameters)(void);
static void VAR(stop)(void);
static STRING VAR(show)(ANY);
static BOOLEAN VAR(matchDregex)(STRING, STRING);
static NUMBER VAR(ordinal)(STRING);
static STRING VAR(character)(NUMBER);
static NUMBER VAR(floor)(NUMBER);
static NUMBER VAR(round)(NUMBER);
static NUMBER VAR(ceiling)(NUMBER);
static void VAR(assert)(STRING, BOOLEAN);
static void VAR(error)(STRING);
static LIST VAR(map)(BLOCK, LIST);
static LIST VAR(filter)(BLOCK, LIST);
static void VAR(for)(LIST, BLOCK);
static LIST VAR(range)(NUMBER, NUMBER);
static ANY VAR(index)(NUMBER, LIST);
static void VAR(puts)(BLOCK);
static void VAR(prints)(BLOCK);
static BLOCK VAR(precompute)(BLOCK);
static void VAR(wait)(NUMBER);
static BLOCK VAR(case)(ANY, ANY, ANY);
static LIST VAR(split)(STRING, STRING);
static NUMBER VAR(length)(LIST);
static LIST VAR(take)(NUMBER,LIST);
static LIST VAR(takeDwhile)(BLOCK,LIST);
static LIST VAR(discard)(NUMBER,LIST);
static BLOCK VAR(remember)(BLOCK);
static BOOLEAN VAR(all)(BLOCK,LIST);

static const char *lookup_type(cognate_type);
static _Bool compare_lists(LIST, LIST);
static _Bool compare_records(RECORD, RECORD);
static _Bool match_records(RECORD, RECORD);
static _Bool match_lists(LIST, LIST);
static void handle_error_signal(int);
static void assert_impure();

#ifdef DEBUG
static _Bool debug = 0;
static size_t next_count = 0;
static size_t debug_lineno = 0;
#endif

static int _argc;
static char** _argv;

static void init(int argc, char** argv)
{
	_argc = argc;
	_argv = argv;
	struct rlimit stack_limit;
	if unlikely(getrlimit(RLIMIT_STACK, &stack_limit) == -1)
		throw_error("Cannot get return stack limit");
	function_stack_size = stack_limit.rlim_cur;
	SET_FUNCTION_STACK_START();
	if (function_stack_size == ULONG_MAX) function_stack_top = NULL;
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
#ifdef DEBUG
	if (getenv("COG_DEBUG")) debug=1;
#endif
}
static void cleanup(void)
{
	if unlikely(stack.top != stack.start || stack.cache != NIL_OBJ)
		throw_error_fmt("Exiting with %ti object(s) on the stack", stack.top - stack.start + (stack.cache != NIL_OBJ));
}

static void check_function_stack_size(void)
{
	const char sp;
	if unlikely(&sp < function_stack_top + STACK_MARGIN_KB * 1024)
		throw_error("Maximum recursion depth exceeded");
}

#ifdef DEBUG
static char* get_source_line(size_t line)
{
	// Length should be enough?
	return source_file_lines[line-1];
}

#define BACKTRACE_PUSH(NAME, LINE, COL) \
	const backtrace _trace_##LINE##_##COL = (backtrace) {.name = (#NAME), .line = (LINE), .col = (COL), .next=trace}; \
	trace = &_trace_##LINE##_##COL;

#define VARS_PUSH(NAME, IDENT) \
	const var_info _varinfo_##IDENT = (var_info) {.name = (#NAME), .value = VAR(IDENT), .next=vars}; \
	vars = &_varinfo_##IDENT;

#define BACKTRACE_POP() \
	trace = trace->next;

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
	print_backtrace(1, trace, 0);
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
			exit(main(_argc, _argv));
		case 's': case 'S':
			// Stack
			flush_stack_cache();
			for (ANY* a = stack.top - 1;  a >= stack.start; --a)
			{
				fputs(show_object(*a, 0), stderr);
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
				print_backtrace(int_arg, trace, 1);
			else print_backtrace(5, trace, 1);
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
			if (int_arg > source_line_num)
				fprintf(stderr, "Line %zi is beyond end of file.\n", int_arg);
			else if (int_arg) breakpoints[int_arg-1] = 1;
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
			for (const var_info* restrict v = vars; v; v = v->next)
			{
				if (!strcmp(v->name, str_arg))
				{
					fprintf(stderr, "%c%s = %s\n", toupper(*s), s+1, show_object(v->value, 0));
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

static void print_backtrace(int n, const backtrace* b, int arrow)
{
	if (!b || !n) return;
	int digits = 0;
	int len = strlen(b->name);
	for (size_t tmp = b->line; tmp /= 10; ++digits);
	char* ln = get_source_line(b->line);
	ssize_t col = b->col;
	while (*ln)
	{
		if (*ln != ' ' && *ln != '\t') break;
		ln++;
		col--;
	}
	fprintf(stderr, "\033[0;2m[%zi]\033[0m %.*s\033[0;1m%.*s\033[0m%s\n",
			b->line,
			(int)(col - len - 1), ln,
			len, ln + col - len - 1,
			ln + col - 1);
	if (!arrow) return;
	while (col-- + digits - len/2 + 2 > 0) fputs(" ", stderr);
	fputs("\033[31;1m^\033[0m\n", stderr);
	print_backtrace(n - 1, b->next, arrow);
}
#endif

static _Noreturn __attribute__((format(printf, 1, 2))) void throw_error_fmt(const char* restrict const fmt, ...)
{
	fputs("\n\n\033[31;1m\t", stderr);
	va_list args;
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	fputs("\n\n\033[0m", stderr);
#ifdef DEBUG
	if (isatty(fileno(stdin)))
	{
		debug = 1;
		debugger_step();
	} else print_backtrace(5, trace, 1);
#endif
	exit(EXIT_FAILURE);
}

static _Noreturn void throw_error(const char* restrict const msg)
{
	fputs("\n\n\033[31;1m\t", stderr);
	fputs(msg, stderr);
	fputs("\n\n\033[0m", stderr);
#ifdef DEBUG
	if (isatty(fileno(stdin)))
	{
		debug = 1;
		debugger_step();
	} else print_backtrace(5, trace, 1);
#endif
	exit(EXIT_FAILURE);
}

static void handle_error_signal(int sig)
{
	throw_error_fmt("Recieved signal %i (%s)", sig, strsignal(sig));
}

static void assert_impure()
{
	if unlikely(pure) throw_error("Invalid operation for pure function");
}


static char* show_object (const ANY object, const _Bool raw_strings)
{
	static char* buffer;
	static size_t depth = 0;
	if (depth++ == 0) buffer = (char*)gc_malloc(0); // i dont like resizing buffers
	switch (get_type(object))
	{
		case number: sprintf(buffer, "%.14g", unbox_number(object));
						 buffer += strlen(buffer);
						 break;
		case string:
			if (raw_strings)
				buffer += strlen(strcpy(buffer, unbox_string(object)));
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
					else *buffer++ = c;
				}
				*buffer++ = '\'';
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
		}
		break;
		case box:
			*buffer++ = '[';
			show_object(*unbox_box(object), 0);
			*buffer++ = ']';
			break;
	}
	depth--;
	if (depth) return NULL;
	*buffer++ = '\0';
	char* b = strdup(gc_malloc(0));
	char* c = gc_strdup(b);
	free(b);
	return c;
}

static void init_stack(void)
{
	stack.absolute_start = stack.top = stack.start
		= mmap(0, system_memory/10, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_NORESERVE, -1, 0);
	stack.cache = NIL_OBJ;
}

__attribute__((hot))
static void push(ANY object)
{
	if likely(stack.cache == NIL_OBJ) { stack.cache = object; return; }
	*stack.top++ = stack.cache;
	stack.cache = object;
}

__attribute__((hot))
static ANY pop(void)
{
	if likely(stack.cache != NIL_OBJ) { const ANY a = stack.cache; stack.cache = NIL_OBJ; return a; }
	if unlikely(stack.top == stack.start) throw_error("Stack underflow");
	return *--stack.top;
}

__attribute__((hot))
static ANY peek(void)
{
	if likely(stack.cache != NIL_OBJ) return stack.cache;
	if unlikely(stack.top == stack.start) throw_error("Stack underflow");
	return *(stack.top - 1);
}

static void flush_stack_cache(void)
{
	if (stack.cache == NIL_OBJ) return;
	push(stack.cache);
	pop();
}

static int stack_length(void)
{
	return stack.top - stack.start + (stack.cache != NIL_OBJ);
}

static const char* lookup_type(cognate_type type)
{
	switch(type)
	{
		case box:     return "box";
		case string:  return "string";
		case number:  return "number";
		case list:    return "list";
		case block:   return "block";
		case record:  return "record";
		case symbol:  return "symbol";
		case boolean: return "boolean";
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


static _Bool compare_objects(ANY ob1, ANY ob2)
{
	if (ob1 == ob2) return 1;
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
		case block:   throw_error("Cannot compare blocks");
		case box:     return compare_objects(*unbox_box(ob1), *unbox_box(ob2));
	}
}

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
	if (patt == obj) return 1;
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
		case box:     return match_objects(*unbox_box(patt), *unbox_box(obj));
		case block:
			push (obj);
			unbox_block(patt)();
			return unbox_boolean(pop());
	}
}

static void destructure_lists(LIST patt, LIST obj)
{
	if (!patt) return;
	destructure_lists(patt->next, obj->next);
	destructure_objects(patt->object, obj->object);
}

static void destructure_records(RECORD patt, RECORD obj)
{
	ssize_t i;
	for (i = 0; record_info[patt->id][i+1]; ++i);
	for (; i >= 0; --i) destructure_objects(patt->items[i], obj->items[i]);
}

static void destructure_objects(ANY patt, ANY obj)
{
	if (get_type(patt) == block)
	{
		push(obj);
		return;
	}
	switch (get_type(patt))
	{
		case list:   destructure_lists(unbox_list(patt), unbox_list(obj)); break;
		case record: destructure_records(unbox_record(patt), unbox_record(obj)); break;
		case box:    destructure_objects(*unbox_box(patt), *unbox_box(obj)); break;
		default:;
	}

}

__attribute__((hot))
static _Bool is_nan(ANY box)
{
	// Mostly works with -ffast-math
	return (box & NAN_MASK) == NAN_MASK;
}

static cognate_type get_type(ANY box)
{
	if (is_nan(box)) return (TYP_MASK & box) >> 48;
	else return number;
}

__attribute__((hot))
static NUMBER unbox_number(ANY box)
{
	if unlikely(is_nan(box))
		throw_error_fmt("Expected a number but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return *(NUMBER*)&box;
}

__attribute__((hot))
static ANY box_number(NUMBER num)
{
	return *(ANY*)&num;
}

__attribute__((hot))
static BOX unbox_box(ANY b)
{
	if unlikely(!is_nan(b) || (TYP_MASK & b) != (long)box << 48)
		throw_error_fmt("Expected a box but got %.64s which is a %s", show_object(b, 0), lookup_type(get_type(b)));
	return (BOX)(PTR_MASK & b);
}

__attribute__((hot))
static ANY box_box(BOX b)
{
	return NAN_MASK | ((long)box << 48) | (long)b;
}

__attribute__((hot))
static BOOLEAN unbox_boolean(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)boolean << 48)
		throw_error_fmt("Expected a boolean but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (BOOLEAN)(PTR_MASK & box);
}

__attribute__((hot))
static ANY box_boolean(BOOLEAN b)
{
	return NAN_MASK | ((long)boolean << 48) | b;
}

__attribute__((hot))
static STRING unbox_string(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)string << 48)
		throw_error_fmt("Expected a string but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (STRING)(PTR_MASK & box);
}

__attribute__((hot))
static ANY box_string(STRING s)
{
	return NAN_MASK | ((long)string << 48) | (long)s;
}

__attribute__((hot))
static LIST unbox_list(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)list << 48)
		throw_error_fmt("Expected a list but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (LIST)(PTR_MASK & box);
}

__attribute__((hot))
static ANY box_list(LIST s)
{
	return NAN_MASK | ((long)list << 48) | (long)s;
}

__attribute__((hot))
static RECORD unbox_record(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)record << 48)
		throw_error_fmt("Expected a record but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (RECORD)(PTR_MASK & box);
}

__attribute__((hot))
static ANY box_record(RECORD s)
{
	return NAN_MASK | ((long)record << 48) | (long)s;
}

__attribute__((hot))
static SYMBOL unbox_symbol(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)symbol << 48)
		throw_error_fmt("Expected a symbol but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (SYMBOL)(PTR_MASK & box);
}

__attribute__((hot))
static ANY box_symbol(SYMBOL s)
{
	return NAN_MASK | ((long)symbol << 48) | (long)s;
}

__attribute__((hot))
static BLOCK unbox_block(ANY box)
{
	if unlikely(!is_nan(box) || (TYP_MASK & box) != (long)block << 48)
		throw_error_fmt("Expected a block but got %.64s which is a %s", show_object(box, 0), lookup_type(get_type(box)));
	return (BLOCK)(PTR_MASK & box);
}

__attribute__((hot))
static ANY box_block(BLOCK s)
{
	return NAN_MASK | ((long)block << 48) | (long)Block_copy(s);
}

__attribute__((hot))
static void check_record_id(size_t i, RECORD r)
{
	if unlikely(i != r->id)
		throw_error_fmt("Expected a %.64s but got %.64s which is a %s", record_info[i][0], show_object(box_record(r), 0), record_info[r->id][0]);

}

#define PAGE_SIZE 4096

#define EMPTY 0x0
#define ALLOC 0x1
#define FORWARD 0x2


// Blocksruntime stuff, nothing to see here
/*
static void* blk_alloc(const unsigned long size, __attribute__((unused)) const _Bool _, __attribute__((unused)) const _Bool __) { return gc_malloc(size); }
static void blk_setHasRefcount(__attribute__((unused)) const void* _, __attribute__((unused)) const _Bool __) {}
static void blk_gc_assign_strong(void* src, void** dst) { *dst = src; }
static void blk_gc_assign_weak(const void* src, void* dst) { *(void**)dst = (void*)src; }
static void blk_gc_memmove(void* dst, void* src, unsigned long size) { memmove(dst, src, size); }

extern void _Block_use_GC(void *(*)(const unsigned long, const _Bool, const _Bool),
                          void (*)(const void *, const _Bool),
                          void (*)(void *, void **),
                          void (*)(const void *, void *),
                          void (*)(void *, void *, unsigned long));
*/

static void gc_init(void)
{
	// Tell blocksruntime to use the gc
	system_memory = sysconf(_SC_PHYS_PAGES) * 4096;
	bitmap[0] = mmap(0, system_memory/18, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE, -1, 0);
   bitmap[1] = mmap(0, system_memory/18, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE, -1, 0);
   space[0]  = mmap(0, (system_memory/18)*8, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE, -1, 0);
   space[1]  = mmap(0, (system_memory/18)*8, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE, -1, 0);
	bitmap[0][0] = ALLOC;
	bitmap[1][0] = ALLOC;
}

__attribute__((malloc, hot, assume_aligned(sizeof(uint64_t)), alloc_size(1), returns_nonnull))
static void* gc_malloc(size_t sz)
{
	static ptrdiff_t interval = 1024l*1024l*10;
	interval -= sz;
	if unlikely(interval < 0)
	{
		gc_collect();
		interval = 1024l*1024l*10l + alloc[z] * 6;
	}
	void* buf = space[z] + alloc[z];
	//assert(bitmap[z][alloc[z]] == ALLOC);
	alloc[z] += (sz + 7) / 8;
	//assert(!sz || bitmap[z][alloc[z]] == EMPTY);
	bitmap[z][alloc[z]] = ALLOC;
	//assert(!((ANY)buf & 7));
	return buf;
}

__attribute__((hot))
static _Bool is_gc_ptr(ANY object)
{
	const ANY upper_bits = object & ~PTR_MASK;
	if (upper_bits && !is_nan(object)) return 0;
	const ANY index = (ANY*)(object & PTR_MASK & ~7) - space[!z];
	if (index >= alloc[!z]) return 0;
	return 1;
}

__attribute__((hot))
static void gc_collect_root(ANY* restrict addr)
{
	if (!is_gc_ptr(*addr)) return;
	struct action {
		ANY from;
		ANY* restrict to;
	};
	struct action* restrict act_stk_start = (struct action*)space[!z] + alloc[!z];
	struct action* restrict act_stk_top = act_stk_start;
	*act_stk_top++ = (struct action) { .from=*addr, .to=addr };
	while (act_stk_top-- != act_stk_start)
	{
		ANY from = act_stk_top->from;
		ANY* to = act_stk_top->to;
		const ANY upper_bits = from & ~PTR_MASK;
		const ANY lower_bits = from & 7;
		ANY index = (ANY*)(from & PTR_MASK & ~7) - space[!z];
		ptrdiff_t offset = 0;
		while (bitmap[!z][index] == EMPTY) index--, offset++; // Ptr to middle of object
		if (bitmap[!z][index] == FORWARD)
			*to = lower_bits | upper_bits | (ANY)((ANY*)space[!z][index] + offset);
		else
		{
			//assert(bitmap[!z][index] == ALLOC);
			ANY* buf = space[z] + alloc[z]; // Buffer in newspace
			//assert(bitmap[z][alloc[z]] == ALLOC);
			size_t sz = 1;
			for (;bitmap[!z][index+sz] == EMPTY;sz++);
			alloc[z] += sz;
			//assert(bitmap[z][alloc[z]] == EMPTY);
			bitmap[z][alloc[z]] = ALLOC;
			for (size_t i = 0;i < sz;i++)
			{
				ANY from = space[!z][index+i];
				if (is_gc_ptr(from))
					*act_stk_top++ = (struct action) { .from=from, .to=buf+i };
				else buf[i] = from;
			}
			space[!z][index] = (ANY)buf; // Set forwarding address
			bitmap[!z][index] = FORWARD;
			*to = lower_bits | upper_bits | (ANY)(buf + offset);
		}
	}
}

static __attribute__((noinline,hot)) void gc_collect(void)
{
	/*
	clock_t start, end;
	double cpu_time_used;
	size_t heapsz = alloc[z];
	start = clock();
	*/
	z = !z;
	memset(bitmap[z], EMPTY, alloc[z]+1);
	alloc[z] = 0;
	bitmap[z][0] = ALLOC;

	flush_stack_cache();
	for (ANY* root = stack.absolute_start; root != stack.top; ++root)
		gc_collect_root(root);

	jmp_buf a;
	if (setjmp(a)) return;

	for (ANY* root = (ANY*)&a; root < (ANY*)function_stack_start; ++root)
		gc_collect_root(root); // Watch me destructively modify the call stack

	//end = clock();
	//printf("%lf seconds for %ziMB -> %ziMB\n", (double)(end - start) / CLOCKS_PER_SEC, heapsz * 8 /1024/1024, alloc[z] * 8 / 1024/1024);

	longjmp(a, 1);
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

static ANY VAR(if)(BOOLEAN cond, ANY a, ANY b)
{
	return cond ? a : b;
}

static void VAR(when)(BOOLEAN cond, BLOCK expr)
{
	if (cond) expr();
}

static void VAR(unless)(BOOLEAN cond, BLOCK expr)
{
	if (!cond) expr();
}

static void VAR(while)(BLOCK cond, BLOCK body)
{
	cond();
	while (unbox_boolean(pop()))
	{
		body();
		cond();
	}
}

static void VAR(until)(BLOCK cond, BLOCK body)
{
	cond();
	while (!unbox_boolean(pop()))
	{
		body();
		cond();
	}
}

static void VAR(do)(BLOCK blk) { blk(); }

static void VAR(put)(ANY a)   { assert_impure(); fputs(show_object(a, 1), stdout); fflush(stdout); }
static void VAR(print)(ANY a) { assert_impure(); puts(show_object(a, 1)); }

static void VAR(puts)(BLOCK b)   { assert_impure(); VAR(for)(VAR(list)(b), ^{VAR(put)(pop()); }); }
static void VAR(prints)(BLOCK b) { assert_impure(); VAR(for)(VAR(list)(b), ^{VAR(put)(pop()); }); putc('\n', stdout); }

static NUMBER VAR(P)(NUMBER a, NUMBER b) { return a + b; } // Add cannot produce NaN.

static NUMBER VAR(M)(NUMBER a, NUMBER b)
{
	const double r = a * b;
	if unlikely(is_nan(*(long*)&r))
		throw_error_fmt("Multiplication by %.14g of %.14g yields invalid result", a, b);
	return r;
}

static NUMBER VAR(D)(NUMBER a, NUMBER b)
{
	const double r = b - a;
	if unlikely(is_nan(*(long*)&r))
		throw_error_fmt("Subtraction of %.14g from %.14g yields invalid result", a, b);
	return r;
}

static NUMBER VAR(S)(NUMBER a, NUMBER b)
{
	const double r = b / a;
	if unlikely(is_nan(*(long*)&r))
		throw_error_fmt("Division by %.14g of %.14g yields invalid result", a, b);
	return r;
}

static NUMBER VAR(modulo)(NUMBER a, NUMBER b)
{
	const double r = b - a * floor(b / a);
	if unlikely(is_nan(*(long*)&r))
		throw_error_fmt("Modulo by %.14g of %.14g yields invalid result", a, b);
	return r;
}

static NUMBER VAR(sqrt)(NUMBER a)
{
	return sqrt(a);
}

static NUMBER VAR(random)(NUMBER low, NUMBER high)
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
	throw_error_fmt("Invalid range %.14g..%.14g", low, high);
}

static void VAR(clear)(void) { stack.cache = NIL_OBJ; stack.top=stack.start; }

static BOOLEAN VAR(true) =  1;
static BOOLEAN VAR(false) = 0;
static BOOLEAN VAR(either)(BOOLEAN a, BOOLEAN b) { return a || b; }
static BOOLEAN VAR(both)(BOOLEAN a, BOOLEAN b)   { return a && b; }
static BOOLEAN VAR(oneDof)(BOOLEAN a, BOOLEAN b) { return a ^ b;  }
static BOOLEAN VAR(not)(BOOLEAN a)               { return !a;     }
static BOOLEAN VAR(EE)(ANY a, ANY b)  { return compare_objects(a,b); }
static BOOLEAN VAR(SE)(ANY a, ANY b) { return !compare_objects(a,b); }
static BOOLEAN VAR(G)(NUMBER a, NUMBER b)  { return a < b; }
static BOOLEAN VAR(L)(NUMBER a, NUMBER b)  { return a > b; }
static BOOLEAN VAR(GE)(NUMBER a, NUMBER b) { return a <= b; }
static BOOLEAN VAR(LE)(NUMBER a, NUMBER b) { return a >= b; }
static BOOLEAN VAR(numberQ)(ANY a)  { return get_type(a)==number; }
static BOOLEAN VAR(listQ)(ANY a)    { return get_type(a)==list;   }
static BOOLEAN VAR(stringQ)(ANY a)  { return get_type(a)==string; }
static BOOLEAN VAR(anyQ)(ANY a)     { (void)a; return 1; }
static BOOLEAN VAR(blockQ)(ANY a)   { return get_type(a)==block;  }
static BOOLEAN VAR(booleanQ)(ANY a) { return get_type(a)==boolean;}
static BOOLEAN VAR(symbolQ)(ANY a)  { return get_type(a)==symbol; }
static BOOLEAN VAR(integerQ)(ANY a) { return VAR(numberQ)(a) && unbox_number(a) == floor(unbox_number(a)); }
static BOOLEAN VAR(zeroQ)(ANY a)    { return VAR(numberQ)(a) && unbox_number(a) == 0; }
static BOOLEAN VAR(match)(ANY patt, ANY obj) { return match_objects(patt,obj); }

static BLOCK VAR(case)(ANY patt, ANY if_match, ANY if_not_match)
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

static ANY VAR(first)(LIST lst)
{
	// Returns the first element of a list. O(1).
	if unlikely(!lst) throw_error("empty list is invalid");
	return lst->object;
}

static LIST VAR(rest)(LIST lst)
{
	// Returns the tail portion of a list. O(1).
	if unlikely(!lst) throw_error("empty list is invalid");
	return lst->next;
}

static STRING VAR(head)(STRING str)
{
	if unlikely(!*str) throw_error("empty string is invalid");
	return gc_strndup((char*)str, mblen(str, MB_CUR_MAX));
}

static STRING VAR(tail)(STRING str)
{
	if unlikely(!*str) throw_error("empty string is invalid");
	return str + mblen(str, MB_CUR_MAX);
}

static LIST VAR(push)(ANY a, LIST b)
{
	// Pushes an object from the stack onto the list's first element. O(1).
	// TODO: Better name? Inconsistent with List where pushing to the stack adds to the END.
	cognate_list* lst = gc_new (cognate_list);
	*lst = (cognate_list) {.object = a, .next = b};
	return lst;
}

static BOOLEAN VAR(emptyQ)(LIST lst)
{
	// Returns true is a list or string is empty. O(1).
	// Can be used to to write a Length function.
	return !lst;
}

static LIST VAR(list)(BLOCK expr)
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

static STRING VAR(join)(NUMBER n)
{
	// Joins a string to the end of another string.
	// Define Prefix (Swap, Suffix);
	size_t n1 = n;
	if (n != n1) throw_error_fmt("Cannot join %.14g strings", n);
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

static NUMBER VAR(stringDlength)(STRING str)
{
	size_t len = 0;
	for (; *str ; str += mblen(str, MB_CUR_MAX), ++len);
	return len;
}

static STRING VAR(substring)(NUMBER startf, NUMBER endf, STRING str)
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
}


static STRING VAR(input)(void)
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

static STRING VAR(read)(STRING filename)
{
	assert_impure();
	// Read a file to a string.
	FILE *fp = fopen(filename, "ro");
	if unlikely(fp == NULL) throw_error_fmt("Cannot open file '%s'", filename);
	struct stat st;
	fstat(fileno(fp), &st);
	char* const text = gc_malloc (st.st_size + 1);
	if (fread(text, sizeof(char), st.st_size, fp) != (unsigned long)st.st_size)
		throw_error_fmt("Error reading file '%s'", filename);
	fclose(fp);
	text[st.st_size] = '\0'; // Remove trailing eof.
	return text;
	// TODO: single line (or delimited) file read function for better IO performance
}

static NUMBER VAR(number)(STRING str)
{
	// casts string to number.
	char* end;
	NUMBER num = strtod(str, &end);
	if (end == str || *end != '\0') goto cannot_parse;
	if unlikely(is_nan(*(long*)&num))
		goto cannot_parse;
	return num;
cannot_parse:
	throw_error_fmt("Cannot parse '%.32s' to a number", str);
}

static STRING VAR(path)(void)
{
	assert_impure();
	char buf[FILENAME_MAX];
	if (!getcwd(buf, FILENAME_MAX))
		throw_error("cannot get working directory");
	char* ret = gc_strdup(buf);
	return ret;
}

static LIST VAR(stack)(void)
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

static void VAR(write)(STRING filename, ANY obj)
{
	assert_impure();
	// Write object to end of file, without a newline.
	FILE* const fp = fopen(filename, "a");
	if unlikely(fp == NULL) throw_error_fmt("Cannot open file '%s'", filename);
	fputs(show_object(obj, 1), fp);
	fclose(fp);
}

static LIST VAR(parameters)(void)
{
	return cmdline_parameters; // TODO should be a variable, and allow mutation and stuff
}

static void VAR(stop)(void)
{
	assert_impure();
	// Don't check stack length, because it probably wont be empty.
	exit(EXIT_SUCCESS);
}

static BOOLEAN VAR(matchDregex)(STRING reg_str, STRING str)
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
			throw_error_fmt("Compile error (%s) in regex '%.32s'", reg_err, reg_str);
		}
		old_str = reg_str;
		// This should probably be strcpy, but I trust that reg_str is either
		// allocated with the garbage collector, or read only in the data segment.
	}
	const int found = regexec(&reg, str, 0, NULL, 0);
	if unlikely(found != 0 && found != REG_NOMATCH)
	{

		throw_error_fmt("Match error with regex '%.32s' on string '%.32s'", str, reg_str);
		// If this error ever actually appears, use regerror to get the full text.
	}
	return !found;
}

static NUMBER VAR(ordinal)(STRING str)
{
	if unlikely(!str[0] || strlen(str) > (size_t)mblen(str, MB_CUR_MAX))
		throw_error_fmt("Invalid string '%.32s' (should be length 1)", str);
	wchar_t chr = 0;
	mbtowc(&chr, str, MB_CUR_MAX);
	return chr;
}

static STRING VAR(character)(NUMBER d)
{
	const wchar_t i = d;
	char* const str = gc_malloc (MB_CUR_MAX + 1);
	if unlikely(i != d || wctomb(str, i) == -1)
		throw_error_fmt("Cannot convert %.14g to UTF8 character", d);
	str[mblen(str, MB_CUR_MAX)] = '\0';
	return str;
}

static NUMBER VAR(floor)(NUMBER a)
{
	return floor(a);
}

static NUMBER VAR(round)(NUMBER a)
{
	return round(a);
}

static NUMBER VAR(ceiling)(NUMBER a)
{
	return ceil(a);
}

static void VAR(assert)(STRING name, BOOLEAN result)
{
	if unlikely(!result)
		throw_error_fmt("Failed assertion '%s'", name);
}

static void VAR(error)(STRING str)
{
	throw_error(str);
}

static LIST VAR(map)(BLOCK blk, LIST lst)
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

static LIST VAR(filter)(BLOCK blk, LIST lst)
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

static void VAR(for)(LIST lst, BLOCK blk)
{
	for (; lst ; lst = lst->next)
	{
		push(lst->object);
		blk();
	}
}

static LIST VAR(range)(NUMBER start, NUMBER end)
{
	if (end < start)
		throw_error_fmt("Invalid range %.14g..%.14g", start, end);
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

static ANY VAR(index)(NUMBER ind, LIST lst)
{
	size_t i = ind;
	if unlikely(i != ind) throw_error_fmt("Cannot get index %.14g", ind);
	for (;lst;lst=lst->next)
	{
		if (!i--) return lst->object;
	}
	throw_error_fmt("Index %zi is outside of array", (size_t)ind);
}

static void VAR(wait)(NUMBER seconds)
{
	assert_impure();
	sleep(seconds);
}

static BLOCK VAR(precompute)(BLOCK blk)
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

static STRING VAR(show)(ANY o)
{
	return show_object(o, 0);
}

static LIST VAR(split)(STRING sep, STRING str)
{
	if (!*sep) throw_error("Empty separator");
	LIST lst = NULL;
	str = gc_strdup((char*)str);
	char* r = (char*)str;
	while ((str = strtok_r(NULL, sep, &r)))
	{
		cognate_list* node = gc_new(cognate_list);
		node->object = box_string(str);
		node->next = lst;
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

static NUMBER VAR(length)(LIST lst) {
	size_t len = 0;
	for (; lst; lst = lst->next)
		len++;
	return len;
}


static LIST VAR(take)(NUMBER n, LIST l) {
	if unlikely(n != (unsigned long)n) throw_error_fmt("Cannot take %.14g elements", n);
	LIST r = NULL;
	while (n --> 0)
	{
		if unlikely(!l) throw_error("List too small");
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

static LIST VAR(discard)(NUMBER n, LIST l) {
	if unlikely(n != (unsigned long)n) throw_error_fmt("Cannot discard %.14g elements", n);
	for (;n-->0;l=l->next)
		if unlikely(!l) throw_error("List too small");
	return l;
}

static BLOCK VAR(remember)(BLOCK b)
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

static BLOCK VAR(pure)(BLOCK b)
{
	return Block_copy(^{
		pure = 1;
		b();
		pure = 0;
	});
}

static LIST VAR(takeDwhile)(BLOCK predicate, LIST lst)
{
	LIST r = NULL;
	while (lst)
	{
		push (lst->object);
		predicate();
		int res = unbox_boolean(pop());
		if (!res) break;
		cognate_list* a = gc_new(cognate_list);
		a->object = lst->object;
		a->next = r;
		r = a;
		lst = lst->next;
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

static BOOLEAN VAR(all)(BLOCK predicate, LIST lst)
{
	for (; lst ; lst = lst->next)
	{
		push(lst->object);
		predicate();
		if unlikely(!unbox_boolean(pop())) return 0;
	}
	return 1;
}

static LIST VAR(append)(ANY a, LIST l)
{
	// TODO iterative version.
	cognate_list* ll = gc_malloc(sizeof(*l));
	if (!l)
	{
		ll->next = NULL;
		ll->object = a;
		return ll;
	}
	ll->object = l->object;
	ll->next = VAR(append)(a, l->next);
	return ll;
}

static BOX VAR(box)(ANY a) // boxes seem to break the GC sometimes TODO
{
	ANY* b = gc_malloc(sizeof *b);
	*b = a;
	return b;
}

static ANY VAR(unbox)(BOX b)
{
	return *b;
}

static void VAR(set)(BOX b, ANY a)
{
	*b = a;
}

static void VAR(debug)()
{
#ifdef DEBUG
	debug = 1;
	debugger_step();
#endif
}
// ---------- ACTUAL PROGRAM ----------

