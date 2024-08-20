// ---------- RUNTIME HEADER ----------
#define _GNU_SOURCE
#define _FORTIFY_SOURCE 2

#include <stddef.h>
#include <wchar.h>
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
#include <regex.h>

#define WORDSZ (sizeof(void*))

#define INITIAL_READ_SIZE 64
#define STACK_MARGIN_KB		50

#define CHECK_DEFINED(str, thing) if (!thing->defined) throw_error(#str" called before definition")

#define MEM_PROT PROT_READ|PROT_WRITE
#define MEM_FLAGS MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE

typedef struct cognate_object ANY;
typedef ANY* restrict ANYPTR;
typedef ANY* restrict BOX;
typedef struct cognate_block BLOCK;
typedef _Bool BOOLEAN;
typedef double NUMBER;
typedef const char* restrict STRING;
typedef const struct cognate_list* restrict LIST;
typedef const char* restrict SYMBOL;
typedef struct cognate_file* IO;
typedef const struct cognate_dict* restrict DICT;

typedef struct cognate_block
{
	void (*fn)(uint8_t*);
	uint8_t* env;
} cognate_block;

typedef enum cognate_type
{
	NIL = 0,
	box,
	boolean,
	list,
	block,
	symbol,
	number,
	string,
	io,
	dict,
} cognate_type;

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
		DICT dict;
	};
	cognate_type type;
} cognate_object;

#define MKEARLY(T) typedef struct _early_##T { T value ; _Bool defined ; } early_##T

MKEARLY(ANY);
MKEARLY(BOX);
MKEARLY(BLOCK);
MKEARLY(BOOLEAN);
MKEARLY(NUMBER);
MKEARLY(STRING);
MKEARLY(LIST);
MKEARLY(SYMBOL);

typedef struct cognate_dict
{
	STRING key;
	ANY value;
	DICT child1;
	DICT child2;
} cognate_dict;

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
	const char* file;
	const char* line_str;
} backtrace;

typedef struct var_info
{
	const struct var_info* restrict next;
	const char* name;
	const ANY value;
} var_info;

#endif

#define PTR_MASK 0x0000ffffffffffff


#define ___name ___##name
#define SYM(name) ____##name
#define CALL(name, args) ___name args

#define SET(name, val) \
	if unlikely(pure) throw_error("Cannot mutate variable in pure function"); \
	___name = val;

#define unlikely(expr) (__builtin_expect((_Bool)(expr), 0))
#define likely(expr)	 (__builtin_expect((_Bool)(expr), 1))

static uintptr_t* space[2] = {NULL,NULL};
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

_Bool breakpoints[10];

extern int main(int, char**);

static const char* restrict function_stack_start;

// Variables and	needed by functions.c defined in runtime.c
static void init_stack(void);
static STRING show_object(const ANY object, const _Bool, char*);
static void _Noreturn __attribute__((format(printf, 1, 2))) throw_error_fmt(const char* restrict const, ...);
static void _Noreturn throw_error(const char* restrict const);
static _Bool compare_objects(ANY, ANY);
static _Bool match_objects(ANY, ANY);
static void destructure_lists(LIST, LIST);
static void destructure_objects(ANY, ANY);
#ifdef DEBUG
static void print_backtrace(int, const backtrace*, int);
#endif

static void* gc_malloc(size_t);
static void gc_collect(void);
static void gc_init(void);
static char* gc_strdup(char*);
static char* gc_strndup(char*, size_t);

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
static DICT unbox_DICT(ANY);
static ANY box_DICT(DICT);

static NUMBER radians_to_degrees(NUMBER);
static NUMBER degrees_to_radians(NUMBER);

static void cleanup(void);
static void push(ANY);
static ANY pop(void);
static ANY peek(void);
static void flush_stack_cache(void);
static int stack_length(void);

// Builtin functions needed by compiled source file defined in functions.c
static DICT ___insert(STRING, ANY, DICT);
static LIST ___empty(void);
static ANY ___if(BOOLEAN, ANY, ANY);
static void ___put(ANY);
static void ___print(ANY);
static NUMBER ___P(NUMBER, NUMBER);
static NUMBER ___M(NUMBER, NUMBER);
static NUMBER ___D(NUMBER, NUMBER);
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
static BOOLEAN ___XE(ANY, ANY);
static BOOLEAN ___L(NUMBER, NUMBER);
static BOOLEAN ___G(NUMBER, NUMBER);
static BOOLEAN ___LE(NUMBER, NUMBER);
static BOOLEAN ___GE(NUMBER, NUMBER);
static BOOLEAN ___match(ANY, ANY);
static BOOLEAN ___numberQ(ANY);
static BOOLEAN ___symbolQ(ANY);
static BOOLEAN ___listQ(ANY);
static BOOLEAN ___stringQ(ANY);
static BOOLEAN ___blockQ(ANY);
static BOOLEAN ___booleanQ(ANY);
static BOOLEAN ___integerQ(ANY);
static BOOLEAN ___ioQ(ANY);
static BOOLEAN ___zeroQ(ANY);
static ANY ___first(LIST);
static LIST ___rest(LIST);
static STRING ___head(STRING);
static STRING ___tail(STRING);
static LIST ___push(ANY, LIST);
static BOOLEAN ___emptyQ(LIST);
static LIST ___list(BLOCK);
static STRING ___join(STRING, STRING);
static NUMBER ___stringDlength(STRING);
static STRING ___substring(NUMBER, NUMBER, STRING);
static STRING ___input(void);
static IO ___open(STRING, STRING);
static void ___close(IO);
static NUMBER ___number(STRING);
static STRING ___path(void);
static LIST ___stack(void);
static LIST ___parameters(void);
static void ___stop(void);
static STRING ___show(ANY);
static BLOCK ___regex(STRING);
static BLOCK ___regexDmatch(STRING);
static NUMBER ___ordinal(STRING);
static STRING ___character(NUMBER);
static NUMBER ___floor(NUMBER);
static NUMBER ___round(NUMBER);
static NUMBER ___ceiling(NUMBER);
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
static _Bool compare_lists(LIST, LIST);
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

static void fn0();

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
	while (argc --> 1)
	{
		cognate_list* const tmp = gc_malloc (sizeof *tmp);
		tmp->object = box_STRING(argv[argc]);
		tmp->next = cmdline_parameters;
		cmdline_parameters = tmp;
	}
	// Bind error signals.
	char signals[] = { SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGABRT, SIGBUS, SIGFPE, SIGPIPE, SIGTERM, SIGCHLD };
	for (size_t i = 0; i < sizeof(signals); ++i) signal(signals[i], handle_error_signal);
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
	if unlikely(stack.top != stack.start || stack.cache.type)
		throw_error_fmt("Exiting with %ti object(s) on the stack", stack.top - stack.start + (stack.cache.type != 0));
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
			flush_stack_cache();
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
			for (const var_info* restrict v = vars; v; v = v->next)
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
	if (!b || !n) return;
	int len = strlen(b->name);
	const char* ln = b->line_str;
	ssize_t col = b->col;
	while (*ln)
	{
		if (*ln != ' ' && *ln != '\t') break;
		ln++;
		col--;
	}
	char* tabs;
	for  (char* tabs ; *tabs ; tabs++) if (*tabs == '\t') *tabs = ' ';
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

static _Noreturn __attribute__((format(printf, 1, 2))) void throw_error_fmt(const char* restrict const fmt, ...)
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

static _Noreturn void throw_error(const char* restrict const msg)
{
	throw_error_fmt("%s", msg);
}

static void handle_error_signal(int sig)
{
	throw_error_fmt("Recieved signal %i (%s)", sig, strsignal(sig));
}

static void assert_impure(void)
{
	if unlikely(pure) throw_error("Invalid operation for pure function");
}

static char* show_dict(DICT d, char* buffer)
{
	if (!d) return buffer;

	buffer = show_dict(d->child1, buffer);
	buffer += strlen(buffer);

	buffer = (char*)show_object(box_STRING(d->key), 0, buffer);
	*buffer++ = ':';
	buffer = (char*)show_object(d->value, 0, buffer);
	*buffer++ = ' ';

	buffer = show_dict(d->child2, buffer);
	buffer += strlen(buffer);

	return buffer;
}


static STRING show_object (const ANY object, const _Bool raw_strings, char* buffer)
{
	static char* buf;
	_Bool root = !buffer;
	if (root) buf = buffer = (char*)(space[z] + alloc[z]); // i dont like resizing buffers
	switch (object.type)
	{
		case NIL: throw_error("This shouldn't happen");
					 break;
		case dict: sprintf(buffer, "{ "); buffer + strlen(buffer);
					  buffer = show_dict(object.dict, buffer);
					  sprintf(buffer, "}"); buffer += strlen(buffer);
					  break;
		case number: sprintf(buffer, "%.14g", object.number);
						 buffer += strlen(buffer);
						 break;
		case io:
						 if (object.io->file != NULL)
						 	sprintf(buffer, "{ %s OPEN mode '%s' }", object.io->path, object.io->mode);
						 else
						 	sprintf(buffer, "{ %s CLOSED }", object.io->path);
						 buffer += strlen(buffer);
						 break;
		case string:
			if (raw_strings)
				buffer += strlen(strcpy(buffer, unbox_STRING(object)));
			else
			{
				*buffer++ = '"';
				for (const char* str = unbox_STRING(object) ; *str ; ++str)
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
			}
			break;
		case list:
			*buffer++ = '(';
			for (LIST l = unbox_LIST(object) ; l ; l = l->next)
			{
				buffer = (char*)show_object(l->object, 0, buffer);
				if (!l->next) break;
				*buffer++ = ',';
				*buffer++ = ' ';
			}
			*buffer++ = ')';
			break;
		case boolean: strcpy(buffer, unbox_BOOLEAN(object) ? "True" : "False");
						  buffer += strlen(buffer);
						  break;
		case symbol:  strcpy(buffer, unbox_SYMBOL(object));
						  buffer += strlen(buffer);
						  break;
		case block:
		{
			void (*fn)(uint8_t*) = unbox_BLOCK(object).fn;
			sprintf(buffer, "<block %p>", *(void**)&fn);
			buffer += strlen(buffer);
			break;
		}
		case box:
			*buffer++ = '[';
			buffer = (char*)show_object(*unbox_BOX(object), 0, buffer);
			*buffer++ = ']';
			break;
	}
	if (!root) return buffer;
	*buffer++ = '\0';
	char* c = gc_strdup(buf);
	return c;
}

static void init_stack(void)
{
	stack.absolute_start = stack.top = stack.start
		= mmap(0, system_memory/10, MEM_PROT, MEM_FLAGS, -1, 0);
	stack.cache.type = 0;
}

__attribute__((hot))
static void push(ANY object)
{
	if likely(stack.cache.type == NIL) { stack.cache = object; return; }
	*stack.top++ = stack.cache;
	stack.cache = object;
}

__attribute__((hot))
static ANY pop(void)
{
	if likely(stack.cache.type != NIL) { const ANY a = stack.cache; stack.cache.type = NIL; return a; }
	if unlikely(stack.top == stack.start) throw_error("Stack underflow");
	return *--stack.top;
}

__attribute__((hot))
static ANY peek(void)
{
	if likely(stack.cache.type != NIL) return stack.cache;
	if unlikely(stack.top == stack.start) throw_error("Stack underflow");
	return *(stack.top - 1);
}

static void flush_stack_cache(void)
{
	if (stack.cache.type == NIL) return;
	push(stack.cache);
	pop();
}

static int stack_length(void)
{
	return stack.top - stack.start + (stack.cache.type != NIL);
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
		case symbol:  return "symbol";
		case boolean: return "boolean";
		default:      return NULL;
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


static _Bool compare_objects(ANY ob1, ANY ob2)
{
	if (ob1.type != ob2.type) return 0;
	switch (ob1.type)
	{
		case number:
			return fabs(unbox_NUMBER(ob1) - unbox_NUMBER(ob2))
				<= 0.5e-14 * fabs(unbox_NUMBER(ob1));
		case boolean: return unbox_BOOLEAN(ob1) == unbox_BOOLEAN(ob2);
		case string:  return !strcmp(unbox_STRING(ob1), unbox_STRING(ob2));
		case symbol:  return unbox_SYMBOL(ob1) == unbox_SYMBOL(ob2);
		case list:    return compare_lists(unbox_LIST(ob1), unbox_LIST(ob2));
		case block:   throw_error("Cannot compare blocks");
		case box:     return compare_objects(*unbox_BOX(ob1), *unbox_BOX(ob2));
		default:      return 0; // really shouldn't happen
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

static void call_block(BLOCK b)
{
	b.fn(b.env);
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

static _Noreturn void type_error(char* expected, ANY got)
{
	char* s = "a";
	switch (expected[0])
		case 'a': case 'e': case 'i': case 'o': case 'u': case 'h':
			s = "an";
	throw_error_fmt("Expected %s %s but got %.64s", s, expected, show_object(got, 0, NULL));
}

__attribute__((hot))
static NUMBER unbox_NUMBER(ANY box)
{
	if likely (box.type == number) return box.number;
	type_error("number", box);
}

__attribute__((hot))
static ANY box_NUMBER(NUMBER num)
{
	return (ANY) {.type = number, .number = num};
}

__attribute__((hot))
static BOX unbox_BOX(ANY b)
{
	if likely (b.type == box) return b.box;
	type_error("box", b);
}

__attribute__((hot))
static ANY box_BOX(BOX b)
{
	return (ANY) {.type = box, .box = b};
}

__attribute__((hot))
static BOOLEAN unbox_BOOLEAN(ANY box)
{
	if likely (box.type == boolean) return box.boolean;
	type_error("boolean", box);
}

__attribute__((hot))
static ANY box_BOOLEAN(BOOLEAN b)
{
	return (ANY) {.type = boolean, .boolean = b};
}

__attribute__((hot))
static STRING unbox_STRING(ANY box)
{
	if likely (box.type == string) return box.string;
	type_error("string", box);
}

__attribute__((hot))
static ANY box_STRING(STRING s)
{
	return (ANY) {.type = string, .string = s};
}

__attribute__((hot))
static LIST unbox_LIST(ANY box)
{
	if likely (box.type == list) return box.list;
	type_error("list", box);
}

__attribute__((hot))
static ANY box_LIST(LIST s)
{
	return (ANY) {.type = list, .list = s};
}

__attribute__((hot))
static SYMBOL unbox_SYMBOL(ANY box)
{
	if likely (box.type == symbol) return box.symbol;
	type_error("list", box);
}

__attribute__((hot))
static ANY box_SYMBOL(SYMBOL s)
{
	return (ANY) {.type = symbol, .symbol = s};
}

__attribute__((hot))
static BLOCK unbox_BLOCK(ANY box)
{
	if likely (box.type == block) return box.block;
	type_error("block", box);
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
static ANY box_BLOCK(BLOCK s)
{
	return (ANY) {.type = block, .block = s};
}

__attribute__((hot))
static ANY box_IO(IO i)
{
	return (ANY) {.type = io, .io = i};
}


__attribute__((hot))
static IO unbox_IO(ANY box)
{
	if likely (box.type == io) return box.io;
	type_error("io", box);
}

__attribute__((hot))
static ANY box_DICT(DICT d)
{
	return (ANY) {.type = dict, .dict = d};
}

__attribute__((hot))
static DICT unbox_DICT(ANY box)
{
	if likely (box.type == dict) return box.dict;
	type_error("dict", box);
}

#define PAGE_SIZE 4096

#define EMPTY     0x0
#define ALLOC     0x1
#define FLATALLOC 0x2
#define FORWARD   0x3

static void gc_init(void)
{
	system_memory = sysconf(_SC_PHYS_PAGES) * 4096;
	bitmap[0] = mmap(0, system_memory/18, MEM_PROT, MEM_FLAGS, -1, 0);
	bitmap[1] = mmap(0, system_memory/18, MEM_PROT, MEM_FLAGS, -1, 0);
	space[0]  = mmap(0, (system_memory/18)*8, MEM_PROT, MEM_FLAGS, -1, 0);
	space[1]  = mmap(0, (system_memory/18)*8, MEM_PROT, MEM_FLAGS, -1, 0);
	bitmap[0][0] = ALLOC;
	bitmap[1][0] = ALLOC;
}

__attribute__((hot))
static _Bool is_heap_ptr(void* ptr)
{
	const uint64_t index = (uintptr_t*)ptr - space[!z];
	if (index >= alloc[!z]) return 0;
	return 1;
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

__attribute__((malloc, hot, assume_aligned(sizeof(uint64_t)), alloc_size(1), returns_nonnull))
static void* gc_flatmalloc(size_t sz)
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
	bitmap[z][alloc[z]] = FLATALLOC;
	//assert(!((ANY)buf & 7));
	return buf;
}

__attribute__((hot))
static _Bool is_gc_ptr(uintptr_t object)
{
	const uintptr_t upper_bits = object & ~PTR_MASK;
	if (upper_bits) return 0;
	const uintptr_t index = (uintptr_t*)(object & PTR_MASK & ~7) - space[!z];
	if (index >= alloc[!z]) return 0;
	return 1;
}

__attribute__((hot))
static void gc_collect_root(uintptr_t* restrict addr)
{
	if (!is_gc_ptr(*addr)) return;
	struct action {
		uintptr_t from;
		uintptr_t* restrict to;
	};
	struct action* restrict act_stk_start = (struct action*)space[!z] + alloc[!z];
	struct action* restrict act_stk_top = act_stk_start;
	*act_stk_top++ = (struct action) { .from=*addr, .to=addr };
	while (act_stk_top-- != act_stk_start)
	{
		uintptr_t from = act_stk_top->from;
		uintptr_t* to = act_stk_top->to;
		const uintptr_t upper_bits = from & ~PTR_MASK;
		const uintptr_t lower_bits = from & 7;
		uintptr_t index = (uintptr_t*)(from & PTR_MASK & ~7) - space[!z];
		ptrdiff_t offset = 0;
		while (bitmap[!z][index] == EMPTY) index--, offset++; // Ptr to middle of object
		if (bitmap[!z][index] == FORWARD)
			*to = lower_bits | upper_bits | (uintptr_t)((uintptr_t*)space[!z][index] + offset);
		else
		{
			_Bool flat = bitmap[!z][index] == FLATALLOC;
			//assert(bitmap[!z][index] == ALLOC);
			uintptr_t* buf = space[z] + alloc[z]; // Buffer in newspace
			//assert(bitmap[z][alloc[z]] == ALLOC);
			size_t sz = 1;
			for (;bitmap[!z][index+sz] == EMPTY;sz++);
			alloc[z] += sz;
			//assert(bitmap[z][alloc[z]] == EMPTY);
			bitmap[z][alloc[z]] = ALLOC;
			for (size_t i = 0;i < sz;i++)
			{
				uintptr_t from = space[!z][index+i];
				if (!flat && is_gc_ptr(from))
					*act_stk_top++ = (struct action) { .from=from, .to=buf+i };
				else buf[i] = from;
			}
			space[!z][index] = (uintptr_t)buf; // Set forwarding address
			bitmap[!z][index] = FORWARD;
			*to = lower_bits | upper_bits | (uintptr_t)(buf + offset);
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
	for (uintptr_t* root = (uintptr_t*)stack.absolute_start; root != (uintptr_t*)stack.top; ++root)
		gc_collect_root(root);

	jmp_buf a;
	if (setjmp(a)) return;

	for (uintptr_t* root = (uintptr_t*)&a; root < (uintptr_t*)function_stack_start; ++root)
		gc_collect_root(root); // Watch me destructively modify the call stack

	/*
	end = clock();
	printf("%lf seconds for %ziMB -> %ziMB\n", (double)(end - start) / CLOCKS_PER_SEC, heapsz * 8 /1024/1024, alloc[z] * 8 / 1024/1024);
	*/

	longjmp(a, 1);
}

static char* gc_strdup(char* src)
{
	const size_t len = strlen(src);
	return memcpy(gc_flatmalloc(len + 1), src, len + 1);
}

static char* gc_strndup(char* src, size_t bytes)
{
	const size_t len = strlen(src);
	if (len < bytes) bytes = len;
	char* dest = gc_flatmalloc(bytes + 1);
	dest[bytes] = '\0';
	return memcpy(dest, src, bytes);
}

static ANY ___if(BOOLEAN cond, ANY a, ANY b)
{
	return cond ? a : b;
}

static void ___put(ANY a)   { assert_impure(); fputs(show_object(a, 1, NULL), stdout); fflush(stdout); }
static void ___print(ANY a) { assert_impure(); puts(show_object(a, 1, NULL)); }

static NUMBER ___P(NUMBER a, NUMBER b) { return a + b; } // Add cannot produce NaN.
static NUMBER ___M(NUMBER a, NUMBER b) { return a * b; }
static NUMBER ___D(NUMBER a, NUMBER b) { return b - a; }
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
}

static void ___clear(void) { stack.cache.type = NIL; stack.top=stack.start; }

static BOOLEAN ___true(void)  { return 1; }
static BOOLEAN ___false(void) { return 0; }
static BOOLEAN ___or(BOOLEAN a, BOOLEAN b) { return a || b; }
static BOOLEAN ___and(BOOLEAN a, BOOLEAN b)   { return a && b; }
static BOOLEAN ___xor(BOOLEAN a, BOOLEAN b) { return a ^ b;  }
static BOOLEAN ___not(BOOLEAN a)               { return !a;     }
static BOOLEAN ___EE(ANY a, ANY b)  { return compare_objects(a,b); }
static BOOLEAN ___XE(ANY a, ANY b) { return !compare_objects(a,b); }
static BOOLEAN ___G(NUMBER a, NUMBER b)  { return a < b; }
static BOOLEAN ___L(NUMBER a, NUMBER b)  { return a > b; }
static BOOLEAN ___GE(NUMBER a, NUMBER b) { return a <= b; }
static BOOLEAN ___LE(NUMBER a, NUMBER b) { return a >= b; }
static BOOLEAN ___numberQ(ANY a)  { return a.type==number; }
static BOOLEAN ___listQ(ANY a)    { return a.type==list;   }
static BOOLEAN ___stringQ(ANY a)  { return a.type==string; }
static BOOLEAN ___blockQ(ANY a)   { return a.type==block;  }
static BOOLEAN ___booleanQ(ANY a) { return a.type==boolean;}
static BOOLEAN ___symbolQ(ANY a)  { return a.type==symbol; }
static BOOLEAN ___ioQ(ANY a)      { return a.type==io; }
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

static NUMBER ___integerX(NUMBER a)
{
	if unlikely(a != (long)a) type_error("integer", box_NUMBER(a));
	return a;
}

static BOOLEAN ___zeroX(NUMBER a)
{
	if unlikely(a != 0.0) type_error("zero", box_NUMBER(a));
	return a;
}

static BOOLEAN ___match(ANY patt, ANY obj) { return match_objects(patt,obj); }

static ANY ___first(LIST lst)
{
	// Returns the first element of a list. O(1).
	if unlikely(!lst) throw_error("empty list is invalid");
	return lst->object;
}

static LIST ___rest(LIST lst)
{
	// Returns the tail portion of a list. O(1).
	if unlikely(!lst) throw_error("empty list is invalid");
	return lst->next;
}

static STRING ___head(STRING str)
{
	if unlikely(!*str) throw_error("empty string is invalid");
	return gc_strndup((char*)str, mblen(str, MB_CUR_MAX));
}

static STRING ___tail(STRING str)
{
	if unlikely(!*str) throw_error("empty string is invalid");
	return str + mblen(str, MB_CUR_MAX);
}

static LIST ___push(ANY a, LIST b)
{
	// Pushes an object from the stack onto the list's first element. O(1).
	// TODO: Better name? Inconsistent with List where pushing to the stack adds to the END.
	cognate_list* lst = gc_malloc (sizeof *lst);
	*lst = (cognate_list) {.object = a, .next = b};
	return lst;
}

static BOOLEAN ___emptyQ(LIST lst)
{
	// Returns true is a list or string is empty. O(1).
	// Can be used to to write a Length function.
	return !lst;
}

static LIST ___list(BLOCK expr)
{
	flush_stack_cache();
	ANYPTR tmp_stack_start = stack.start;
	stack.start = stack.top;
	// Eval expr
	call_block(expr);
	// Move to a list.
	cognate_list* lst = NULL;
	flush_stack_cache();
	size_t len = stack_length();
	for (size_t i = 0; i < len; ++i)
	{
		cognate_list* l = gc_malloc(sizeof *l);
		l->object = stack.start[i];
		l->next = lst;
		lst = l;
	}
	stack.top = stack.start;
	stack.start = tmp_stack_start;
	return lst;
}

static STRING ___join(STRING s1, STRING s2)
{
	size_t l1 = strlen(s1);
	size_t l2 = strlen(s2);
	char* result = gc_malloc(l1 + l2 + 1);
	strcpy(result, s1);
	strcpy(result+l1, s2);
 	return result;
}

static NUMBER ___stringDlength(STRING str)
{
	size_t len = 0;
	for (; *str ; str += mblen(str, MB_CUR_MAX), ++len);
	return len;
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
	flush_stack_cache();
	for (size_t i = 0; i + stack.start < stack.top; ++i)
	{
		cognate_list* tmp = gc_malloc (sizeof *tmp);
		tmp -> object = stack.start[i];
		tmp -> next = lst;
		lst = tmp;
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

void apply_regex(uint8_t* env)
{
	regex_t reg = *(regex_t*)env;
	STRING str = unbox_STRING(pop());
	const int found = regexec(&reg, str, 0, NULL, 0);
	if unlikely(found != 0 && found != REG_NOMATCH)
		throw_error_fmt("Regex failed matching string '%.32s'", str);
	push(box_BOOLEAN(!found));

}

static BLOCK ___regex(STRING reg_str)
{
	regex_t* reg = gc_malloc(sizeof *reg);
	const int status = regcomp(reg, reg_str, REG_EXTENDED | REG_NEWLINE | REG_NOSUB);
	errno = 0; // Hmmm
	if unlikely(status)
	{
		char reg_err[256];
		regerror(status, reg, reg_err, 256);
		throw_error_fmt("Compile error (%s) in regex '%.32s'", reg_err, reg_str);
	}

	return (cognate_block){ .env = (void*)reg, .fn = apply_regex };
}

void match_regex(uint8_t* env)
{
	regex_t reg = *(regex_t*)env;
	STRING str = unbox_STRING(pop());
	size_t groups = reg.re_nsub + 1;
	regmatch_t matches[groups];
	const int found = regexec(&reg, str, groups, matches, 0);
	if unlikely(found != 0 && found != REG_NOMATCH)
	throw_error_fmt("Regex failed matching string '%.32s'", str);

	LIST lst = NULL;
	if (found == 0) {
		for (unsigned int g = 0; g < groups; g++)
		{
			size_t from = matches[g].rm_so;
			size_t to = matches[g].rm_eo;

			if (from == (size_t)-1)
				break;

			cognate_list *node = gc_malloc(sizeof *node);
			char* item = gc_strndup(str, to);
			node->object = box_STRING(item + from);
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
		lst = prev;
	}
	push(box_LIST(lst));
}

static BLOCK ___regexDmatch(STRING reg_str)
{
	regex_t reg;
	const int status = regcomp(&reg, reg_str, REG_EXTENDED | REG_NEWLINE);
	errno = 0;
	if unlikely(status)
	{
		char reg_err[256];
		regerror(status, &reg, reg_err, 256);
		throw_error_fmt("Compile error (%s) in regex '%.32s'", reg_err, reg_str);
	}

	return (cognate_block){ .env = (void*)&reg, .fn = match_regex };
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
	char* const str = gc_flatmalloc (MB_CUR_MAX + 1);
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
*/

static STRING ___show(ANY o)
{
	return show_object(o, 1, NULL);
}

static LIST ___split(STRING sep, STRING str)
{
	if (!*sep) throw_error("Empty separator");
	LIST lst = NULL;
	size_t len = strlen(sep);
	char* found;
	while ((found = strstr(str, sep)))
	{
		found = strstr(str, sep);
		if (found != str)
		{
			char* item = gc_flatmalloc(found - str + 1);
			memcpy(item, str, found - str);
			item[found - str] = '\0';
			cognate_list* node = gc_malloc(sizeof *node);
			node->object = box_STRING(item);
			node->next = lst;
			lst = node;
		}
		str = found + len;
	}
	if (*str)
	{
		cognate_list* node = gc_malloc(sizeof *node);
		node->object = box_STRING(str);
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

static STRING ___uppercase(STRING str)
{
	char* converted = gc_strdup(str);
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
	char* converted = gc_strdup(str);
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
	ANY* b = gc_malloc(sizeof *b);
	*b = a;
	return b;
}

static ANY ___unbox(BOX b)
{
	return *b;
}

static void ___set(BOX b, ANY a)
{
	*b = a;
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
	return radians_to_degrees(sinh(a));
}

static NUMBER ___coshd(NUMBER a)
{
	return radians_to_degrees(cosh(a));
}

static NUMBER ___tanhd(NUMBER a)
{
	return radians_to_degrees(tanh(a));
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

static IO ___open(STRING path, STRING mode)
{
	assert_impure();
	// TODO mode should definitely be a symbol.
	FILE* fp = fopen(path, mode);
	if unlikely(!fp) throw_error_fmt("cannot open file '%s'", path);
	IO io = gc_malloc(sizeof *io);
	io->path = path;
	io->mode = mode;
	io->file = fp;
	return io;
}

static STRING ___readDfile(IO io)
{
	assert_impure();
	// Read a file to a string.
	FILE *fp = io->file;
	if unlikely(!io->mode) throw_error_fmt("File '%s' is not open", io->path);
	if unlikely(fp == NULL) throw_error_fmt("Cannot open file '%s'", io->path);
	struct stat st;
	fstat(fileno(fp), &st);
	char* const text = gc_flatmalloc (st.st_size + 1);
	if (fread(text, sizeof(char), st.st_size, fp) != (unsigned long)st.st_size)
		throw_error_fmt("Error reading file '%s'", io->path);
	text[st.st_size] = '\0'; // Remove trailing eof.
	return text;
	// TODO: single line (or delimited) file read function for better IO performance
}

static void ___close(IO io)
{
	assert_impure();
	fclose(io->file);
	io->file = NULL;
}

static BOOLEAN ___openQ(IO io)
{
	return (BOOLEAN)io->file;
}

static STRING ___fileDname(IO io)
{
	return io->path;
}

static STRING ___fileDmode(IO io)
{
	return io->mode; // TODO symbol
}

static void ___write(STRING s, IO io)
{
	fputs(s, io->file);
}

static void ___seek(NUMBER n, IO io)
{
	size_t p = n;
	if unlikely(p != n) throw_error_fmt("cannot seek to position %.14g", n);
	fseek(io->file, p, SEEK_CUR);
}

static void invalid_jump(uint8_t* env)
{
	throw_error("cannot resume expired continuation");
}

static void oh_no(uint8_t* env)
{
	longjmp(*(jmp_buf*)env, 1);
}

static void ___begin(BLOCK f)
{
	jmp_buf b;
	if (!setjmp(b))
	{
		BLOCK a = (BLOCK) {.fn=oh_no, .env=(void*)&b};
		push(box_BLOCK(a));
		call_block(f);
		a.fn = invalid_jump;
	}
}

static LIST ___empty (void)
{
	return NULL;
}

static DICT ___dict (BLOCK expr)
{
	flush_stack_cache();
	ANYPTR tmp_stack_start = stack.start;
	stack.start = stack.top;
	// Eval expr
	call_block(expr);
	// Move to a list.
	DICT d = NULL;
	flush_stack_cache();
	size_t len = stack_length();
	if (len % 2 != 0) throw_error("Dict initialiser must be key-value pairs");
	for (size_t i = 0; i < len; i += 2)
	{
		STRING key = unbox_STRING(stack.start[i+1]);
		ANY value = stack.start[i];
		DICT ptr = d;
		cognate_dict** assign = (cognate_dict**)&d;
		while (ptr != NULL)
		{
			long diff = strcmp(ptr->key, key);
			if (diff > 0) { assign = (cognate_dict**)&ptr->child1 ; ptr = ptr->child1; }
			else if (diff < 0) { assign = (cognate_dict**)&ptr->child2 ; ptr = ptr->child2; }
			else throw_error("duplicate keys in Dict initialiser");
		}

		*assign = gc_malloc(sizeof (**assign));
		(*assign)->child1 = NULL;
		(*assign)->child2 = NULL;
		(*assign)->key = key;
		(*assign)->value = value;
	}
	stack.top = stack.start;
	stack.start = tmp_stack_start;
	return d;
}

static DICT ___insert(STRING key, ANY value, DICT d)
{
	// TODO at the moment dicts are UNBALANCED!!
	// This is obviously bad performance-wise and means real-life performance is not O(log(n)).
	// Thus at some point we need to balance all these dicts - preserving all immutable references!
	long diff;
	cognate_dict* D = gc_malloc(sizeof *D);
	if (d == NULL || (diff = strcmp(d->key, key)) == 0)
	{
		D->child1 = d ? d->child1 : NULL;
		D->child2 = d ? d->child2 : NULL;
		D->key = key;
		D->value = value;
	}
	else if (diff > 0)
	{
		D->child2 = d->child2;
		D->child1 = ___insert(key, value, d->child1);
		D->key = d->key;
		D->value = d->value;
	}
	else if (diff < 0)
	{
		D->child1 = d->child1;
		D->child2 = ___insert(key, value, d->child2);
		D->key = d->key;
		D->value = d->value;
	}

	return D;
}

static ANY ___get(STRING key, DICT d)
{
	if (d == NULL) throw_error_fmt("%s is not in dictionary", key);

	long diff = strcmp(d->key, key);

	if (diff == 0) return d->value;
	else if (diff > 0) return ___get(key, d->child1);
	else return ___get(key, d->child2);
}

// ---------- ACTUAL PROGRAM ----------
