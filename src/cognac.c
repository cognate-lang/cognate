#include "cognac.h"
#include "parser.h"
#include "runtime_bytes.h"
#include "prelude.h"
#include <limits.h>
#include <assert.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <execinfo.h>

#define STR_(x) #x
#define STR(x) STR_(x)
#define MAX_ERROR_LINE_LENGTH 256

ast_list_t* full_ast = NULL;
module_t* pmod = NULL;
char* heap = NULL;
module_t prelude1 = { .prefix = "prelude" }; // written in C
module_t prelude2 = { .prefix = "prelude" }; // written in Cognate
module_list_t preludes = { .mod=&prelude2, .next = &(module_list_t){.mod=&prelude1, .next=NULL} };

char runtime_filename[] = "/tmp/cognac-runtime-XXXXXX.h";

int usleep (unsigned int);
char* strdup (const char*);

bool debug = false;

static bool is_prelude(module_t* mod)
{
	return mod == &prelude1 || mod == &prelude2;
}

static void print_banner(void)
{
	char* banner =
	"\t   ______                        ______\n"
	"\t  / ____/___  ____ _____  ____ _/ ____/\n"
	"\t / /   / __ \\/ __ `/ __ \\/ __ `/ /\n"
	"\t/ /___/ /_/ / /_/ / / / / /_/ / /___\n"
	"\t\\____/\\____/\\__, /_/ /_/\\__,_/\\____/\n"
	"\t           /____/\n"
	"\t                  Cognate Compiler\n";
	puts(banner);
}

_Noreturn static void unreachable(void)
{
	char msg[] = "\n\n\033[31;1m"
	"\t  ___  _\n"
	"\t / _ \\| |__    _ __   ___\n"
   "\t| | | | '_ \\  | '_ \\ / _ \\\n"
   "\t| |_| | | | | | | | | (_) |\n"
   "\t \\___/|_| |_| |_| |_|\\___/\n\n"
	"\tERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR\n\n"
	"\tThe compiler has reached an unreachable state!\n"
	"\tThis error is my fault not yours, so give\n"
	"\tyourself a pat on the back for finding a bug\n"
	"\tin the compiler - then submit a bug report!\n\n"
	"\tTo make this easier, I'm including a handy\n"
	"\tbacktrace:\n\n";

	write(STDERR_FILENO, msg, sizeof(msg));

	static void *bt[128];
	int bt_sz = backtrace(bt, 128);
	backtrace_symbols_fd(bt, bt_sz, STDERR_FILENO);
	write(STDERR_FILENO, "\033[0m\n\n", sizeof("\033[0m\n\n"));
	abort();
}

static void* alloc(size_t n)
{
	// allocate n bytes (leaking).
	return (heap += n) - n;
}

where_t* parse_pos(char* sym)
{
	where_t* p = alloc(sizeof *p);
	p->mod = pmod;
	p->line = yylloc.first_line;
	p->col = yylloc.first_column;
	p->symbol = sym;
	return p;
}


_Noreturn void type_error(val_type_t expected, val_type_t got, where_t* pos)
{
	char buf[100];
	sprintf(buf, "expected %s\ngot %s", print_val_type(expected), print_val_type(got));
	throw_error(buf, pos);
}

_Noreturn void throw_error(char* message, where_t* where)
{
	/*
	puts(message);
	puts(where->mod->prefix);
	exit(-1);
	*/

	// Calculate width of the "[LINE_NUMBER]" bit
	message = strdup(message);
	char number_box[64];
	sprintf(number_box, "[%zu] ", where->line);
	size_t offset = strlen(number_box) + where->col - 1;

	// Ok now we need to actually get the line
	fclose(where->mod->file);
	if (!where->mod->path) // probably preludes
		where->mod->path = "src/prelude.cog";
	where->mod->file = fopen(where->mod->path, "r");
	while (--where->line)
	{
		char c;
		do { c = fgetc(where->mod->file); } while (c != '\n'); // read one line. TODO this is bad
	}

	char line_start[MAX_ERROR_LINE_LENGTH];
	char* line = line_start;
	fgets(line, MAX_ERROR_LINE_LENGTH, where->mod->file);

	// Now we strip leading whitespace
	while (isspace(*line)) line++, offset--;

	// Now print shit
	fprintf(stderr, "\n\033[0;2m%s\033[0;1m%s", number_box, line); // Should have newline already(?)
	for (int i = 1 ; i < offset ; ++i) fputc(' ', stderr);
	fprintf(stderr, "\033[31;1m|\\");
	for (char* p = message + 1; *p != '\n' && *p != '\0' ; p++) fputc('_', stderr);
	fputc('\n', stderr);
	for (;;)
	{
		char* next = message;
		for (; *next != '\n' && *next != '\0' ; next++);
		bool again = *next == '\n';
		*next = '\0';
		for (int i = 1 ; i < offset ; ++i) fputc(' ', stderr);
		fputs("|", stderr);
		fputs(message, stderr);
		if (!again) break;
		fputc('\n', stderr);
		message = next + 1;
	}
	fputs("\033[0m\n\n", stderr);
	fflush(stderr);
	exit(EXIT_FAILURE);
}


func_t* unknown_func = &(func_t) {
	.argc=0,
	.args=NULL,
	.returns=false,
	.stack=true,
	.captures=NULL,
	.unique=false,
};

func_t* call_to_func(ast_t* op)
{
	return op->type==call
			&& op->word->val->source
			&& op->word->val->source->op->type==closure
		? op->word->val->source->op->func
		: unknown_func;
}

val_list_t* reverse(val_list_t* v)
{
	val_list_t* new = NULL;
	for (val_list_t* c = v ; c ; c = c->next)
	{
		val_list_t* V = alloc(sizeof *V);
		V->next = new;
		V->val = c->val;
		new = V;
	}
	return new;
}

func_t* static_call_to_func(ast_t* c)
{
	if (c->type == call) return unknown_func;
	else return c->func;
}

val_list_t* push_val (val_t* v, val_list_t* rest)
{
	val_list_t* a = alloc(sizeof *a);
	a->val = v;
	a->next = rest;
	return a;
}

val_t* make_value (val_type_t type, ast_list_t* source)
{
	val_t* v = alloc(sizeof *v);
	v->source = source;
	v->type = type;
	return v;
}

word_list_t* push_word(word_t* w, word_list_t* next)
{
	word_list_t* n = alloc(sizeof *n);
	n->word = w;
	n->next = next;
	return n;
}

word_t* make_word(char* name, type_t calltype, val_t* v, module_t* mod)
{
	static size_t shadow_id = 1;
	word_t* w = alloc(sizeof *w);
	w->used = false;
	w->name = name;
	w->shadow_id = shadow_id++;
	w->used_early = false;
	w->mod = mod;
	w->calltype = calltype;
	w->val = v;
	return w;
}

func_list_t* push_func(func_t* f, func_list_t* next)
{
	func_list_t* n = alloc(sizeof *n);
	n->func = f;
	n->next = next;
	return n;
}

char* make_func_name(void)
{
	static size_t fid = 0;
	char* str = alloc(20);
	sprintf(str, "fn%zu", fid++);
	return str;
}

func_t* make_func(ast_list_t* tree, char* name)
{
	func_t* func = alloc(sizeof *func);
	func->unmangled_name = NULL;
	func->returns = false;
	func->ops = tree;
	func->args = NULL;
	func->argc = 0;
	func->has_args = false;
	func->unique = false;
	func->branch = false;
	func->has_regs = false;
	func->builtin = false;
	func->name = name;
	func->locals = NULL;
	func->generic_variant = NULL;
	func->generic = false;
	func->calls = NULL;
	func->stack = false; // hmm
	func->captures = NULL;
	func->entry = false;
	return func;
}

ast_t* make_op(type_t type, void* data, where_t* pos)
{
	ast_t* a = alloc(sizeof *a);
	a->type = type;
	a->data = data;
	a->where = pos;
	return a;
}

void insert_op_before(ast_t* op, ast_list_t* current)
{
	ast_list_t* n = alloc(sizeof *n);
	n->prev = current->prev;
	n->next = current;
	if (current->prev) current->prev->next = n;
	current->prev = n;
	n->op = op;
}

void insert_op_after(ast_t* op, ast_list_t* current)
{
	ast_list_t* n = alloc(sizeof *n);
	n->prev = current;
	n->next = current->next;
	if (current->next) current->next->prev = n;
	current->next = n;
	n->op = op;
}

void remove_op(ast_list_t* op)
{
	op->prev->next = op->next;
	op->next->prev = op->prev;
}

reg_t* make_register(val_type_t t, ast_list_t* source)
{
	if (!t) unreachable();
	static size_t next_register_id = 0;
	reg_t* reg = alloc (sizeof *reg);
	reg->type = t;
	reg->source = source;
	reg->id = next_register_id++;
	reg->next = reg->prev = NULL;
	return reg;
}

void push_register_front(reg_t* reg, reg_dequeue_t* registers)
{
	if (!reg) unreachable();
	if (registers->len)
	{
		reg->prev = NULL;
		reg->next = registers->front;
		registers->front->prev = reg;
		registers->front = reg;
	}
	else
	{
		registers->front = registers->rear = reg;
		reg->next = NULL;
		reg->prev = NULL;
	}
	registers->len++;
}

void push_register_rear(reg_t* reg, reg_dequeue_t* registers)
{
	if (!reg) unreachable();
	if (registers->len)
	{
		reg->next = NULL;
		reg->prev = registers->rear;
		registers->rear->next = reg;
		registers->rear = reg;
	}
	else
	{
		registers->rear = registers->front = reg;
		reg->next = NULL;
		reg->prev = NULL;
	}
	registers->len++;
}

void clear_registers(reg_dequeue_t* registers)
{
	registers->front = registers->rear = NULL;
	registers->len = 0;
}

reg_t* pop_register_front(reg_dequeue_t* registers)
{
	reg_t* reg = registers->front;
	if (!registers->len) return NULL;
	registers->front = registers->front->next;
	registers->len--;
	return reg;
}

reg_t* pop_register_rear(reg_dequeue_t* registers)
{
	reg_t* reg = registers->rear;
	if (!registers->len) return NULL;
	registers->rear = registers->rear->prev;
	registers->len--;
	return reg;
}

reg_dequeue_t* make_register_dequeue(void)
{
	reg_dequeue_t* r = alloc(sizeof *r);
	r->front = r->rear = NULL;
	r->len = 0;
	return r;
}


module_t* create_module(char* path)
{
	module_t* mod = alloc(sizeof *mod);
	mod->path = path;
	mod->file = fopen(path, "r");
	char* path2 = strdup(path);
	char* path3 = path2;
	for (char* s = path2 ; *s ; ++s) if (*s == '/') path2 = s+1;
	for (char* s = path2 ; *s ; ++s) if (*s == '.') { *s = '\0'; break; }
	path2[-1] = '\0';
	mod->prefix = lowercase(path2);
	mod->dir = path3;
	mod->tree = NULL;
	mod->funcs = NULL;
	mod->uses = &preludes;
	return mod;
}

void module_parse(module_t* mod)
{
	if (mod->path)
	{
		FILE* t = fopen(mod->path, "r");
		if (!t)
		{
			if (!mod->first_ref)
				fprintf(stderr, "\nCan't open file '%s'!\n", mod->path);
			else
			{
				throw_error("can't find module", mod->first_ref);
			}
			exit(EXIT_FAILURE);
		}
		fclose(t);
	}
	pmod = mod;
	yyin = mod->file; // imagine having a reentrant parser.
	yylloc.first_line = 1;
	yylloc.first_column = 1;
	yyparse();
	mod->tree = full_ast;
}

ast_list_t* _predeclare(ast_list_t* tree, module_t* mod)
{
	for (ast_list_t* node = tree ; node ; node = node->next)
	{
		switch(node->op->type)
		{
			case let:
			case def:
				{
					word_t* new = make_word(
							node->op->string,
							node->op->type==let?var:call,
							make_value(node->op->type==def?block:any, node), node->op->where->mod);
					node->op->type = bind;
					node->op->word = new;
					insert_op_after(make_op(define, new, node->op->where), tree);
					break;
				}
				break;
			case braces:
				node->op->child = _predeclare(node->op->child, mod);
				break;
			default: break;
		}
	}
	return tree;
}

void shorten_references(module_t* mod)
{
	for (func_list_t* f = mod->funcs ; f ; f = f->next)
	{
		reg_dequeue_t* regs = make_register_dequeue();
		for (ast_list_t* a = f->func->ops ; a ; a = a->next)
		{
			switch (a->op->type)
			{
				default: unreachable();
				case backtrace_push:
				case backtrace_pop:
				case none:
				case define:
					break;
				case pick:
					push_register_front(pop_register_rear(regs), regs);
					break;
				case unpick:
					push_register_rear(pop_register_front(regs), regs);
					break;
				case to_any:
				case from_any:
					pop_register_front(regs);
					push_register_front(make_register(any, a), regs); // preserve source??
					break;
				case literal:
				case closure:
				case pop:
				case load:
				case var:
					push_register_front(make_register(any, a), regs);
					break;
				case branch:
					pop_register_front(regs);
					pop_register_front(regs);
					pop_register_front(regs);
					push_register_front(make_register(any, a), regs);
					break;
				case fn_branch:
					{
						pop_register_front(regs); // bool
						func_t* f = a->op->funcs->func;
						for ( size_t i = 0 ; i < f->argc ; ++i )
							pop_register_front(regs);
						if (f->stack) assert(regs->len == 0);
						if (f->returns)
							push_register_front(make_register(f->rettype, a), regs);
						break;

					}
				case call:
				case static_call:
					{
						func_t* f = static_call_to_func(a->op);
						for ( size_t i = 0 ; i < f->argc ; ++i )
							pop_register_front(regs);
						if (f->stack) assert(regs->len == 0);
						if (f->returns)
							push_register_front(make_register(f->rettype, a), regs);
						break;
					}
				case bind:
					{
						reg_t* r = pop_register_front(regs);
						while (r->source && r->source->op->type == var)
							r->source = r->source->op->word->val->source;
						a->op->word->val->source = r->source;
						break;
					}
				case push:
				case ret:
				case drop:
					pop_register_front(regs);
					break;
			}
		}
	}
}

void resolve_early_use(module_t* mod)
{
	for (func_list_t* f = mod->funcs ; f ; f = f->next)
	{
		word_list_t* used = NULL;
		word_list_t* bound = NULL;
		for (ast_list_t* node = f->func->ops ; node ; node = node->next)
		{
			switch(node->op->type)
			{
				case var:
				case call:
					used = push_word(node->op->word, used);
					break;
				case closure:
					for (word_list_t* w = node->op->func->captures ; w ; w = w->next)
						used = push_word(w->word, used);
					break;
				case bind:
					{
						for (word_list_t* w = bound ; w ; w=w->next )
							if (w->word == node->op->word) goto end;
						for (word_list_t* w = used ; w ; w=w->next )
						{
							if (w->word == node->op->word)
							{
								node->op->word->used_early = true;
								goto end;
							}
						}
						node->op->word->used_early = false;
end:;
	 					bound = push_word(node->op->word, bound);
						break;
					}
				default: break;
			}
		}
	}
}

void predeclare(module_t* mod)
{
	mod->tree = _predeclare(mod->tree, mod);
}

void _resolve_scope(ast_list_t* tree, word_list_t* words, module_t* m)
{
	for (ast_list_t* node = tree ; node ; node = node->next)
	{
		switch(node->op->type)
		{
			case braces:
				_resolve_scope(node->op->child, words, m);
				break;
			case identifier:;
				word_list_t* w = words;
				for (; w ; w = w->next)
				{
					if (!strcmp(w->word->name, node->op->string) && (w->word->mod == node->op->where->mod || is_prelude(w->word->mod)))
						break;
				}
				if (!w) throw_error("undefined word", node->op->where);
				node->op->type = w->word->calltype;
				node->op->word = w->word;
				break;
			case module_identifier:;
				w = words;
				char* mod_name = strdup(node->op->string);
				char* i = mod_name;
				while (*i != ':') ++i;
				*i = '\0';
				char* ident = i + 1;
				for (; w ; w = w->next)
					if (!strcmp(w->word->name, ident) && !strcmp(mod_name, w->word->mod->prefix))
						break;
				if (!w) throw_error("undefined word", node->op->where);
				node->op->type = w->word->calltype;
				node->op->word = w->word;
				break;
			case define:
				words = push_word(node->op->word, words);
				break;
			default: break;
		}
	}
}

void resolve_scope(module_t* mod)
{
	_resolve_scope(mod->tree, builtins(), mod);
}

func_t* _flatten_ast(ast_list_t* tree, func_list_t** rest)
{
	// Build a list of functions in the `rest` parameter
	// return the entry function for the AST node
	func_t* func = make_func(tree, make_func_name());
	//func->stack = true;
	static bool entry = 1;
	func->entry = entry;
	entry = 0;
	*rest = push_func(func, *rest);
	for (ast_list_t* node = tree ; node ; node = node->next)
	{
		if (node->op->type == braces)
		{
			node->op->func = _flatten_ast(node->op->child, rest);
			node->op->type = closure;
		}
	}
	return func;
}

void flatten_ast(module_t* mod)
{
	func_list_t* f = NULL;
	_flatten_ast(mod->tree, &f);
	mod->funcs = f;
	for (func_list_t* fn = f ; fn ; fn = fn->next)
	{
		if (fn->func->entry)
		{
			mod->entry = fn->func;
			return;
		}
	}
}

const char* c_literal(lit_t* literal)
{
	if (literal->type == symbol)
	{
		char* prefix = "SYM";
		char s[strlen(literal->string) + strlen(prefix) + 1];
		s[0] = '\0';
		strcat(s, prefix);
		strcat(s, literal->string);
		return strdup(s);
	}
	return literal->string; // TODO
}

const char* print_val_type(val_type_t type)
{
	switch (type)
	{
		case number: return "number";
		case dict:   return "dict";
		case symbol: return "symbol";
		case string: return "string";
		case block:  return "block";
		case list:   return "list";
		case boolean:return "boolean";
		case any:    return "any";
		case box:    return "box";
		case io:     return "io";
		case NIL:    return "NIL";
		case strong_any: return "strong_any";
	}
	return NULL;
}



const char* c_val_type(val_type_t type)
{
	switch (type)
	{
		case number: return "NUMBER";
		case dict:   return "DICT";
		case symbol: return "SYMBOL";
		case string: return "STRING";
		case block:  return "BLOCK";
		case list:   return "LIST";
		case boolean:return "BOOLEAN";
		case any:    return "ANY";
		case box:    return "BOX";
		case io:     return "IO";
		case strong_any: return "STRONG_ANY";
		case NIL:    unreachable();
	}
	return NULL;
}

char* sanitize(char* s)
{
	char* str = strdup(s);
	for ( size_t i = 0 ; str[i] ; ++i )
	{
		switch(str[i])
		{
			case '-':  str[i] = 'D'; break;
			case '!':  str[i] = 'X'; break;
			case '?':  str[i] = 'Q'; break;
			case '=':  str[i] = 'E'; break;
			case '<':  str[i] = 'L'; break;
			case '>':  str[i] = 'G'; break;
			case '+':  str[i] = 'P'; break;
			case '*':  str[i] = 'M'; break;
			case '/':  str[i] = 'S'; break;
			case '^':  str[i] = 'C'; break;
			case '\'': str[i] = 'A'; break;
			default:;
		}
	}
	return str;
}

char* prefix(char* str)
{
	size_t len = strlen(str);
	char* s = alloc(len + 4);
	*(int*)s = *(int*)"___\0";
	strcpy(s+3, str);
	return s;
}

const char* c_word_name(word_t* word)
{
	char* str = alloc(strlen(word->name) + 20);
	*(uint32_t*)str = *(uint32_t*)"___";
	if (word->shadow_id)
		sprintf(str+3, "%s_%zu", word->name, word->shadow_id);
	else
		sprintf(str+3, "%s", word->name);
	return sanitize(str);
}

void to_exe(module_t* mod)
{
	char* exe_path = strdup(mod->path);
	exe_path[strlen(exe_path) - 4] = '\0';

	char* c_source_path = strdup(mod->path);
	c_source_path[strlen(c_source_path) - 2] = '\0';

	char* debug_args[] = {
		STR(CC), c_source_path, "-o", exe_path,
		"-O0", "-ggdb3", "-g", "-rdynamic", "-DDEBUG",
		"-lm", "-Wall", "-Wpedantic", "-Wno-unused", NULL
	} ;

	char* normal_args[] = {
		STR(CC), c_source_path, "-o", exe_path,
		"-Ofast", "-flto", "-s", "-w",
		"-lm", "-Wall", "-Wpedantic", "-Wno-unused", NULL
	};

	char** args = debug ? debug_args : normal_args;

	pid_t p = fork();
	if (!p) execvp(args[0], args);
	int status;
	int i = 0;
	while (!waitpid(p, &status, WNOHANG))
	{
		usleep(50000);
		fputc('\r', stdout);
		printf("%s ", mod->path);
		char* bar[3] = { "> >> >> >> >> >> >> >> >> >> ", ">> >> >> >> >> >> >> >> >> >>", " >> >> >> >> >> >> >> >> >> >" };
		fputs(bar[i], stdout);
		if (++i == 3) i = 0;
		printf(" %s", exe_path);
		fflush(stdout);
	}
	fputc('\n', stdout);
	remove(runtime_filename);
	if (status != EXIT_SUCCESS) exit(status);
}

void c_emit_funcall(func_t* fn, FILE* c_source, reg_dequeue_t* registers)
{
	fprintf(c_source, "%s(", sanitize(fn->name));
	if (!fn->generic)
		for (word_list_t* w = fn->captures ; w ; w = w->next)
		{
			fprintf(c_source, "%s", c_word_name(w->word));
			if (w->next || fn->argc) fprintf(c_source, ",");
		}
	else fprintf(c_source, "NULL");
	for (size_t i = 0 ; i < fn->argc ; ++i)
	{
		char* sep = i + 1 == fn->argc ? ")" : ", ";
		reg_t* r = pop_register_front(registers);
		fprintf(c_source, "_%zu%s", r->id, sep);
	}
	if (fn->argc == 0) fprintf(c_source, ")");

}

void to_c(module_t* mod)
{
	mkstemps(runtime_filename, 2);
	FILE* runtime_file = fopen(runtime_filename, "w");
	fprintf(runtime_file, "%.*s", src_runtime_h_len, (char*)src_runtime_h);
	fclose(runtime_file);

	char* c_source_path = strdup(mod->path);
	c_source_path[strlen(c_source_path) - 2] = '\0';
	FILE* c_source = fopen(c_source_path, "w");
	fprintf(c_source, "#include \"%s\"\n\n", runtime_filename);
	for (symbol_list_t* syms = mod->symbols ; syms ; syms = syms->next)
		fprintf(c_source, "SYMBOL SYM%s = \"%s\";\n", syms->text, syms->text);
	if (mod->symbols) fputc('\n', c_source);
	for (func_list_t* func = mod->funcs ; func ; func = func->next)
	{
		fprintf(c_source, "static %s %s(",
				func->func->returns ? c_val_type(func->func->rettype) : "void",
				func->func->name);
		if (!func->func->generic) for (word_list_t* w = func->func->captures ; w ; w = w->next)
		{
			if (w->word->used_early)
				fprintf(c_source, "early_%s*", c_val_type(w->word->val->type));
			else
				fprintf(c_source, "%s", c_val_type(w->word->val->type));
			if (w->next || func->func->argc) fprintf(c_source, ", ");
		}
		else
		{
			fprintf(c_source, "uint8_t* env");
			if (func->func->argc) fprintf(c_source, ", ");
		}
		//reg_dequeue_t* ar = make_register_dequeue();
		for (val_list_t* v = func->func->args ; v ; v = v->next)
		{
			fprintf(c_source, "%s", c_val_type(v->val->type));
			if (v->next) fprintf(c_source, ", ");
		}
		if (!func->func->generic && !func->func->captures && !func->func->argc)
			fprintf(c_source, "void");
		fprintf(c_source, ");\n");
	}
	fputc('\n', c_source);
	for (func_list_t* func = mod->funcs ; func ; func = func->next)
	{
		//size_t num_words = 0;
		fprintf(c_source, "static %s %s(",
		func->func->returns ? c_val_type(func->func->rettype) : "void",
		func->func->name);
		if (!func->func->generic)
			for (word_list_t* w = func->func->captures ; w ; w = w->next)
			{
				if (w->word->used_early)
					fprintf(c_source, "early_%s* %s",
							c_val_type(w->word->val->type),
							c_word_name(w->word));
				else
					fprintf(c_source, "%s %s",
							c_val_type(w->word->val->type),
							c_word_name(w->word));
				if (w->next || func->func->argc) fprintf(c_source, ", ");
			}
		else
		{
			fprintf(c_source, "uint8_t* env");
			if (func->func->argc) fprintf(c_source, ", ");
		}
		reg_dequeue_t* ar = make_register_dequeue();
		for (val_list_t* v = func->func->args ; v ; v = v->next)
		{
			reg_t* r = make_register(v->val->type, NULL);
			fprintf(c_source, "%s _%zu", c_val_type(v->val->type), r->id);
			push_register_front(r, ar);
			if (v->next) fprintf(c_source, ", ");
		}
		if (!(func->func->generic) && !func->func->captures && !func->func->args)
			fprintf(c_source, "void");
		fprintf(c_source, ") {\n");
		if (func->func->generic)
			for (word_list_t* w = func->func->captures ; w ; w = w->next)
			{
				if (w->word->used_early)
				{
					fprintf(c_source, "\tearly_%s* %s = *(early_%s**)env;\n",
						c_val_type(w->word->val->type),
						c_word_name(w->word),
						c_val_type(w->word->val->type));
					if (w->next)
						fprintf(c_source, "\tenv += sizeof(early_%s*);\n", c_val_type(w->word->val->type));
				}
				else
				{
					fprintf(c_source, "\t%s %s = *(%s*)env;\n",
						c_val_type(w->word->val->type),
						c_word_name(w->word),
						c_val_type(w->word->val->type));
					if (w->next)
						fprintf(c_source, "\tenv += sizeof(%s);\n", c_val_type(w->word->val->type));
				}
			}

		for (word_list_t* w = func->func->locals ; w ; w = w->next)
		{
			if (w->word->used_early)
			{
				fprintf(c_source, "\tearly_%s* %s = gc_malloc(sizeof(early_%s));\n",
					c_val_type(w->word->val->type),
					c_word_name(w->word),
					c_val_type(w->word->val->type));
			}
			else
				fprintf(c_source, "\t%s %s;\n",
					c_val_type(w->word->val->type),
					c_word_name(w->word));
		}
		reg_dequeue_t* registers = make_register_dequeue();
		reg_t* res = NULL;
		size_t bid = 0;
		for (ast_list_t* op = func->func->ops ; op ; op = op->next)
		{
			switch (op->op->type)
			{
				default: unreachable();
				case fn_branch:
					{
						func_t* v = op->op->funcs->func;
						if (v->returns)
						{
							reg_t* ret = make_register(v->rettype, NULL);
							fprintf(c_source, "\t%s _%zu = ",
									c_val_type(ret->type),
									ret->id);
							reg_dequeue_t saved = *registers;
							for (func_list_t* f = op->op->funcs ; f ; f = f->next)
							{
								if (f->next)
								{
									reg_t* r1 = pop_register_front(registers);
									fprintf(c_source, "_%zu ? ", r1->id);
									c_emit_funcall(f->func, c_source, registers);
									fprintf(c_source, " : ");
								}
								else
								{
									c_emit_funcall(f->func, c_source, registers);
									fprintf(c_source, ";\n");
								}
								*registers = saved;
								pop_register_front(registers);
								saved = *registers;
							}
							for (size_t i = 0; i < v->argc; ++i)
								pop_register_front(registers);
							push_register_front(ret, registers);
						}
						else
						{
							reg_dequeue_t saved = *registers;
							for (func_list_t* f = op->op->funcs ; f ; f = f->next)
							{
								if (f->next)
								{
									reg_t* r1 = pop_register_front(registers);
									fprintf(c_source, "\tif (_%zu) ", r1->id);
									c_emit_funcall(f->func, c_source, registers);
									fprintf(c_source, ";\n\telse ");
								}
								else
								{
									c_emit_funcall(f->func, c_source, registers);
									fprintf(c_source, ";\n");
								}
								*registers = saved;
								pop_register_front(registers);
								saved = *registers;
							}
							for (size_t i = 0; i < v->argc; ++i)
								pop_register_front(registers);
						}
						break;
					}
				case branch:
					{
						reg_t* r1 = pop_register_front(registers);
						reg_t* r2 = pop_register_front(registers);
						reg_t* r3 = pop_register_front(registers);
						assert(r2->type == r3->type);
						reg_t* ret = make_register(r2->type, NULL);
						push_register_front(ret, registers);
						fprintf(c_source, "\t%s _%zu = _%zu ? _%zu : _%zu;\n",
								c_val_type(r2->type), ret->id, r1->id, r2->id, r3->id);
					}
					break;
				case none: break;
				case define: unreachable();
				case push:
					fprintf(c_source, "\tpush(_%zu);\n", pop_register_front(registers)->id);
					break;
				case ret:
					res = pop_register_front(registers);
					break;
				case drop:
					pop_register_front(registers);
					break;
				case pop:
					{
						reg_t* reg = make_register(any, NULL);
						push_register_front(reg, registers);
						fprintf(c_source, "\tANY _%zu = pop();\n", reg->id);
						break;
					}
				case load:
					{
						push_register_front(pop_register_rear(ar), registers);
						break;
					}
				case pick:
					push_register_front(pop_register_rear(registers), registers);
					break;
				case unpick:
					push_register_rear(pop_register_front(registers), registers);
					break;

				case literal:
					{
						reg_t* reg = make_register(op->op->literal->type, NULL);
						push_register_front(reg, registers);
						fprintf(c_source, "\t%s _%zu = %s;\n",
							c_val_type(op->op->literal->type),
							reg->id,
							c_literal(op->op->literal));
						break;
					}
				case backtrace_push:
					if (op->op->where && op->op->where->mod->path && op->op->where->symbol)
						fprintf(c_source, "\tBACKTRACE_PUSH(\"%s\", %zu, %zu, \"%s\", %zu);\n", op->op->where->symbol, op->op->where->line, op->op->where->col, op->op->where->mod->path, bid++);
					break;
				case backtrace_pop:
					if (op->op->where && op->op->where->mod->path && op->op->where->symbol)
						fprintf(c_source, "\tBACKTRACE_POP();\n");
					break;
				case var:
					{

						reg_t* reg = make_register(op->op->word->val->type, NULL);
						push_register_front(reg, registers);
						if (op->op->word->used_early)
							fprintf(c_source, "\tCHECK_DEFINED(%c%s, %s);\n\t%s _%zu = %s->value;\n",
								toupper(op->op->word->name[0]),
								op->op->word->name+1,
								c_word_name(op->op->word),
								c_val_type(op->op->word->val->type),
								reg->id,
								c_word_name(op->op->word));
						else
							fprintf(c_source, "\t%s _%zu = %s;\n",
								c_val_type(op->op->word->val->type),
								reg->id,
								c_word_name(op->op->word));
						break;
					}
				case bind:
					{
						const char* cname = c_word_name(op->op->word);
						size_t reg_id = pop_register_front(registers)->id;
						if (op->op->word->used_early)
						{
							fprintf(c_source, "\t%s->defined = 1;\n\t%s->value = _%zu;\n",
								cname,
								cname,
								reg_id);
							/*
							if (strlen(op->op->word->name) && op->op->where->mod->path)
							{
								if (op->op->word->val->type == any) fprintf(c_source, "\tVARS_PUSH(\"%s\", %s, %s->value);\n", op->op->word->name, cname, cname);
								else fprintf(c_source, "\tVARS_PUSH(\"%s\", %s, box_%s(%s->value));\n", op->op->word->name, cname, c_val_type(op->op->word->val->type), cname);
		 					}
							*/
						}
						else
						{
							fprintf(c_source, "\t%s = _%zu;\n",
								cname,
								reg_id);
							/*
							if (strlen(op->op->word->name) && op->op->where->mod->path)
							{
								if (op->op->word->val->type == any) fprintf(c_source, "\tVARS_PUSH(\"%s\", %s, %s);\n", op->op->word->name, cname, cname);
								else fprintf(c_source, "\tVARS_PUSH(\"%s\", %s, box_%s(%s));\n", op->op->word->name, cname, c_val_type(op->op->word->val->type), cname);
							}
							*/
						}
					}
					break;
				case static_call:
					{
						reg_t* ret = NULL;
						func_t* fn = op->op->func;
						if (fn->returns)
						{
							ret = make_register(fn->rettype, NULL);
							fprintf(c_source, "\t%s _%zu = ",
									c_val_type(fn->rettype),
									ret->id);
						}
						else fprintf(c_source, "\t");
						c_emit_funcall(fn, c_source, registers);
						fprintf(c_source, ";\n");
						if (fn->returns) push_register_front(ret, registers);
						break;
					}
				case call:
					// TODO remove call and use var and a do op
					if (op->op->word->used_early)
						fprintf(c_source, "\tCHECK_DEFINED(%c%s, %s);\n\t%s->value.fn(%s->value.env);\n",
							toupper(op->op->word->name[0]),
							op->op->word->name+1,
							c_word_name(op->op->word),
							c_word_name(op->op->word),
							c_word_name(op->op->word));
					else
						fprintf(c_source, "\t%s.fn(%s.env);\n",
							c_word_name(op->op->word),
							c_word_name(op->op->word));
					break;
				case closure:
					{
						// TODO: variable loading order is a tad funky, could be neater if loaded in reverse
						//for (word_list_t* w = op->op->func->captures ; w ; w = w->next) num_words++;
						reg_t* reg = make_register(block, NULL);
						push_register_front(reg, registers);
						fprintf(c_source, "\tBLOCK _%zu = (BLOCK) { .fn = %s",
								reg->id, op->op->func->generic_variant->name);
						if (op->op->func->captures)
						{
							fprintf(c_source, ", .env = gc_malloc(");
							for (word_list_t* w = op->op->func->captures ; w ; w = w->next)
							{
								fprintf(c_source, "sizeof(%s)", c_val_type(w->word->val->type));
								if (w->next)
									fprintf(c_source, " + ");
							}
							fprintf(c_source, ")");
						}
						fprintf(c_source, " };\n");
						size_t i = 0;
						for (word_list_t* w = op->op->func->captures ; w ; w = w->next, i++)
						{
							if (w->word->used_early)
							{
								fprintf(c_source, "\t*(early_%s**)_%zu.env = %s;\n",
									c_val_type(w->word->val->type),
									reg->id, c_word_name(w->word));
								if (w->next)
									fprintf(c_source, "\t_%zu.env += sizeof(early_%s*);\n", reg->id, c_val_type(w->word->val->type));
							}
							else
							{
								fprintf(c_source, "\t*(%s*)_%zu.env = %s;\n",
									c_val_type(w->word->val->type),
									reg->id, c_word_name(w->word));
								if (w->next)
									fprintf(c_source, "\t_%zu.env += sizeof(%s);\n", reg->id, c_val_type(w->word->val->type));
							}
						}
						if (op->op->func->captures && op->op->func->captures->next)
						{
							fprintf(c_source, "\t_%zu.env -= ", reg->id);
							for (word_list_t* w = op->op->func->captures ; w->next ; w = w->next)
							{
								if (w->word->used_early)
									fprintf(c_source, "sizeof(early_%s*)", c_val_type(w->word->val->type));
								else
									fprintf(c_source, "sizeof(%s)", c_val_type(w->word->val->type));
								if (w->next && w->next->next)
									fprintf(c_source, " + ");
							}
							fprintf(c_source, ";\n");
						}
						break;
					}
				case to_any:
					{
						reg_t* in = pop_register_front(registers);
						reg_t* out = make_register(any, NULL);
						push_register_front(out, registers);
						fprintf(c_source, "\tANY _%zu = box_%s(_%zu);\n",
							out->id,
							c_val_type(op->op->val_type),
							in->id);
						break;
					}
				case from_any:
					{
						reg_t* in = pop_register_front(registers);
						reg_t* out = make_register(op->op->val_type, NULL);
						push_register_front(out, registers);
						fprintf(c_source, "\t%s _%zu = unbox_%s(_%zu);\n",
								c_val_type(op->op->val_type),
								out->id,
								c_val_type(op->op->val_type),
								in->id);
						break;
					}
				}
		}

		/*
		for (word_list_t* w = func->func->locals ; w ; w = w->next)
		{
			if (strlen(w->word->name) && func->func->ops && func->func->ops->op->where
				&& func->func->ops->op->where->mod && func->func->ops->op->where->mod->path)
				fprintf(c_source, "\tVARS_POP();\n");
		}
		*/

		if (res)
			fprintf(c_source, "\treturn _%zu;\n", res->id);
		fprintf(c_source, "}\n");
		if (func->next) fputc('\n', c_source);
	}
	fclose(c_source);
}

void print_ast(ast_list_t* tree, int i)
{
	if (!tree) return;
	for (int ii = 0; ii < i; ++ii) fputs("| ", stdout);
	switch (tree->op->type)
	{
		case identifier: printf("[ident] %s\n", tree->op->string); break;
		case module_identifier: printf("[mod_ident] %s\n", tree->op->string); break;
		case literal:    printf("[literal] %s\n", tree->op->literal->string); break;
		case braces:     puts("\\"); print_ast(tree->op->child, i+1); break;
		case closure:    printf("[closure] %s\n", tree->op->func->name); break;
		case def:        printf("[def] %s\n", tree->op->string); break;
		case let:        printf("[def] %s\n", tree->op->string); break;
		case call:       printf("[call] %s(%zu)\n", tree->op->word->name, tree->op->word->shadow_id); break;
		case var:        printf("[var] %s(%zu)\n", tree->op->word->name, tree->op->word->shadow_id); break;
		case bind:       printf("[bind] %s(%zu)\n", tree->op->word->name, tree->op->word->shadow_id); break;
		case to_any:     printf("[to_any] %s\n", c_val_type(tree->op->val_type)); break;
		case from_any:   printf("[from_any] %s\n", c_val_type(tree->op->val_type)); break;
		case pop:        printf("[pop]\n"); break;
		case push:       printf("[push]\n"); break;
		case pick:       printf("[pick]\n"); break;
		case unpick:     printf("[unpick]\n"); break;
		case define:     printf("[define] %s(%zu)\n", tree->op->word->name, tree->op->word->shadow_id); break;
		case drop:       printf("[drop]\n"); break;
		case branch:     printf("[branch]\n"); break;
		case none:       printf("[none]\n"); break;
		case load:       printf("[load]\n"); break;
		case ret:        printf("[ret]\n"); break;
		case static_call:printf("[static_call] %s (%zu args)\n", tree->op->func->name, tree->op->func->argc); break;
		case fn_branch:  printf("[fn_branch] %s (%zu args%s) %s (%zu args%s)\n", tree->op->funcs->func->name, tree->op->funcs->func->argc, tree->op->funcs->func->stack?" STACK":"", tree->op->funcs->next->func->name, tree->op->funcs->next->func->argc, tree->op->funcs->next->func->stack?" STACK":""); break;
		default:         printf("[INVALID %i]\n", tree->op->type); break;
	}
	print_ast(tree->next, i);
}

void add_generics(module_t* mod)
{
	for (func_list_t* f = mod->funcs ; f ; f = f->next)
	{
		if (f->func == mod->entry || f->func->generic)
		{
			f->func->generic_variant = NULL;
			continue;
		}
		ast_list_t* tree = alloc(sizeof *tree);
		tree->prev = NULL;
		tree->next = alloc(sizeof *(tree->next));
		tree->op = make_op(none, NULL, NULL);
		tree->next->op = make_op(none, NULL, NULL);
		tree->next->prev = tree;
		tree->next->next = NULL;
		insert_op_after(
				make_op(static_call, f->func, NULL), tree);
		char* name = make_func_name();
		func_t* f_ = make_func(tree, name);
		f_->generic = true;
		f_->captures = f->func->captures;
		f_->generic_variant = NULL;
		mod->funcs = push_func(f_, mod->funcs);
		f->func->generic_variant = f_;
	}
}

bool _determine_arguments(func_t* f)
{
	if (f->has_args) return 0;
	size_t argc = 0;
	bool returns = false;
	f->has_args = true;
	size_t registers = 0;
	bool changed = false;
	bool can_use_args = true;
	for (ast_list_t* n = f->ops ; n ; n = n->next)
	{
		ast_t* op = n->op;
		switch(op->type)
		{
			case closure:
			case literal:
			case var:
			case load:
				registers++;
				break;
			case fn_branch:
				{
					if (registers) registers--;
					else if (can_use_args && argc < 255 && !f->entry) argc++;
					for (func_list_t* f = op->funcs ; f ; f = f->next)
						changed |= _determine_arguments(f->func);
					int min_argc = INT_MAX;
					int stack = false;
					int returns = true;
				   for (func_list_t* ff = op->funcs ; ff ; ff = ff->next)
					{
						if (min_argc > ff->func->argc) min_argc = ff->func->argc;
						stack |= ff->func->stack;
						returns &= ff->func->returns;
					}
					if (stack && registers > min_argc)
						registers = min_argc;
					for (size_t i = 0 ; i < min_argc ; ++i)
					{
						if (registers) registers--;
						else if (can_use_args && argc < 255 && !f->entry) argc++;
					}
					if (stack) can_use_args = false;
					if (returns) registers++;
					break;
				}
			case branch:
				//printf("[ %s\n", f->name);
				for (int i = 0; i < 3; ++i)
				{
					if (registers)
					{
						//printf("REGS %li -> %li\n", registers, registers-1);
						registers--;
					}
					else if (can_use_args && (argc < 255) && !f->entry)
					{
						//printf("ARGC %li -> %li\n", argc, argc+1);
						argc++;
					}
				}
				registers++;
				break;
			case static_call:
			case call:
				{
					func_t* fn = static_call_to_func(op);
					if (fn != unknown_func) changed |= _determine_arguments(fn);
					if (fn->stack && registers > fn->argc) registers = fn->argc;
					for (size_t i = 0 ; i < fn->argc ; ++i)
					{
						if (registers) registers--;
						else if (can_use_args && argc < 255 && !f->entry) argc++;
					}
					if (fn->stack) can_use_args = false;
					if (fn->returns) registers++;
					break;
				}
			case bind:
				if (registers) registers--;
				else if (can_use_args && argc < 255 && !f->entry) argc++;
				break;
			case backtrace_push: case backtrace_pop: case define: case none: case pick: case unpick: break;
			case drop:
				if (registers) registers--;
				else if (can_use_args && argc < 255 && !f->entry) argc++;
				break;
			default: unreachable();
		}
		if (!n->next && registers && !f->entry)
		{
			returns = true;
			if (registers > 1) f->stack = true;
		}
	}
	changed |= argc != f->argc || returns != f->returns;
	f->argc = argc;
	f->returns = returns;
	return changed;
}

void _add_arguments(func_t* f)
{
	for (size_t i = 0 ; i < f->argc ; ++i)
	{
		insert_op_after(make_op(unpick, NULL, NULL), f->ops);
		insert_op_after(make_op(load, NULL, NULL), f->ops);
		f->args = push_val(make_value(any, f->ops), f->args);
	}
	if (f->returns)
	{
		ast_list_t* end = f->ops;
		while (end->next) end = end->next;
		insert_op_before(make_op(ret, NULL, NULL), end);
		f->rettype = any;
	}
	f->args = reverse(f->args);
}

void determine_arguments(module_t* mod)
{
	bool changed = 1;
	while (changed)
	{
		changed = false;
		for (func_list_t* f = mod->funcs ; f ; f = f->next)
			f->func->has_args = false;
		for (func_list_t* f = mod->funcs ; f ; f = f->next)
			changed |= _determine_arguments(f->func);
	}
}

void add_arguments(module_t* mod)
{
	for (func_list_t* f = mod->funcs ; f ; f = f->next)
		_add_arguments(f->func);
}

bool _determine_registers(func_t* f)
{
	if (f->has_regs) return false;
	f->has_regs = true;
	bool changed = false;
	bool old_stack = f->stack;
	size_t registers = 0;
	for (ast_list_t* n = f->ops ; n ; n = n->next)
	{
		ast_t* op = n->op;
		switch(op->type)
		{
			case closure:
			case literal:
			case var:
			case load:
			case pop:
				registers++;
				break;
			case fn_branch:
				{
					if (registers) registers--;
					else f->stack = true;
					func_t* v = op->funcs->func;
					for (func_list_t* f = op->funcs ; f ; f = f->next)
				   	changed |= _determine_registers(f->func);

					for (func_list_t* f = op->funcs ; f ; f = f->next)
						if (f->func->stack)
							for (func_list_t* f = op->funcs ; f ; f = f->next)
								f->func->stack = true;
					if (v->stack)
					{
						f->stack = true;
						for (size_t i = registers; i > (size_t)v->argc ; --i)
							registers--;
					}
					for (size_t i = 0 ; i < v->argc ; ++i)
					{
						if (registers) registers--;
						else f->stack = true;
					}
					if (v->stack) assert(!registers);
					if (v->returns) registers++;
					break;
				}
			case branch:
				for (char i = 0; i < 3; ++i)
				{
					if (registers) registers--;
					else f->stack = true;
				}
				registers++;
				break;
			case call:
			case static_call:
				{
					func_t* fn = static_call_to_func(op);
					if (fn != unknown_func)
						changed |= _determine_registers(fn);
					if (fn->stack)
					{
						f->stack = true;
						for (size_t i = registers; i > (size_t)fn->argc ; --i)
							registers--;
					}
					for (size_t i = 0 ; i < fn->argc ; ++i)
					{
						if (registers) registers--;
						else f->stack = true;
					}
					if (fn->returns) registers++;
					break;
				}
			case bind:
			case ret:
			case push:
				if (registers) registers--;
				else f->stack = true;
				break;
			case define: case none: case pick: case unpick: break;
			default: unreachable();
		}
		if (!n->next && registers)
			f->stack = true;
	}
	return changed || f->stack != old_stack;
}

void determine_registers(module_t* mod)
{
	// Honestly? idk if this pass is needed.
	bool changed = 1;
	while (changed)
	{
		changed = false;
		for (func_list_t* f = mod->funcs ; f ; f = f->next)
			f->func->has_regs = false;
		for (func_list_t* f = mod->funcs ; f ; f = f->next)
			changed |= _determine_registers(f->func);
	}
}


bool _add_registers(func_t* f)
{
	if (f->has_regs) return false;
	f->has_regs = true; // problem is stack usage in recursive branches?
	bool changed = false;
	size_t registers = 0;
	for (ast_list_t* n = f->ops ; n ; n = n->next)
	{
		ast_t* op = n->op;
		switch(op->type)
		{
			case backtrace_push:
			case backtrace_pop:
			case none:
			case define:
			case pick:
			case unpick:
			case to_any:
			case from_any:
				break;
			case closure:
			case literal:
			case var:
			case load:
			case pop:
				registers++;
				break;
			case fn_branch:
				{
					if (registers) registers--; // boolean arg
					else
					{
						insert_op_before(make_op(pop, NULL, op->where), n);
						insert_op_before(make_op(unpick, NULL, op->where), n);
						f->stack = true;
						changed = true;
					}

					func_t* v = op->funcs->func;

					for (func_list_t* f = op->funcs ; f ; f = f->next)
				   	changed |= _add_registers(f->func);

					for (func_list_t* f = op->funcs ; f ; f = f->next)
						if (f->func->stack)
						{
							for (func_list_t* f = op->funcs ; f ; f = f->next)
								f->func->stack = true;
							v->stack = true;
							break;
						}

					if (v->stack)
					{
						f->stack = true;
						while (registers > v->argc)
						{
							changed = true;
							insert_op_before(make_op(pick, NULL, op->where), n);
							insert_op_before(make_op(push, NULL, op->where), n);
							registers--;
						}
						assert(registers == v->argc);
					}

					for (size_t i = 0 ; i < v->argc ; ++i)
					{
						if (registers) registers--;
						else
						{
							insert_op_before(make_op(pop, NULL, op->where), n);
							insert_op_before(make_op(unpick, NULL, op->where), n);
							f->stack = true;
							changed = true;
						}
					}
					if (v->stack) assert(!registers);
					if (v->returns) registers++;
					break;
				}
			case branch:
				for (char i = 0; i < 3; ++i)
				{
					if (registers) registers--;
					else
					{
						insert_op_before(make_op(pop, NULL, op->where), n);
						insert_op_before(make_op(unpick, NULL, op->where), n);
						f->stack = true;
						changed = true;
					}
				}
				registers++;
				break;
			case call:
			case static_call:
				{
					func_t* fn = static_call_to_func(op);
					if (fn != unknown_func) changed |= _add_registers(fn);
					if (fn->stack)
					{
						f->stack = true;
						while (registers > fn->argc)
						{
							insert_op_before(make_op(pick, NULL, op->where), n);
							insert_op_before(make_op(push, NULL, op->where), n);
							registers--;
							changed = true;
						}
					}
					for (size_t i = 0 ; i < fn->argc ; ++i)
					{
						if (registers) registers--;
						else
						{
							insert_op_before(make_op(pop, NULL, op->where), n);
							insert_op_before(make_op(unpick, NULL, op->where), n);
							f->stack = true;
							changed = true;
						}
					}
					if (fn->stack) assert(!registers);
					if (fn->returns) registers++;
					break;
				}
			case bind:
			case ret:
			case drop:
			case push:
				if (registers) registers--;
				else
				{
					insert_op_before(make_op(pop, NULL, op->where), n);
					insert_op_before(make_op(unpick, NULL, op->where), n);
					f->stack = true;
					changed = true;
				}
				break;
			default: unreachable();
		}
		if (!n->next)
		{
			if (registers)
			{
				for (size_t i = 0 ; i < registers ; ++i)
				{
					insert_op_before(make_op(pick, NULL, op->where), n);
					insert_op_before(make_op(push, NULL, op->where), n);
				}
				f->stack = true;
				changed = true;
			}
		}
	}
	return changed;
}

void add_registers(module_t* mod)
{
	for (func_list_t* f = mod->funcs ; f ; f = f->next)
		f->func->has_regs = false;
	bool changed = true;
	while (changed)
	{
		changed = false;
		for (func_list_t* f = mod->funcs ; f ; f = f->next)
			changed |= _add_registers(f->func);
		for (func_list_t* f = mod->funcs ; f ; f = f->next)
			f->func->has_regs = false;
	}
}

bool add_var_types_forwards(module_t* mod)
{
	bool changed = 0;
	for (func_list_t* func = mod->funcs ; func ; func = func->next)
	{
		reg_dequeue_t* registers = make_register_dequeue();
		val_list_t* args = func->func->args;
		for (ast_list_t* op = func->func->ops ; op ; op = op->next)
		{
			switch(op->op->type)
			{
				case branch:
					{
						pop_register_front(registers);
						reg_t* ar2 = pop_register_front(registers);
						reg_t* ar3 = pop_register_front(registers);
						val_type_t t = ar2->type == ar3->type ? ar2->type : any;
						push_register_front(make_register(t, op), registers);
						break;
					}
				case fn_branch:
					{
						pop_register_front(registers);
						func_t* fn = op->op->funcs->func;
						func_t* fn2 = op->op->funcs->next->func;
						for ( val_list_t* v1 = fn->args, *v2=fn2->args ; v1 ; v1=v1->next, v2=v2->next )
						{
							// you thought the other one was bad???
							val_type_t t = pop_register_front(registers)->type;
							if (!fn->builtin && fn->unique && fn2->unique
									&& v1->val->type == any && t != any)
								// yay we're allowed to mess with it
							{
								v1->val->type = t; // kinda dodgy
								v2->val->type = t; // god this is gonna break something
								// Will this even help anything???
								changed = 1; // hmmm
							}
						}
						if (fn->stack) assert(registers->len == 0);
						if (fn->returns)
							push_register_front(
									make_register(fn->rettype, op),
									registers);
						break;
					}

				case literal:
					push_register_front(
							make_register(op->op->literal->type, op),
							registers);
					break;
				case var:
					push_register_front(
							make_register(op->op->word->val->type, op),
							registers);
					break;
				case load:
					push_register_front(
							make_register(args->val->type, op),
							registers);
					args = args->next;
					break;
				case pop:
					push_register_front(
							make_register(any, op),
							registers);
					break;
				case closure:
					push_register_front(
							make_register(block, op),
							registers);
					break;
				case ret:
					{
						val_type_t t = pop_register_front(registers)->type;
						if (!func->func->builtin && func->func->rettype != strong_any && t != any && func->func->rettype != t && !func->func->branch)
						{
							if (func->func->rettype != any)
								func->func->rettype = strong_any;
							else
								func->func->rettype = t;
							changed = 1;
						}
						break;
					}
				case drop:
				case push:
					pop_register_front(registers);
					break;
				case pick:
					push_register_front(
							pop_register_rear(registers),
							registers);
					break;
				case unpick:
					push_register_rear(
							pop_register_front(registers),
							registers);
					break;
				case call:
					op->op->word->val->type = block;
				case static_call:
					{
						func_t* fn = static_call_to_func(op->op);
						for ( val_list_t* v = fn->args ; v ; v = v->next )
						{
							val_type_t t = pop_register_front(registers)->type;
							if (!fn->builtin)
							{
								if (fn->unique && v->val->type == any && t != any && v->val->type != t)
								{
									v->val->type = t;
									changed = 1;
								}
								else if (v->val->type != any && t != any && t != v->val->type && v->val->type != strong_any)
								{
									v->val->type = strong_any;
									changed = 1;
								}
							}
						}
						if (fn->stack) assert(registers->len == 0);
						if (fn->returns)
							push_register_front(
									make_register(fn->rettype, op),
									registers);
						break;
					}
				case bind:
					{
						val_type_t t = pop_register_front(registers)->type;
						if (op->op->word->calltype == call)
						{
							if (t != block && t != any && t != strong_any)
					 			type_error(op->op->word->val->type, t, op->op->where);
						}
						else if (op->op->word->val->type != strong_any && t != any && op->op->word->val->type != t) // Early use needs to be bound to undefined symbol
						{
							if (op->op->word->val->type != any && op->op->word->val->type != strong_any)
							{
								op->op->word->val->type = strong_any;
								changed = 1;
							}
							else if (op->op->word->val->type == any)
							{
								op->op->word->val->type = t;
								changed = 1;
							}
						}
						break;
					}
				default: break;
			}
		}
	}
	return changed;
}
bool add_var_types_backwards(module_t* mod)
{

	bool changed = 0;
	for (func_list_t* ff = mod->funcs ; ff ; ff = ff->next)
		ff->func->tentative_rettype = ff->func->rettype;
	for (func_list_t* func = mod->funcs ; func ; func = func->next)
	{
		ast_list_t* op = func->func->ops;
		for (; op->next ; op = op->next);
		reg_dequeue_t* registers = make_register_dequeue();
		val_list_t* args = NULL;
		for (; op ; op = op->prev)
		{
			switch(op->op->type)
			{
				case branch:
					{
						pop_register_front(registers);
						push_register_front(make_register(any, op), registers);
						push_register_front(make_register(any, op), registers);
						push_register_front(make_register(boolean, op), registers);
						// branch may be typelevel dispatch and thus we don't know that both inputs have the same type as the output.
						break;
					}
				case fn_branch:
					{
						// hopefully balanced
						func_t* fn = op->op->funcs->func;
						if (fn->returns)
						{
							reg_t* r = pop_register_front(registers);
							if (r->type != any)
								for (func_list_t* ff = op->op->funcs ; ff ; ff = ff->next)
								{
									if (!ff->func->builtin)
									{
										if (ff->func->tentative_rettype != strong_any && ff->func->tentative_rettype != r->type)
										{
											if (ff->func->tentative_rettype != any)
												ff->func->tentative_rettype = strong_any;
											else
												ff->func->tentative_rettype = r->type;
											changed = 1;
										}
									}
								}
						}
						if (fn->stack) assert(registers->len == 0);
						val_list_t* reversed_args = reverse(fn->args);
						for ( val_list_t* a = reversed_args ; a ; a = a->next )
							push_register_front(make_register(a->val->type, op), registers);
						push_register_front(make_register(boolean, op), registers);
						break;
					}
				case var:
					{
						val_type_t t = pop_register_front(registers)->type;
						if (op->op->word->val->type != strong_any && t != any && op->op->word->val->type != t)
						{
							if (op->op->word->val->type != any)
								op->op->word->val->type = strong_any;
							else
								op->op->word->val->type = t;
							changed = true;
						}
					}
					break;
				case load:
					args = push_val(make_value(pop_register_front(registers)->type, op), args);
					break;
				case literal:
				case pop:
				case closure:
					pop_register_front(registers);
					break;
				case drop:
				case push:
					push_register_front(
							make_register(any, op),
							registers);
					break;
				case ret:
					push_register_front(
							make_register(func->func->rettype, op),
							registers);
					break;
				case pick:
					push_register_rear(
							pop_register_front(registers),
							registers);
					break;
				case unpick:
					push_register_front(
							pop_register_rear(registers),
							registers);
					break;
				case call:
				case static_call:
					{
						func_t* fn = static_call_to_func(op->op);
						if (fn->returns)
						{
							reg_t* r = pop_register_front(registers);
							if (!fn->builtin && fn->tentative_rettype != strong_any && fn->tentative_rettype != r->type && r->type != any)
							{
								if (fn->tentative_rettype != any)
									fn->tentative_rettype = strong_any;
								else
									fn->tentative_rettype = r->type;
								changed = 1;
							}
						}
						if (fn->stack) assert(registers->len == 0);
						val_list_t* reversed_args = reverse(fn->args);
						for ( val_list_t* a = reversed_args ; a ; a = a->next )
							push_register_front(make_register(a->val->type, op), registers);
						break;
					}
				case bind:
					push_register_front(make_register(op->op->word->val->type, op), registers);
					break;
				case none:
				case define:
				case backtrace_push:
				case backtrace_pop:
					break;
				default: unreachable();
			}

		}
		for (val_list_t* v = func->func->args; v ; v = v->next)
		{
			val_type_t t = args->val->type;
			if (!func->func->builtin && v->val->type != strong_any && t != any && v->val->type != t && !func->func->branch)
			{
				if (v->val->type != any)
					v->val->type = strong_any;
				else
					v->val->type = t;
				changed = 1;
			}
			args = args->next;
		}

	}
	for (func_list_t* ff = mod->funcs ; ff ; ff = ff->next)
		ff->func->rettype = ff->func->tentative_rettype;
	return changed;
}

void add_var_types(module_t* mod)
{
	bool changed = 1;
	while (changed)
	{
		changed = 0;
		changed |= add_var_types_forwards(mod);
		changed |= add_var_types_backwards(mod);
	}

	for (func_list_t* func = mod->funcs ; func ; func = func->next)
	{
		for (word_list_t* w = func->func->locals ; w ; w = w->next)
			if (w->word->val->type == strong_any) w->word->val->type = any;

		for (word_list_t* w = func->func->captures ; w ; w = w->next)
			if (w->word->val->type == strong_any) w->word->val->type = any;

		for (val_list_t* v = func->func->args ; v ; v = v->next)
			if (v->val->type == strong_any) v->val->type = any;

		if (func->func->rettype == strong_any)
			func->func->rettype = any;

		if (func->func->generic_variant) for (word_list_t* w = func->func->generic_variant->locals ; w ; w = w->next)
			if (w->word->val->type == strong_any) w->word->val->type = any;

		if (func->func->generic_variant) for (word_list_t* w = func->func->generic_variant->captures ; w ; w = w->next)
			if (w->word->val->type == strong_any) w->word->val->type = any;
	}
}

void add_typechecks(module_t* mod)
{
	// Simple single direction typechecking for now.
	for (func_list_t* func = mod->funcs ; func ; func = func->next)
	{
		reg_dequeue_t* registers = make_register_dequeue();
		val_list_t* args = func->func->args;
		for (ast_list_t* op = func->func->ops ; op ; op = op->next)
		{
			switch (op->op->type)
			{
				case branch:
					{
						reg_t* ar1 = pop_register_front(registers);
						reg_t* ar2 = pop_register_front(registers);
						reg_t* ar3 = pop_register_front(registers);
						if (ar1->type == boolean) {}
						else if (ar1->type == any)
						{
							insert_op_before(make_op(from_any, (void*)boolean, op->op->where), op);
						} else type_error(boolean, ar1->type, op->op->where);
						insert_op_before(make_op(unpick, NULL, op->op->where), op);
						val_type_t res = any;
						if (ar2->type == ar3->type)
						{
							res = ar2->type;
							insert_op_before(make_op(unpick, NULL, op->op->where), op);
						}
						else if (ar2->type == any)
						{
							insert_op_before(make_op(unpick, NULL, op->op->where), op);
							insert_op_before(make_op(to_any, (void*)(uintptr_t)ar3->type, op->op->where), op);
						}
						else if (ar3->type == any)
						{
							insert_op_before(make_op(to_any, (void*)(uintptr_t)ar2->type, op->op->where), op);
							insert_op_before(make_op(unpick, NULL, op->op->where), op);
						}
						else
						{
							insert_op_before(make_op(to_any, (void*)(uintptr_t)ar2->type, op->op->where), op);
							insert_op_before(make_op(unpick, NULL, op->op->where), op);
							insert_op_before(make_op(to_any, (void*)(uintptr_t)ar3->type, op->op->where), op);
						}
						insert_op_before(make_op(pick, NULL, op->op->where), op);
						insert_op_before(make_op(pick, NULL, op->op->where), op);
						push_register_front(make_register(res, op), registers);
					}
					break;
				case literal:
					push_register_front(make_register(op->op->literal->type, op), registers);
					break;
				case var:
					push_register_front(make_register(op->op->word->val->type, op), registers);
					break;
				case load:
					push_register_front(make_register(args->val->type, op), registers);
					args = args->next;
					break;
				case pop:
					push_register_front(make_register(any, op), registers);
					break;
				case closure:
					push_register_front(make_register(block, op), registers);
					break;
				case drop:
					pop_register_front(registers);
					break;
				case push:
					{
						reg_t* reg = pop_register_front(registers);
						if (reg->type != any)
							insert_op_before(make_op(to_any, (void*)(uintptr_t)reg->type, op->op->where), op);
						break;
					}
				case pick:
					{
						reg_t* r = pop_register_rear(registers);
						push_register_front(r, registers);
						break;
					}
				case unpick:
					{
						reg_t* r = pop_register_front(registers);
						push_register_rear(r, registers);
						break;
					}
				case fn_branch:
					{
						reg_t* b = pop_register_front(registers);
						if (b->type == boolean) {}
						else if (b->type == any)
						{
							insert_op_before(make_op(from_any, (void*)(uintptr_t)boolean, op->op->where), op);
						} else type_error(boolean, b->type, op->op->where);
						func_t* fn = op->op->funcs->func;
						//func_t* fn2 = op->op->funcs->next->func;
						//size_t i = 0;
						//for ( val_list_t *a = fn->args, *a2=fn2->args ; a ; a = a->next, a2=a2->next )
						for ( val_list_t *a = fn->args ; a ; a = a->next )
						{
							reg_t* reg = pop_register_front(registers);
							val_type_t expected = a->val->type;
							val_type_t got = reg->type;
							//if (a->val->type != a2->val->type) expected = any; // hmmm
							insert_op_before(make_op(unpick, NULL, op->op->where), op);
							if (expected == got) { }
							else if (expected == any)
								insert_op_before(make_op(to_any, (void*)(uintptr_t)got, op->op->where), op);
							else if (got == any)
								insert_op_before(make_op(from_any, (void*)(uintptr_t)expected, op->op->where), op);
							else type_error(expected, got, op->op->where);
						}
						for ( int i = 0 ; i < fn->argc ; ++i )
							insert_op_before(make_op(pick, NULL, op->op->where), op);
						if (fn->returns)
							push_register_front(make_register(fn->rettype, op), registers);
						break;

					}
				case call:
				case static_call:
					{
						func_t* fn = static_call_to_func(op->op);
						/*
						 * TODO perhaps this could work in some ideal world.
						 * doubtful tho
						if (fn->checks)
						{
							// typecheck function
							reg_t* r = pop_register_front(registers);
							if (r->type != any)
							{
								op->op->type = literal;
								op->op->literal = alloc(sizeof *op->op->literal);
								op->op->literal->type = boolean;
								op->op->literal->string = r->type==fn->checks?"1":"0";
								insert_op_before(make_op(drop, NULL, op->op->where), op);
								push_register_front(make_register(boolean, op), registers);
								break;
							}
							else
								push_register_front(r, registers);
						}
						*/
						for ( val_list_t* a = fn->args ; a ; a = a->next )
						{
							reg_t* reg = pop_register_front(registers);
							val_type_t expected = a->val->type;
							val_type_t got = reg->type;
							if (expected == got) { }
							else if (expected == any)
								insert_op_before(make_op(to_any, (void*)(uintptr_t)got, op->op->where), op);
							else if (got == any)
								insert_op_before(make_op(from_any, (void*)(uintptr_t)expected, op->op->where), op);
							else type_error(expected, got, op->op->where);
							insert_op_before(make_op(unpick, NULL, op->op->where), op);
						}
						for ( size_t i = 0 ; i < fn->argc ; ++i )
							insert_op_before(make_op(pick, NULL, op->op->where), op);
						if (fn->returns)
							push_register_front(make_register(fn->rettype, op), registers);
						break;
					}
				case ret:
					{
						reg_t* reg1 = pop_register_front(registers);
						val_type_t expected = func->func->rettype;
						val_type_t got = reg1->type;
						if (expected == got) { }
						else if (expected == any)
							insert_op_before(make_op(to_any, (void*)(uintptr_t)got, op->op->where), op);
						else if (got == any)
							insert_op_before(make_op(from_any, (void*)(uintptr_t)expected, op->op->where), op);
						else type_error(expected, got, op->op->where);
						break;
					}
				case bind:
					{
						reg_t* reg1 = pop_register_front(registers);
						val_type_t expected = op->op->word->val->type;
						val_type_t got = reg1->type;
						if (expected == got) { }
						else if (expected == any)
							insert_op_before(make_op(to_any, (void*)(uintptr_t)got, op->op->where), op);
						else if (got == any)
							insert_op_before(make_op(from_any, (void*)(uintptr_t)expected, op->op->where), op);
						else type_error(expected, got, op->op->where);
						break;
					}
				case none: break;
				case define: break;
				case backtrace_push:
				case backtrace_pop:
					break;
				default: unreachable();
			}
		}
	}
}

char* lowercase(const char* S)
{
	char* s = strdup(S);
	for (char* i = s; *i; ++i) *i = tolower(*i);
	return s;
}

void print_funcs (module_t* mod)
{
	for (func_list_t* f = mod->funcs ; f ; f = f->next)
	{
		printf("======== %s ========%s\n", f->func->name, f->func->stack?" (STACK)":"");
		printf("( ");
		for (word_list_t* w = f->func->captures ; w ; w = w->next)
			printf("%s ", w->word->name);
		printf(")\n");
		if (f->func->stack) printf("NEEDS STACK\n");
		print_ast(f->func->ops, 0);
	}
}

void* assoc_push(void* from, void* to, ptr_assoc_t* assoc)
{
	ptr_assoc_t* new = alloc(sizeof *new);
	new->next = assoc;
	new->from = from;
	new->to = to;
	return new;
}

void* ptr_assoc_lookup(void* ptr, ptr_assoc_t* assoc)
{
	for (ptr_assoc_t* p = assoc ; p ; p = p->next)
	{
		if (p->from == ptr)
		{
			return p->to;
		}
	}
	return NULL;
}

bool does_call(func_t* target, ast_list_t* start)
{
	for (ast_list_t* node = start ; node ; node = node->next)
	{
		if (node->op->type == call && call_to_func(node->op) == target)
			return true;
		else if (node->op->type == closure
				&& does_call(target, node->op->func->ops))
			return true;
	}
	return false;
}

ast_list_t* clone_func(ast_list_t* ops, ptr_assoc_t* assoc, func_list_t** funcs)
{
	ast_list_t* l = alloc (sizeof *l);
	l->op = alloc(sizeof *(l->op));
	l->op->type = none;
	l->prev = NULL;
	ast_list_t* prev = l;
	for (ast_list_t* op = ops->next ; op ; op = op->next)
	{
		ast_list_t* node = alloc(sizeof *node);
		node->next = NULL;
		node->prev = prev;
		prev->next = node;
		prev = node;
		switch (op->op->type)
		{
			case define:
				{
					word_t* new_word =
						make_word(
							op->op->word->name,
							op->op->word->calltype,
							make_value(op->op->word->val->type,
								ptr_assoc_lookup(op->op->word->val->source,
									assoc)), op->op->word->mod);
					assoc = assoc_push(op->op->word, new_word, assoc);
					node->op = make_op(define, new_word, op->op->where);
					break;
				}
			case bind:
			case var:
			case call:
				{
					word_t* new = ptr_assoc_lookup(op->op->word, assoc);
					if (!new)
					{
						node->op = make_op(op->op->type, op->op->word, op->op->where);
						break;
					}
					node->op = make_op(op->op->type, new, op->op->where);
					break;
				}
			case closure:
				{
					func_t* fn = make_func(
							clone_func(op->op->func->ops, assoc, funcs),
							make_func_name());
					*funcs = push_func(fn, *funcs);
					node->op = make_op(closure, fn, op->op->where);
					break;
				}
			default:
				node->op = op->op;
				node->op = make_op(op->op->type, op->op->data, op->op->where);
				break;
		}
		assoc = assoc_push(op, node, assoc);
	}
	return l;
}

void _compute_sources(ast_list_t* a)
{
	/*
	 * TODO
	 * We need a souped-up version of this functions
	 * shorten_references() is just better, but needs more context
	 * at this point we have to assume all functions use the stack
	 */
	reg_dequeue_t* regs = make_register_dequeue();
	for (ast_list_t* node = a ; node ; node = node->next)
	{
		switch (node->op->type)
		{
			case closure: _compute_sources(node->op->func->ops);
			case literal:
				{
					reg_t* r = make_register(any, node);
					push_register_front(r, regs);
				}
				break;
			case var:
				{
					reg_t* r = make_register(any, node->op->word->val->source);
					push_register_front(r, regs);
					// TODO shorten references
				}
				break;
			case bind:
				{
					reg_t* r = pop_register_front(regs);
					if (r && r->source)
					{
						if (r->source->op->type == closure)
							r->source->op->func->unmangled_name = node->op->word->name;
						node->op->word->val->source = r->source;
					}
					break;
				}
			case call:
				{
					func_t* fn = call_to_func(node->op);
					for (size_t i = 0 ; i < fn->argc ; ++i)
						pop_register_front(regs);
					// assume all functions are stack
					clear_registers(regs);
					if (fn->returns)
						push_register_front(make_register(any, node), regs);
				}
				break;
			case branch:
				for (int i = 0 ; i < 3 ; ++i) pop_register_front(regs);
				push_register_front(make_register(any, node), regs);
				break;
			case fn_branch:
				clear_registers(regs);
				break;
			case none:
			case define:
			case backtrace_push:
			case backtrace_pop:
				break;
			case drop:
				pop_register_front(regs);
				break;
			default: unreachable();
		}
	}
}

void compute_sources(module_t* m)
{
	_compute_sources(m->entry->ops);
}

void _inline_functions(func_t* f, func_list_t** funcs)
{
	// TODO: we can do better
	for (bool changed = 1 ; changed ;)
	{
		changed = 0;
		_compute_sources(f->ops);
		for (ast_list_t* node = f->ops ; node ; node = node->next)
		{
			switch (node->op->type)
			{
				case closure:
					_inline_functions(node->op->func, funcs);
					break;
				case call:
					{
						func_t* fn = call_to_func(node->op);
						if (fn != unknown_func)
						{
							if (!fn->ops) break;
							//if (node->op->sqnum <= f->ops->op->sqnum) break;
							size_t n = 0;
							for (ast_list_t* inl = fn->ops ; inl ; inl = inl->next) n++;
							if (n > 30) break;
							if (does_call(fn, fn->ops)) break;
							changed = 1;
							ast_list_t* inl = clone_func(fn->ops, NULL, funcs);
							for ( ; inl ; inl = inl->next)
								if (inl->op->type != none) insert_op_before(inl->op, node);
							remove_op(node);
						}
					}
				default: break;
			}
		}
	}
}

void inline_functions(module_t* m)
{
	_inline_functions(m->entry, &m->funcs);
}

bool _compute_stack(func_t* f)
{
	if (f->has_stack) return f->stack;
	f->has_stack = true;
	for (ast_list_t* a = f->ops ; a ; a = a->next)
	{
		switch (a->op->type)
		{
			default: break;
			case call:
			case static_call:
				{
					func_t* called = static_call_to_func(a->op);
					if (_compute_stack(called))
					{
						f->stack = true;
						return true;
					}
				}
				break;
			case fn_branch:
				{
					for (func_list_t* F = a->op->funcs ; F ; F = F->next)
						if (_compute_stack(F->func))
						{
							f->stack = true;
							return true;
						}
				}
				break;
			case push:
			case pop:
				f->stack = true;
				return true;
		}
	}
	return false;
}

void _assign_sequence_numbers(func_t* f, size_t sqnum)
{
	for ( ast_list_t* a = f->ops ; a ; a = a->next )
	{
		a->op->sqnum = sqnum++;
		if (a->op->type == closure)
			_assign_sequence_numbers(a->op->func, sqnum);
	}
}

void assign_sequence_numbers(module_t* m)
{
	_assign_sequence_numbers(m->entry, 0);
}

void compute_stack(module_t* m)
{
	for (func_list_t* f = m->funcs ; f ; f = f->next)
		f->func->stack = false, f->func->has_stack = false;
	for (func_list_t* f = m->funcs ; f ; f = f->next)
		_compute_stack(f->func);
}

void compute_locals(func_t* f)
{
	f->locals = NULL;
	for (ast_list_t* a = f->ops ; a ; a = a->next)
		if (a->op->type == define)
			f->locals = push_word(a->op->word, f->locals);
}

void compute_captures(func_t* f, word_list_t** ret, word_list_t* kinda_locals, func_list_t** tried)
{
	for (ast_list_t* a = f->ops ; a ; a = a->next)
		if (a->op->type == define)
			kinda_locals = push_word(a->op->word, kinda_locals);
	for (ast_list_t* a = f->ops ; a ; a = a->next)
	{
		switch (a->op->type)
		{
			default: break;
			case static_call:
				{
					func_t* called = a->op->func;
					for (func_list_t* r = *tried ; r ; r=r->next)
						if (called == r->func) goto end4;
					*tried = push_func(called, *tried);
					if (called != f) compute_captures(called, ret, kinda_locals, tried);
end4:;
					break;
				}
			case fn_branch:
				{
					for (func_list_t* called = a->op->funcs ; called ; called = called->next)
					{
						for (func_list_t* r = *tried ; r ; r=r->next)
							if (called->func == r->func) goto end3;
						*tried = push_func(called->func, *tried);
						if (called->func != f)
							compute_captures(called->func, ret, kinda_locals, tried);
end3:;
					}
					break;
				}
			case call:
			case var:
				for (word_list_t* r = *ret ; r ; r=r->next)
					if (r->word == a->op->word) goto end;
				for (word_list_t* r = kinda_locals ; r ; r=r->next)
					if (r->word == a->op->word) goto end;
				*ret = push_word(a->op->word, *ret);
				end:
				break;
			case closure:
				for (func_list_t* r = *tried ; r ; r=r->next)
					if (a->op->func == r->func) goto end4;
				*tried = push_func(a->op->func, *tried);
				if (a->op->func != f) compute_captures(a->op->func, ret, kinda_locals, tried);
				break;
		}
	}
}

bool remove_unused_vars(func_t* f)
{
	bool changed = 0;
	reg_dequeue_t* registers = make_register_dequeue();
	for (ast_list_t* node = f->ops ; node ; node = node->next)
	{
		switch (node->op->type)
		{
			case branch:
				pop_register_front(registers);
				pop_register_front(registers);
				pop_register_front(registers);
				push_register_front(make_register(any, NULL), registers);
				break;
			case var:
			case literal:
			case closure:
				push_register_front(make_register(any, node), registers);
				break;
			case pop:
			case load:
				push_register_front(make_register(any, NULL), registers);
				break;
			case push:
			case ret:
				pop_register_front(registers);
				break;
			case call:
			case static_call:
				{
					func_t* fn = node->op->func;
					if (fn->stack)
						for (size_t i = registers->len; i > fn->argc ; --i)
							pop_register_front(registers);
					else for (size_t i = 0 ; i < fn->argc && registers->len ; ++i)
						pop_register_front(registers);
					if (fn->returns)
						push_register_front(make_register(fn->rettype, NULL), registers);
					break;
				}
			case define:
				if (!node->op->word->used)
				{
					remove_op(node);
					changed = true;
				}
				break;
			case bind:
				{
					reg_t* r = pop_register_front(registers);
					if (!node->op->word->used)
					{
						if (r->source)
						{
							remove_op(node);
							remove_op(r->source);
						} else node->op->type = drop;
						changed = true;
					}
					break;
				}
			default: break;
		}
	}
	return changed;
}

void mark_all_unused(module_t* m)
{
	for (func_list_t* f = m->funcs ; f ; f = f->next)
	{
		for (ast_list_t* a = f->func->ops ; a ; a = a->next)
		{
			if (a->op->type == define) a->op->word->used = false;
		}
	}
}

void mark_used_vars(module_t* m)
{
	for (func_list_t* f = m->funcs ; f ; f = f->next)
	{
		for (ast_list_t* a = f->func->ops ; a ; a = a->next)
		{
			switch (a->op->type)
			{
				case call:
				case var:
					a->op->word->used = true;
					break;
				default: break;
			}
		}
	}
}

void remove_defines(module_t* m)
{
	for (func_list_t* f = m->funcs ; f ; f = f->next)
	{
		for (ast_list_t* a = f->func->ops ; a ; a = a->next)
		{
			if (a->op->type == define) remove_op(a);
		}
	}
}

void compute_variables(module_t* m)
{
	bool changed = 1;
	while (changed)
	{
		changed = 0;
		mark_all_unused(m);
		mark_used_vars(m);
		for (func_list_t* f = m->funcs ; f ; f = f->next)
			changed |= remove_unused_vars(f->func);
		for (func_list_t* f = m->funcs ; f ; f = f->next)
		{
			f->func->captures = NULL;
			f->func->locals = NULL;
			f->func->calls = NULL;
		}
		for (func_list_t* f = m->funcs ; f ; f = f->next)
			compute_locals(f->func);
		for (func_list_t* f = m->funcs ; f ; f = f->next)
		{
			func_list_t* _ = NULL;
			compute_captures(f->func, &f->func->captures, NULL, &_);
		}
	}
	remove_defines(m); // why did we even have those???
}

void mark_used_funcs(func_t* f)
{
	if (f->used) return;
	f->used = true;
	for (ast_list_t* n = f->ops ; n ; n = n->next)
	{
		if (n->op->type == static_call)
			mark_used_funcs(n->op->func);

		if (n->op->type == closure)
			mark_used_funcs(n->op->func->generic_variant);

		if (n->op->type == fn_branch)
		{
			for (func_list_t* f = n->op->funcs ; f ; f = f->next)
				mark_used_funcs(f->func);
		}
	}
}

void remove_unused_funcs(module_t* m)
{
	for (func_list_t* f = m->funcs ; f ; f = f->next)
		f->func->used = false;

	mark_used_funcs(m->entry);

	func_list_t** prevnext = &m->funcs;
	for (func_list_t* f = m->funcs ; f ; f = f->next)
	{
		if (!f->func->used) *prevnext = f->next;
		else prevnext = &f->next;
	}
}

void _add_backlinks(ast_list_t** t)
{
	ast_list_t* b1 = ast_single(none, NULL, NULL);
	b1->prev = NULL;
	b1->next = *t;
	// add prev pointers and buffer elements
	ast_list_t* prev = b1;
	ast_list_t* a = *t;
	for (; a ; a = a->next)
	{
		a->prev = prev;
		prev = prev->next;
		if (a->op->type == braces) _add_backlinks(&a->op->child);
	}
	ast_list_t* b2 = ast_single(none, NULL, NULL);
	prev->next = b2;
	b2->prev = prev;
	b2->next = NULL;
	*t = b1;
}

void add_backlinks(module_t* m)
{
	_add_backlinks(&m->tree);
}

void inline_values(module_t* m)
{
	for (func_list_t* f = m->funcs ; f ; f = f->next)
	{
		for (ast_list_t* a = f->func->ops ; a ; a = a->next)
		{
			if (a->op->type == var
					&& a->op->word->val->source
					&& a->op->word->val->source->op->type == literal)
				a->op = a->op->word->val->source->op;
		}
	}
}

void static_calls(module_t* m)
{
	for (func_list_t* f = m->funcs ; f ; f = f->next)
	{
		for (ast_list_t* a = f->func->ops ; a ; a = a->next)
		{
			if (a->op->type == call)
			{
				func_t* called = call_to_func(a->op);
				if (called == unknown_func) continue;
				a->op->func->unmangled_name = a->op->word->name;
				a->op->type = static_call;
				a->op->func = called;
			}
		}
	}
}

ast_list_t* make_astlist(void)
{
	ast_list_t* a = alloc(sizeof *a);
	ast_list_t* b = alloc(sizeof *b);
	ast_t* A = alloc(sizeof *A);
	ast_t* B = alloc(sizeof *B);
	A->type = none;
	B->type = none;
	a->op = A;
	b->op = B;
	a->next = b;
	b->prev = a;
	a->prev = NULL;
	b->next = NULL;
	return a;
}

void static_branches(module_t* m)
{
	for (func_list_t* f = m->funcs ; f ; f = f->next)
	{
		reg_dequeue_t* regs = make_register_dequeue();
		for (ast_list_t* a = f->func->ops ; a ; a = a->next)
		{
			switch (a->op->type)
			{
				default: unreachable();
				case none:
				case define:
				case backtrace_push:
				case backtrace_pop:
					break;
				case pick:
					push_register_front(pop_register_rear(regs), regs);
					break;
				case unpick:
					push_register_rear(pop_register_front(regs), regs);
					break;
				case to_any:
				case from_any:
					pop_register_front(regs);
				case literal:
				case closure:
				case pop:
				case load:
				case var:
					push_register_front(make_register(any, a), regs);
					break;
				case branch:
					{
						pop_register_front(regs); // bool
						reg_t* i1 = pop_register_front(regs);
						reg_t* i2 = pop_register_front(regs);
						push_register_front(make_register(any, a), regs);
						//printf("%p %p\n", i1, i2);
						if (!i1 || !i2) break;
						ast_list_t* s1 = i1->source;
						ast_list_t* s2 = i2->source;
						while (s1 && s1->op->type == var) s1 = s1->op->word->val->source;
						while (s2 && s2->op->type == var) s2 = s2->op->word->val->source;
						if (!s1 || !s2 || s1->op->type != closure || s2->op->type != closure) break;
						//if (s1->op->sqnum > a->op->sqnum || s2->op->sqnum > a->op->sqnum) break; // ???
						func_list_t* fl
							= push_func(s1->op->func,
									push_func(s2->op->func,
										NULL));
						ast_list_t* op = make_astlist();
						word_t* b = make_word("", var, NULL, NULL);
						insert_op_after(make_op(fn_branch, fl, a->op->where), op);
						insert_op_after(make_op(var, b, a->op->where), op);
						func_t* F = make_func(op, make_func_name());
						a->op = make_op(closure, F, a->op->where);
						insert_op_before(make_op(define, b, a->op->where), a);
						insert_op_before(make_op(bind, b, a->op->where), a);
						b->val = make_value(any, a->prev);
						m->funcs = push_func(F, m->funcs);
						assert(i1->source->op->type == var || i1->source->op->type == closure);
						assert(i2->source->op->type == var || i2->source->op->type == closure);
						remove_op(i1->source);
						remove_op(i2->source);
						break;
					}
				case call:
				case static_call:
					{
						func_t* f = call_to_func(a->op);
						for ( size_t i = 0 ; i < f->argc ; ++i )
							pop_register_front(regs);
						if (f->stack) clear_registers(regs);
						if (f->returns)
						push_register_front(make_register(f->rettype, a), regs);
					}
					break;
				case bind:
					{
						reg_t* r = pop_register_front(regs);
						if (r && r->source)
						{
							//a->op->word->val->source = r->source;
						}
						break;
					}
				case push:
				case ret:
				case drop:
					pop_register_front(regs);
					break;
			}
		}
	}
}

void merge_symbols(module_t* m)
{
	for (func_list_t* funcs = m->funcs ; funcs ; funcs = funcs->next)
	{
		for (ast_list_t* ast = funcs->func->ops ; ast ; ast = ast->next)
		{
			if (ast->op->type == literal && ast->op->literal->type == symbol)
			{
				for (symbol_list_t* s = m->symbols ; s ; s = s->next)
				{
					if (!strcmp(s->text, ast->op->literal->string)) goto next;
				}
				symbol_list_t* S = alloc(sizeof *S);
				S->text = (char*)ast->op->literal->string;
				S->next = m->symbols;
				m->symbols = S;
			}
next:;
		}
	}
}

void _catch_shadows(ast_list_t* tree, module_t* m)
{
	word_list_t* defined = NULL;
	for (ast_list_t* a = tree ; a ; a = a->next)
	{
		if (a->op->type == let || a->op->type == def)
		{
			for (word_list_t* w = defined ; w ; w = w->next)
			{
				if (!strcmp(w->word->name, a->op->string) && w->word->mod == a->op->where->mod)
					throw_error("cannot shadow\nin same block", a->op->where);
			}
			word_t* W = make_word(a->op->string, a->op->type, NULL, m);
			defined = push_word(W, defined);
		}
		else if (a->op->type == braces) _catch_shadows(a->op->child, m);
	}
}

void catch_shadows(module_t* m)
{
	_catch_shadows(m->tree, m);
}

bool consistent(func_t* f1, func_t* f2)
{
	// essentially we can only form a static if when they have the same argc, same arg types, same return types, etc
	// TODO different types can be handled if we generate adaptor functions but effort
	if (f1->argc != f2->argc || f1->returns != f2->returns) return false;
	if (f1->returns && f1->rettype != f2->rettype) return false;
	if (f1->stack != f2->stack) return false;
	for (val_list_t *v1 = f1->args, *v2 = f2->args ; v1 ; v1 = v1->next, v2 = v2->next)
	{
		if (v1->val->type != v2->val->type) return false;
	}
	return true;
}

void balance_branches(module_t* m)
{
	for (func_list_t* funcs = m->funcs ; funcs ; funcs = funcs->next)
	{
		for (ast_list_t* ast = funcs->func->ops ; ast ; ast = ast->next)
		{
			ast_t* op = ast->op;
			switch (op->type)
			{
				case fn_branch:
					{
						int min_argc = INT_MAX;
						int max_argc = 0;
						bool all_return  = true;
						bool some_return = false;
						for (func_list_t* fns = op->funcs ; fns ; fns = fns->next)
						{
							if (fns->func->argc < min_argc) min_argc = fns->func->argc;
							if (fns->func->argc > max_argc) max_argc = fns->func->argc;
							all_return  &= fns->func->returns;
							some_return |= fns->func->returns;
						}

						if (all_return == some_return
								&& min_argc == max_argc) goto end;

						val_list_t* adapt_args = NULL;
						for (int i = 0 ; i < min_argc ; ++i)
							adapt_args = push_val(make_value(any, NULL), adapt_args);
						// generate adaptor functions.
						for (func_list_t* fns = op->funcs ; fns ; fns = fns->next)
						{
							ast_list_t* body = make_astlist();
							func_t* adaptor = make_func(body, make_func_name());
							adaptor->argc = min_argc;
							adaptor->returns = all_return;
							adaptor->rettype = any;
							adaptor->args = adapt_args;

							if (adaptor->returns)
								insert_op_after(make_op(ret, NULL, op->where), body);
							/*
							else if (fns->func->returns)
								insert_op_after(make_op(push, NULL), body);
								*/

							insert_op_after(make_op(static_call, fns->func, op->where), body);

							/*
							int remaining = fns->func->argc - adaptor->argc;
							for (int i = 0 ; i < remaining ; ++i)
							{
								insert_op_after(make_op(unpick, NULL), body);
								insert_op_after(make_op(pop, NULL), body);
							}
							*/

							for (val_list_t* v = adaptor->args ; v ; v = v->next)
							{
								insert_op_after(make_op(unpick, NULL, op->where), body);
								insert_op_after(make_op(load, NULL, op->where), body);
							}

							m->funcs = push_func(adaptor, m->funcs);
							fns->func = adaptor;
						}
end:
						for (func_list_t* fns = op->funcs ; fns ; fns = fns->next)
							fns->func->branch = true;
					}
					break;
				default: break;
			}
		}
	}
}

void determine_unique_calls(module_t* m)
{
	func_list_t* once = NULL;
	func_list_t* twice = NULL;

	for (func_list_t* f = m->funcs ; f ; f = f->next)
	{
		f->func->unique = true;
		for (ast_list_t* a = f->func->ops ; a ; a = a->next)
		{
			switch (a->op->type)
			{
				case static_call:
					for (func_list_t* o = once ; o ; o = o->next)
						if (o->func == a->op->func)
						{
							twice = push_func(a->op->func, twice);
							goto end;
						}
					for (func_list_t* t = twice ; t ; t = t->next)
						if (t->func == a->op->func) goto end;
					once = push_func(a->op->func, once);
end:;
					break;
				case fn_branch:
					break;
				default:
					break;
			}
		}
	}
	for (func_list_t* t = twice ; t ; t = t->next)
		t->func->unique = false;
}

void end(module_t* m) { exit(EXIT_SUCCESS); }

void _compute_modules(ast_list_t* A, module_t* m)
{
	for (ast_list_t* a = A ; a ; a = a->next)
	{
		if (a->op->type == module_identifier)
		{
			// There's a lot of graph theory i could do here but TODO.
			char* mod_name = strdup(a->op->string);
			char* i = mod_name;
			while (*i != ':') ++i;
			*i = '\0';
			for (module_list_t* mm = m->uses ; mm ; mm = mm->next)
				if (!strcmp(mm->mod->prefix, mod_name)) goto end;
			char* filename = alloc(strlen(m->dir) + strlen(mod_name) + 6);
			*filename = '\0';
			strcpy(filename, m->dir);
			strcat(filename, "/");
			strcat(filename, mod_name);
			strcat(filename, ".cog");
			module_t* M = create_module(filename);
			module_list_t* mm = alloc(sizeof *mm);
			mm->next = m->uses;
			mm->mod = M;
			m->uses = mm;
			M->first_ref = a->op->where;
			module_parse(M);
			add_backlinks(M);
			_compute_modules(M->tree, m);
		}
		else if (a->op->type == braces) _compute_modules(a->op->child, m);
end:;
	}
}

void compute_modules(module_t* m)
{
	_compute_modules(m->tree, m);
}

void demodulize(module_t* m)
{
	where_t* anywhere = alloc(sizeof *anywhere);
	anywhere->mod = m;
	for (module_list_t* mm = m->uses ; mm ; mm = mm->next)
	{
		if (mm->mod != &prelude1)
		{
			ast_list_t* ptr = make_astlist();
			ast_list_t* ptr_start = ptr;
			for (ast_list_t* tree = mm->mod->tree ; tree ; tree = tree->next)
				if (tree->op->type != none)
				{
					insert_op_after(tree->op, ptr);
					ptr = ptr->next;
				}
			// We make a function call that will hopefully be inlined later.
			insert_op_after(make_op(braces, m->tree, NULL), ptr);
			ptr=ptr->next;
			static size_t id = 0;
			char fn[20];
			sprintf(fn, "module_%zi", id++);
			char* s = strdup(fn);
			insert_op_after(make_op(def, s, anywhere), ptr);
			ptr=ptr->next;
			insert_op_after(make_op(identifier, s, anywhere), ptr);
			m->tree = ptr_start;
			demodulize(mm->mod);
		}
	}
}

void add_backtraces(module_t* mod)
{
	if (!debug) return;
	for (func_list_t* funcs = mod->funcs ; funcs ; funcs = funcs->next)
	{
		for (ast_list_t* ops = funcs->func->ops ; ops ; ops = ops->next)
		{
			switch (ops->op->type)
			{
		 		case var:
				case call:
					insert_op_before(make_op(backtrace_push, NULL, ops->op->where), ops);
					insert_op_after(make_op(backtrace_pop, NULL, ops->op->where), ops);

				default: break;
			}
		}

	}
}


int main(int argc, char** argv)
{
	char* filename = NULL;

	for (int i = 1 ; i < argc ; ++i)
	{
		if (!strcmp(argv[i], "-debug")) debug = true;
		else
		{
			char* ext = strrchr(argv[i], '.');
			if (ext && !strcmp(ext, ".cog")) filename = argv[i];
		}
	}
	if (!filename)
	{
		fprintf(stderr, "USAGE: cognac filename.cog\n");
		return EXIT_FAILURE;
	}
	print_banner();
	long system_memory = sysconf(_SC_PHYS_PAGES) * 4096;
	heap = mmap(0, system_memory, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE, -1, 0);
	load_preludes();
	module_t* m = create_module(filename);
	void(*stages[])(module_t*)
	= {
		/*
		 * TODO
		 *
		 * Optimizations should be applied in loops until they stabilise
		 *
		 * There should be 1, 2, maybe 3 optimization loops with the (non
		 * destructive) optimizations in them.
		 *
		 * That way we avoid each opt having its own limited loop.
		 */
		module_parse,
		add_backlinks,
		compute_modules,
		demodulize,
		catch_shadows,
		predeclare,
		resolve_scope,
		flatten_ast,
		merge_symbols,
		//assign_sequence_numbers,
		add_backtraces,
		inline_functions,
		compute_sources,
		compute_stack,
		static_branches,
		compute_sources,
		compute_stack,
		static_calls,
		determine_arguments,
		compute_stack,
		add_arguments,
		add_generics,
		balance_branches, // IDK if this is needed
		compute_stack,
		add_registers,
		shorten_references,
		//inline_values, // I can't remember if this is broken or not
		compute_variables,
		resolve_early_use,
		determine_unique_calls,
		add_var_types,
		add_typechecks,
		remove_unused_funcs,
		// TODO renaming pass to renumber registers and shadow_ids
		to_c,
		to_exe
	};
	for (size_t i = 0 ; i < sizeof stages / sizeof stages[0] ; ++i)
	{
		stages[i](m);
	}
	return EXIT_SUCCESS;
}

ast_list_t* join_ast(ast_list_t* a, ast_list_t* b)
{
	if (!a) return b;
	ast_list_t* ptr = a;
	while (ptr->next) ptr=ptr->next;
	ptr->next = b;
	return a;
}

ast_list_t* ast_single(type_t type, void* data, where_t* pos)
{
	ast_list_t* n = alloc(sizeof *n);
	ast_t* a = alloc (sizeof *a);
	*a = (ast_t) {.type=type, .data=data, .where=pos};
	n->op = a;
	n->next = n->prev = NULL;
	return n;
}

lit_t* mk_lit(val_type_t t, const char* str)
{
	lit_t* l = alloc(sizeof *l);
	l->type = t;
	l->string = strdup(str);
	return l;
}

word_list_t* builtins(void)
{
	static builtin_t b[] =
	{
		#include "builtins.c"
	};

	word_list_t* words = NULL;

	for (size_t i = 0; b[i].name ; ++i)
	{
		func_t* fn = alloc(sizeof *fn);
		fn->name = prefix(sanitize(b[i].name));
		fn->returns = b[i].returns;
		fn->stack = b[i].stack;
		fn->tentative_rettype = fn->rettype = b[i].rettype;
		//fn->checks = b[i].checks;
		fn->argc = b[i].argc;
		fn->args = NULL;
		fn->locals = NULL;
		fn->ops = NULL;
		fn->generic_variant = NULL;
		fn->generic = false;
		fn->has_stack = true;
		fn->captures = NULL;
		fn->calls = NULL;
		fn->branch = false;
		fn->unique = false;
		fn->has_args = true;
		fn->builtin = true;
		fn->unmangled_name = b[i].name;
		type_t calltype = b[i].calltype;
		for (int ii = b[i].argc-1 ; ii >= 0 ; --ii)
			fn->args = push_val(make_value(b[i].args[ii], NULL), fn->args);
		words = push_word(
				make_word(b[i].name, calltype,
					make_value(block,
						ast_single(closure, fn, NULL)), &prelude1), words);
	}
	return words;
}

void load_preludes(void)
{
	prelude2.path = NULL;
	prelude2.file = tmpfile();
	prelude2.uses = NULL;
	prelude2.tree = NULL;
	prelude2.funcs = NULL;
	prelude2.uses = preludes.next;
	fprintf(prelude2.file, "%.*s", src_prelude_cog_len, (char*)src_prelude_cog);
	rewind(prelude2.file);
	module_parse(&prelude2);
	add_backlinks(&prelude2);
}
