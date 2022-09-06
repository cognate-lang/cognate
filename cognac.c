#include "cognac.h"
#include "parser.h"
#include "runtime.h"
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

func_t unknown_func = (func_t)
{
	.argc=0,
	.args=NULL,
	.returns=false,
	.stack=true,
	.captures=NULL,
	.has_captures = false,
};

func_t* word_func(word_t* w)
{
	return w->val->source && w->val->source->op->type == closure
		? w->val->source->op->func
		: &unknown_func;
}

val_list_t* reverse(val_list_t* v)
{
	val_list_t* prev = NULL;
	val_list_t* current = v;
	val_list_t* new = NULL;
	for (val_list_t* c = v ; c ; c = c->next)
	{
		val_list_t* V = malloc(sizeof *V);
		V->next = new;
		V->val = c->val;
		new = V;
	}
	return new;
}

func_t* call_to_func(ast_t* c)
{
	if (c->type == call) return &unknown_func;
	else return c->func;
}

val_list_t* push_val (val_t* v, val_list_t* rest)
{
	val_list_t* a = malloc(sizeof *a);
	a->val = v;
	a->next = rest;
	return a;
}

val_t* make_value (val_type_t type, ast_list_t* source)
{
	val_t* v = malloc(sizeof *v);
	v->source = source;
	v->type = type;
	return v;
}

word_list_t* push_word(word_t* w, word_list_t* next)
{
	word_list_t* n = malloc(sizeof *n);
	n->word = w;
	n->next = next;
	return n;
}

word_t* make_word(char* name, type_t calltype, val_t* v)
{
	static size_t shadow_id = 1;
	word_t* w = malloc(sizeof *w);
	w->used = false;
	w->name = name;
	w->shadow_id = shadow_id++;
	w->used_early = true; // assume early use
	w->calltype = calltype;
	w->val = v;
	return w;
}

func_list_t* push_func(func_t* f, func_list_t* next)
{
	func_list_t* n = malloc(sizeof *n);
	n->func = f;
	n->next = next;
	return n;
}

char* make_func_name()
{
	static size_t fid = 0;
	char* str = malloc(20);
	sprintf(str, "fn%zu", fid++);
	return str;
}

func_t* make_func(ast_list_t* tree, char* name)
{
	func_t* func = malloc(sizeof *func);
	func->returns = false;
	func->ops = tree;
	func->args = NULL;
	func->argc = 0;
	func->has_args = false;
	func->has_regs = false;
	func->name = name;
	func->locals = NULL;
	func->generic_variant = NULL;
	func->generic = false;
	func->noargs_variant = NULL;
	func->noargs = false;
	func->calls = NULL;
	func->stack = false; // hmm
	func->captures = NULL;
	func->entry = false;
	func->has_captures = false;
	return func;
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

func_t* virtual_choose_func(func_list_t* F)
{
	if (consistent(F->func, F->next->func)) return F->func;
	else return &unknown_func;
}

ast_t* make_op(type_t type, void* data)
{
	ast_t* a = malloc(sizeof *a);
	a->type = type;
	a->data = data;
	return a;
}

void insert_op_before(ast_t* op, ast_list_t* current)
{
	ast_list_t* n = malloc(sizeof *n);
	n->prev = current->prev;
	n->next = current;
	current->prev->next = n;
	current->prev = n;
	n->op = op;
}

void insert_op_after(ast_t* op, ast_list_t* current)
{
	ast_list_t* n = malloc(sizeof *n);
	n->prev = current;
	n->next = current->next;
	current->next->prev = n;
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
	static size_t next_register_id = 0;
	reg_t* reg = malloc (sizeof *reg);
	reg->type = t;
	reg->source = source;
	reg->id = next_register_id++;
	reg->next = reg->prev = NULL;
	return reg;
}

void push_register_front(reg_t* reg, reg_dequeue_t* registers)
{
	if (!reg) __builtin_trap();
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
	if (!reg) __builtin_trap();
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

reg_dequeue_t* make_register_dequeue()
{
	reg_dequeue_t* r = malloc(sizeof *r);
	r->front = r->rear = NULL;
	r->len = 0;
	return r;
}

void _fold_defs(ast_list_t* tree)
{
	for (ast_list_t* ptr = tree ; ptr && ptr->next ; ptr = ptr->next)
	{
		if (ptr->op->type == braces) _fold_defs(ptr->op->child);
		switch (ptr->op->type)
		{
			default: break;
			case identifier:;
				char* name = ptr->op->string;
				type_t t;
				if      (!strcmp("def", name)) t = def;
				else if (!strcmp("let", name)) t = let;
				else break;
				if (ptr->prev->op->type != identifier) __builtin_trap(); // TODO
				ptr->prev->op->type = t;
				remove_op(ptr);
		}
	}
}

void fold_defs(module_t* m)
{
	_fold_defs(m->tree);
}

module_t* create_module(char* path)
{
	module_t* mod = malloc(sizeof *mod);
	mod->path = path;
	mod->file = fopen(path, "r");
	mod->name = NULL; // TODO
	mod->tree = NULL;
	mod->funcs = NULL;
	return mod;
}

void module_parse(module_t* mod)
{
	pcc_context_t *ctx = pcc_create(mod);
	while (pcc_parse(ctx, (void*)&mod->tree));
	pcc_destroy(ctx);
}

ast_list_t* _predeclare(ast_list_t* tree, word_list_t* words)
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
							make_value(node->op->type==def?block:any, node));
					node->op->type = bind;
					node->op->word = new;
					insert_op_after(make_op(define, new), tree);
					break;
				}
				break;
			case braces:
				node->op->child = _predeclare(node->op->child, words);
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
				case none:
				case define:
					break;
				default: __builtin_trap();
				case pick:
					push_register_front(pop_register_rear(regs), regs);
					break;
				case unpick:
					push_register_rear(pop_register_front(regs), regs);
					break;
				case literal:
				case closure:
				case pop:
				case to_any:
				case from_any:
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
						pop_register_front(regs);
						func_t* f = virtual_choose_func(a->op->funcs);
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
						func_t* f = call_to_func(a->op);
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
						while (r->source->op->type == var)
							r->source = r->source->op->word->val->source;
						a->op->word->val->source = r->source;
						break;
					}
				case push:
				case ret:
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

void _resolve_scope(ast_list_t* tree, word_list_t* words)
{
	for (ast_list_t* node = tree ; node ; node = node->next)
	{
		switch(node->op->type)
		{
			case braces:
				_resolve_scope(node->op->child, words);
				break;
			case identifier:;
				word_list_t* w = words;
				for (; w ; w = w->next)
					if (!strcmp(w->word->name, node->op->string))
						break;
				if (!w) printf("cant find %s\n", node->op->string), __builtin_trap();
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

void predeclare(module_t* mod)
{
	mod->tree = _predeclare(mod->tree, NULL);
}

void resolve_scope(module_t* mod)
{
	_resolve_scope(mod->tree, builtins());
}

/*
void _add_captures(func_t* func)
{
	if (func->has_captures) return;
	func->has_captures = true;
	// Annotate a list of func_t structures with captured variables
	func->locals = NULL;
	func->captures = NULL;
	for (ast_list_t* node = func->ops ; node ; node = node->next)
	{
		switch(node->op->type)
		{
			case closure:
				if (!node->op->func->has_captures) _add_captures(node->op->func);
				for (word_list_t* c = node->op->func->captures ; c ; c = c->next)
				{
					for (word_list_t* w = func->captures ; w ; w = w->next)
						if (w->word == c->word) goto next; // O(n^2)
					for (word_list_t* w = func->locals ; w ; w = w->next)
						if (w->word == c->word) goto next; // very O(n^2)
					func->captures = push_word(c->word, func->captures);
					next:;
				}
				break;
			case call:
			case var:
				for (word_list_t* w = func->captures ; w ; w = w->next)
					if (w->word == node->op->word) goto next2; // O(n^2)
				for (word_list_t* w = func->locals ; w ; w = w->next)
					if (w->word == node->op->word) goto next2; // very O(n^2)
				func->captures = push_word(node->op->word, func->captures);
				break;
			case bind:
			case define:
				func->locals = push_word(node->op->word, func->locals);
				break;
			default: next2:;
		}
	}
}

void add_captures(module_t* mod)
{
	for (func_list_t* f = mod->funcs ; f ; f = f->next)
		_add_captures(f->func);
}
*/

func_t* _flatten_ast(ast_list_t* tree, func_list_t** rest)
{
	// Build a list of functions in the `rest` parameter
	// return the entry function for the AST node
	func_t* func = make_func(tree, make_func_name());
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
		char s[strlen(literal->string) + 2];
		s[0] = '$';
		s[1] = '\0';
		strcat(s, literal->string);
		return strdup(s);
	}
	return literal->string; // TODO
}

const char* c_val_type(val_type_t type)
{
	switch (type)
	{
		case number: return "NUMBER";
		case symbol: return "SYMBOL";
		case string: return "STRING";
		case block:  return "BLOCK";
		case list:   return "LIST";
		case boolean:return "BOOLEAN";
		case any:    return "ANY";
		case box:    return "BOX";
		default: __builtin_trap();
	}
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
	char* s = malloc(len + 4);
	*(int*)s = *(int*)"___\0";
	strcpy(s+3, str);
	return s;
}

const char* c_word_name(word_t* word)
{
	char* str = malloc(strlen(word->name) + 20);
	*(uint32_t*)str = *(uint32_t*)"___";
	if (word->shadow_id)
		sprintf(str+3, "%s_%zu", word->name, word->shadow_id);
	else
		sprintf(str+3, "%s", word->name);
	return sanitize(str);
}

void to_exe(module_t* mod)
{
	char* c_source_path = malloc(strlen(mod->path) + 3);
	strcpy(c_source_path, mod->path);
	strcat(c_source_path, ".c");
	char* args[] =
	{
		"gcc", c_source_path, "-o", "./a.out",
		"-Ofast", "-flto", //"-Wno-unused", "-Wall", "-Wextra", "-Wpedantic",
		"-std=gnu11", "-lm", NULL
	};
	execvp(args[0], args);
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
		char sep = i + 1 == fn->argc ? ')' : ',';
		fprintf(c_source, "_%zu%c", pop_register_front(registers)->id, sep);
	}
	if (fn->argc == 0) fprintf(c_source, ")");

}

void to_c(module_t* mod)
{
	char* c_source_path = malloc(strlen(mod->path) + 3);
	strcpy(c_source_path, mod->path);
	strcat(c_source_path, ".c");
	FILE* c_source = fopen(c_source_path, "w");
	fprintf(c_source, "%.*s", runtime_c_len, (char*)runtime_c);
	fputc('\n', c_source);
	for (symbol_list_t* syms = mod->symbols ; syms ; syms = syms->next)
	{
		fprintf(c_source, "SYMBOL $%s = \"%s\";\n", syms->text, syms->text);
	}
	if (mod->symbols) fputc('\n', c_source);
	for (func_list_t* func = mod->funcs ; func ; func = func->next)
	{
		size_t num_words = 0;
		fprintf(c_source, "%s %s(",
				func->func->returns ? c_val_type(func->func->rettype) : "void",
				func->func->name);
		if (!func->func->generic) for (word_list_t* w = func->func->captures ; w ; w = w->next)
		{
			fprintf(c_source, "%s", c_val_type(w->word->val->type));
			if (w->next || func->func->argc) fprintf(c_source, ", ");
		}
		else
		{
			fprintf(c_source, "void* env[%zu]", num_words);
			if (func->func->argc) fprintf(c_source, ", ");
		}
		reg_dequeue_t* ar = make_register_dequeue();
		for (val_list_t* v = func->func->args ; v ; v = v->next)
		{
			fprintf(c_source, "%s", c_val_type(v->val->type));
			if (v->next) fprintf(c_source, ", ");
		}
		if (!func->func->generic && !func->func->captures && !func->func->argc)
			fprintf(c_source, "void");
		fprintf(c_source, ");\n");

		if (func->func->generic_variant && func->func->generic_variant->used) fprintf(c_source, "BLOCK clone_%s(BLOCK);\n", func->func->name);
	}
	fputc('\n', c_source);
	for (func_list_t* func = mod->funcs ; func ; func = func->next)
	{
		size_t num_words = 0;
		if (func->func->generic_variant && func->func->generic_variant->used && !func->func->generic && !func->func->noargs)
		{
			for (word_list_t* w = func->func->captures ; w ; w = w->next) num_words++;
			fprintf(c_source, "BLOCK clone_%s(BLOCK b)\n{\n", func->func->name);
			fprintf(c_source, "\tif (b->heap_clone) return b->heap_clone;\n");
			fprintf(c_source, "\tBLOCK B = gc_malloc(%zu * WORDSZ);\n", num_words + 3);
			fprintf(c_source, "\tb->heap_clone = B;\n");
			fprintf(c_source, "\tB->heap_clone = B;\n");
			fprintf(c_source, "\tB->fn = &%s;\n\tB->clone = &clone_%s;\n",
					func->func->generic_variant->name, func->func->name);
			size_t i = 0;
			for (word_list_t* w = func->func->captures ; w ; w = w->next, i++)
			{
				char cast[128] = "";
				if (w->word->used_early) sprintf(cast, "*(%s*)", c_val_type(w->word->val->type));
				else sprintf(cast, "*(%s*)&", c_val_type(w->word->val->type));
				if (w->word->used_early) fprintf(c_source, "\tB->env[%zu] = gc_malloc(WORDSZ);\n", i);
				if (w->word->val->type == block)
				{
					fprintf(c_source, "\t%sB->env[%zu] = (%sb->env[%zu])->clone(%sb->env[%zu]);\n",
						cast, i, cast, i, cast, i);
				}
				else
				{
					fprintf(c_source, "\t%sB->env[%zu] = %sb->env[%zu];\n",
						cast, i, cast, i);
				}
			}
			fprintf(c_source, "\treturn B;\n}\n");
		}

		fprintf(c_source, "%s %s(",
				func->func->returns ? c_val_type(func->func->rettype) : "void",
				func->func->name);
		if (!func->func->generic)
			for (word_list_t* w = func->func->captures ; w ; w = w->next)
			{
				fprintf(c_source, "%s %s",
						c_val_type(w->word->val->type),
						c_word_name(w->word));
				if (w->next || func->func->argc) fprintf(c_source, ", ");
			}
		else
		{
			fprintf(c_source, "void* env[%zu]", num_words);
			if (func->func->argc) fprintf(c_source, ", ");
		}
		reg_dequeue_t* ar = make_register_dequeue();
		for (val_list_t* v = func->func->args ; v ; v = v->next)
		{
			reg_t* r = make_register(v->val->type, NULL), args;
			fprintf(c_source, "%s _%zu", c_val_type(v->val->type), r->id);
			push_register_front(r, ar);
			if (v->next) fprintf(c_source, ", ");
		}
		if (!(func->func->generic || func->func->noargs) && !func->func->captures && !func->func->args)
			fprintf(c_source, "void");
		fprintf(c_source, ") {\n");
		size_t i = 0;
		if (func->func->generic)
			for (word_list_t* w = func->func->captures ; w ; w = w->next, i++)
			{
				char cast[128] = "";
				if (w->word->used_early) sprintf(cast, "*(%s*)", c_val_type(w->word->val->type));
				else sprintf(cast, "*(%s*)&", c_val_type(w->word->val->type));
				fprintf(c_source, "\t%s %s = %senv[%zu];\n",
					c_val_type(w->word->val->type),
					c_word_name(w->word),
					cast, i);
			}
		reg_dequeue_t* registers = make_register_dequeue();
		reg_t* res = NULL;
		for (ast_list_t* op = func->func->ops ; op ; op = op->next)
		{
			switch (op->op->type)
			{
				default: __builtin_trap();
				case fn_branch:
					{
						func_t* v = virtual_choose_func(op->op->funcs); // hmmm
						bool g = v == &unknown_func;
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
									c_emit_funcall(g?f->func->noargs_variant:f->func, c_source, registers);
									fprintf(c_source, " : ");
								}
								else
								{
									c_emit_funcall(g?f->func->noargs_variant:f->func, c_source, registers);
									fprintf(c_source, ";\n", sanitize(f->func->name));
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
									c_emit_funcall(g?f->func->noargs_variant:f->func, c_source, registers);
									fprintf(c_source, ";\n\telse ");
								}
								else
								{
									c_emit_funcall(g?f->func->noargs_variant:f->func, c_source, registers);
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
				case define: // TODO default value
					if (op->op->word->calltype == call)
						fprintf(c_source, "\tBLOCK %s%s;\n",
							c_word_name(op->op->word), op->op->word->used_early ? " = undefined_function" : "");
					else
						fprintf(c_source, "\t%s %s%s;\n",
							c_val_type(op->op->word->val->type),
							c_word_name(op->op->word), op->op->word->used_early ? " = box_SYMBOL(\"undefined\")" : "");
					break;
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
				case var:
					{
						reg_t* reg = make_register(op->op->word->val->type, NULL);
						push_register_front(reg, registers);
						fprintf(c_source, "\t%s _%zu = %s;\n",
							c_val_type(op->op->word->val->type),
							reg->id,
							c_word_name(op->op->word));
						break;
					}
				case bind:
					fprintf(c_source, "\t%s = _%zu;\n",
						c_word_name(op->op->word),
						pop_register_front(registers)->id);
					break;
				case static_call:
					{
						reg_t* ret;
						func_t* fn = op->op->func;
						if (fn->returns)
						{
							ret = make_register(fn->rettype, NULL);
							fprintf(c_source, "\t%s _%zu = ",
									c_val_type(fn->rettype),
									ret->id);
						}
						else fprintf(c_source, "\t");
						fprintf(c_source, "%s(",
								sanitize(fn->name));
						for (word_list_t* w = fn->captures ; w ; w = w->next)
						{
							fprintf(c_source, "%s", c_word_name(w->word));
							if (w->next || fn->argc) fprintf(c_source, ",");
						}
						for (size_t i = 0 ; i < fn->argc ; ++i)
						{
							char sep = i + 1 == fn->argc ? ')' : ',';
							fprintf(c_source, "_%zu%c", pop_register_front(registers)->id, sep);
						}
						if (fn->argc == 0) fprintf(c_source, ")");
						fprintf(c_source, ";\n");
						if (fn->returns) push_register_front(ret, registers);
						break;
					}
				case call:
					fprintf(c_source, "\t%s->fn(%s->env);\n",
						c_word_name(op->op->word),
						c_word_name(op->op->word));
					break;
				case closure:
					{
						size_t num_words = 0;
						for (word_list_t* w = op->op->func->captures ; w ; w = w->next) num_words++;
						reg_t* reg = make_register(block, NULL);
						push_register_front(reg, registers);
						fprintf(c_source, "\tBLOCK _%zu = alloca(%zu * WORDSZ);\n",
							reg->id,
							num_words + 3);
						fprintf(c_source, "\t_%zu->fn = %s;\n", reg->id, op->op->func->generic_variant->name);
						fprintf(c_source, "\t_%zu->clone = &clone_%s;\n", reg->id, op->op->func->name);
						fprintf(c_source, "\t_%zu->heap_clone = NULL;\n", reg->id);
						size_t i = 0;
						for (word_list_t* w = op->op->func->captures ; w ; w = w->next, i++)
						{
							char* cast = w->word->used_early ? "(void*)&" : "*(void**)&";
							fprintf(c_source, "\t_%zu->env[%zu] = %s%s;\n",
								reg->id, i,
								cast,
								c_word_name(w->word));
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
		if (res)
		{
			if (res->type == block)
				fprintf(c_source, "\treturn _%zu->clone(_%zu);\n", res->id, res->id);
			else fprintf(c_source, "\treturn _%zu;\n", res->id);
		}
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
		case literal:    printf("[literal] %s\n", tree->op->literal->string); break;
		case braces:     puts("\\"); print_ast(tree->op->child, i+1); break;
		case closure:    printf("[closure] %s\n", tree->op->func->name); break;
		case def:        printf("[def] %s\n", tree->op->string); break;
		case let:        printf("[def] %s\n", tree->op->string); break;
		case call:       printf("[call] %s(%zu)\n", tree->op->word->name, tree->op->word->shadow_id); break;
		case var:        printf("[var] %s(%zu)\n", tree->op->word->name, tree->op->word->shadow_id); break;
		case bind:       printf("[bind] %s(%zu)\n", tree->op->word->name, tree->op->word->shadow_id); break;
		case to_any:        printf("[to_any] %s\n", c_val_type(tree->op->val_type)); break;
		case from_any:      printf("[from_any] %s\n", c_val_type(tree->op->val_type)); break;
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
		case fn_branch:  printf("[fn_branch] %s %s\n", tree->op->funcs->func->name, tree->op->funcs->next->func->name); break;
		default:         printf("[INVALID %i]\n", tree->op->type); break;
	}
	print_ast(tree->next, i);
}

void add_noargs(module_t* mod)
{
	for (func_list_t* f = mod->funcs ; f ; f = f->next)
	{
		if (f->func == mod->entry || f->func->generic || f->func->noargs)
		{
			f->func->noargs_variant = NULL;
			continue;
		}
		else if (f->func->argc == 0 && f->func->returns == false)
		{
			f->func->noargs_variant = f->func;
			continue;
		}
		ast_list_t* tree = malloc(sizeof *tree);
		tree->prev = NULL;
		tree->next = malloc(sizeof *(tree->next));
		tree->op = make_op(none, NULL);
		tree->next->op = make_op(none, NULL);
		tree->next->prev = tree;
		tree->next->next = NULL;
		insert_op_after(
				make_op(static_call, f->func), tree);
		char* name = malloc(strlen(f->func->name) + strlen("noargs") + 1);
		strcpy(name, f->func->name);
		strcat(name, "noargs");
		func_t* f_ = make_func(tree, name);
		f_->noargs = true;
		f_->captures = f->func->captures;
		f_->noargs_variant = NULL;
		mod->funcs = push_func(f_, mod->funcs);
		f->func->noargs_variant = f_;
	}
}

void add_generics(module_t* mod)
{
	for (func_list_t* f = mod->funcs ; f ; f = f->next)
	{
		if (f->func == mod->entry || f->func->generic || f->func->noargs)
		{
			f->func->generic_variant = NULL;
			continue;
		}
		ast_list_t* tree = malloc(sizeof *tree);
		tree->prev = NULL;
		tree->next = malloc(sizeof *(tree->next));
		tree->op = make_op(none, NULL);
		tree->next->op = make_op(none, NULL);
		tree->next->prev = tree;
		tree->next->next = NULL;
		insert_op_after(
				make_op(static_call, f->func), tree);
		char* name = malloc(strlen(f->func->name) + strlen("generic") + 1);
		strcpy(name, f->func->name);
		strcat(name, "generic");
		func_t* f_ = make_func(tree, name);
		f_->generic = true;
		f_->captures = f->func->captures;
		f_->generic_variant = NULL;
		mod->funcs = push_func(f_, mod->funcs);
		f->func->generic_variant = f_;
	}
}

bool _add_arguments(func_t* f)
{
	if (f->has_args) return 0;
	f->has_args = true;
	bool changed = 0;
	size_t registers = 0;
	bool can_use_args = 1;
	for (ast_list_t* n = f->ops ; n ; n = n->next)
	{
		ast_t* op = n->op;
		switch(op->type)
		{
			case ret:
				/*
				 * TODO TODO TODO
				 * This almost seems to work for some reason
				 * Never remove arguments but allow removing the returns
				 * each pass?!?!?!?
				 * What is this madness?
				 */
				remove_op(n);
				f->returns = false;
				break;
			case closure:
				changed |= _add_arguments(op->func);
				registers++;
				break;
			case load:
			case literal:
			case var:
				registers++;
				break;
			case fn_branch:
				{
					if (registers) registers--;
					else
					{
						f->args = push_val(make_value(any, n), f->args);
						insert_op_before(make_op(load, f->args->val), n);
						f->argc++;
						changed = true;
					}

					for (func_list_t* f = op->funcs ; f ; f = f->next)
						changed |= _add_arguments(f->func);

					func_t* v = virtual_choose_func(op->funcs);

					if (v->stack)
					{
						for (size_t i = registers; i > (size_t)v->argc ; --i)
							registers--;
					}

					for (size_t i = 0 ; i < v->argc ; ++i)
					{
						if (registers) registers--;
						else if (can_use_args && f->argc < 255 && !f->entry)
						{
							f->args = push_val(make_value(any, n), f->args);
							insert_op_before(make_op(load, f->args->val), n);
							insert_op_before(make_op(unpick, f->args->val), n);
							f->argc++;
							changed = true;
						}
					}

					if (v->stack) can_use_args = false;
					if (v->returns) registers++;
					break;
				}
			case branch:
				for (char i = 0; i < 3; ++i)
				{
					if (registers) registers--;
					else if (can_use_args && f->argc < 255 && !f->entry)
					{
						f->args = push_val(make_value(any, n), f->args);
						insert_op_before(make_op(load, f->args->val), n);
						insert_op_before(make_op(unpick, f->args->val), n);
						f->argc++;
						changed = true;
					}
				}
				registers++;
				break;
			case static_call:
			case call:
				{
					func_t* fn = call_to_func(op);
					if (fn->stack)
					{
						for (size_t i = registers; i > (size_t)fn->argc ; --i)
							registers--;
					}
					for (size_t i = 0 ; i < fn->argc ; ++i)
					{
						if (registers) registers--;
						else if (can_use_args && f->argc < 255 && !f->entry)
						{
							f->args = push_val(make_value(any, n), f->args);
							insert_op_before(make_op(load, f->args->val), n);
							insert_op_before(make_op(unpick, f->args->val), n);
							f->argc++;
							changed = true;
						}
					}
					if (fn->stack) can_use_args = false;
					if (fn->returns) registers++;
					break;
				}
			case bind:
				if (registers) registers--;
				else if (can_use_args && f->argc < 255 && !f->entry)
				{
					f->args = push_val(make_value(any, n), f->args);
					insert_op_before(make_op(load, f->args->val), n);
					insert_op_before(make_op(unpick, f->args->val), n);
					f->argc++;
					changed = true;
				}
				break;
			case define: case none: case pick: case unpick: break;
			default: __builtin_trap();
		}
		if (!n->next && registers && !f->entry)
		{
			insert_op_before(make_op(ret, NULL), n);
			f->returns = true;
			f->rettype = any;
			//changed = true;
		}
	}
	f->args = reverse(f->args);
	return changed;
}

void add_arguments(module_t* mod)
{
	bool changed = true;
	while (changed)
	{
		for (func_list_t* f = mod->funcs ; f ; f = f->next)
			f->func->has_args = false;
		changed = false;
		for (func_list_t* f = mod->funcs ; f ; f = f->next)
			changed |= _add_arguments(f->func);
	}
}

void _add_registers(func_t* f)
{
	if (f->has_regs) return;
	size_t registers = 0;
	for (ast_list_t* n = f->ops ; n ; n = n->next)
	{
		ast_t* op = n->op;
		switch(op->type)
		{
			case closure:
				_add_registers(op->func);
				registers++;
				break;
			case literal:
			case var:
			case load:
			case pop:
				registers++;
				break;
			case fn_branch:
				{
					if (registers) registers--;
					else
					{
						insert_op_before(make_op(pop, NULL), n);
						f->stack = true;
					}

					for (func_list_t* f = op->funcs ; f ; f = f->next)
						_add_registers(f->func);

					func_t* v = virtual_choose_func(op->funcs);

					if (v->stack)
					{
						for (size_t i = registers; i > (size_t)v->argc ; --i)
						{
							insert_op_before(make_op(pick, NULL), n);
							insert_op_before(make_op(push, NULL), n);
							f->stack = true;
							registers--;
						}
					}

					for (size_t i = 0 ; i < v->argc ; ++i)
					{
						if (registers) registers--;
						else
						{
							insert_op_before(make_op(pop, NULL), n);
							insert_op_before(make_op(unpick, NULL), n);
							f->stack = true;
						}
					}

					if (v->returns) registers++;
					break;
				}
			case branch:
				for (char i = 0; i < 3; ++i)
				{
					if (registers) registers--;
					else
					{
						insert_op_before(make_op(pop, NULL), n);
						insert_op_before(make_op(unpick, NULL), n);
						f->stack = true;
					}
				}
				registers++;
				break;
			case call:
			case static_call:
				{
					func_t* fn = call_to_func(op);
					if (fn->stack)
					{
						for (size_t i = registers; i > (size_t)fn->argc ; --i)
						{
							insert_op_before(make_op(pick, NULL), n);
							insert_op_before(make_op(push, NULL), n);
							f->stack = true;
							registers--;
						}
					}
					for (size_t i = 0 ; i < fn->argc ; ++i)
					{
						if (registers) registers--;
						else
						{
							insert_op_before(make_op(pop, NULL), n);
							insert_op_before(make_op(unpick, NULL), n);
							f->stack = true;
						}
					}
					if (fn->returns) registers++;
					break;
				}
			case bind:
			case ret:
			case push:
				if (registers) registers--;
				else
				{
					insert_op_before(make_op(pop, NULL), n);
					insert_op_before(make_op(unpick, NULL), n);
					f->stack = true;
				}
				break;
			case define: case none: case pick: case unpick: break;
			default: __builtin_trap();
		}
		if (!n->next)
		{
			for (size_t i = 0 ; i < registers ; ++i)
			{
				insert_op_before(make_op(pick, NULL), n);
				insert_op_before(make_op(push, NULL), n);
			}
		}

	}
	f->has_regs = true;
}

void add_registers(module_t* mod)
{
	for (func_list_t* f = mod->funcs ; f ; f = f->next)
		_add_registers(f->func);
}


/*
void add_registers(module_t* mod)
{
	for (func_list_t* func = mod->funcs ; func ; func = func->next)
	{
		size_t registers = 0;
		for (ast_list_t* op = func->func->ops ; op ; op = op->next)
		{
			switch (op->op->type)
			{
				case pick:
				case unpick:
					break;
				case literal:
				case var:
				case closure:
				case load:
					registers++;
					break;
				case branch:
					for (char i = 0; i < 3; ++i)
					{
						if (registers) registers--;
						else
						{
							insert_op_before(make_op(pop, NULL), op);
							insert_op_before(make_op(unpick, NULL), op);
						}
					}
					registers++;
					break;
				case fn_branch:
					{
						if (registers) registers--;
						else
							insert_op_before(make_op(pop, NULL), op);

						func_t* v = virtual_choose_func(op->op->funcs);
						assert(!(v->stack && v->argc));
						for (size_t i = 0 ; i < v->argc ; ++i)
						{
							if (registers) registers--;
							else
							{
								insert_op_before(make_op(pop, NULL), op);
								insert_op_before(make_op(unpick, NULL), op);
							}
						}
						if (v->stack && registers)
						{
							for (size_t i = 0 ; i < registers ; ++i)
							{
								insert_op_before(make_op(pick, NULL), op);
								insert_op_before(make_op(push, NULL), op);
							}
							registers = 0;
						}
						if (v->stack) assert(!registers);
						if (v->returns) registers++;
						break;
					}
				case call:
				case static_call:
					{
						func_t* fn = call_to_func(op->op);
						for (size_t i = 0 ; i < fn->argc ; ++i)
						{
							if (registers) registers--;
							else
							{
								insert_op_before(make_op(pop, NULL), op);
								insert_op_before(make_op(unpick, NULL), op);
							}
						}
						if (fn->stack && registers)
						{
							for (size_t i = 0 ; i < registers ; ++i)
							{
								insert_op_before(make_op(pick, NULL), op);
								insert_op_before(make_op(push, NULL), op);
							}
							registers = 0;
						}
						if (fn->stack) assert(!registers);
						if (fn->returns) registers++;
						break;
					}
				case bind:
				case ret:
					if (registers) registers--;
					else insert_op_before(make_op(pop, NULL), op);
					break;
				case none: case define: break;
				default: __builtin_trap();
			}
			if (!op->next)
			{
				for (size_t i = 0 ; i < registers ; ++i)
				{
					insert_op_before(make_op(pick, NULL), op);
					insert_op_before(make_op(push, NULL), op);
				}
			}
		}
	}
}
*/

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
						func_t* fn = virtual_choose_func(op->op->funcs);
						for ( size_t i = 0 ; i < fn->argc ; ++i )
							pop_register_front(registers);
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
						if (t != any && func->func->rettype != t)
						{
							if (func->func->rettype != any) __builtin_trap(); // type error
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
				case static_call:
					{
						func_t* fn = call_to_func(op->op);
						for ( size_t i = 0 ; i < fn->argc ; ++i )
							pop_register_front(registers);
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
						if (!op->op->word->used_early
								&& t != any
								&& op->op->word->val->type != t) // Early use needs to be bound to undefined symbol
						{
							if (op->op->word->val->type != any) __builtin_trap(); // type error
							op->op->word->val->type = t;
							changed = 1;
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
						reg_t* ar1 = pop_register_front(registers);
						push_register_front(make_register(ar1->type, op), registers);
						push_register_front(make_register(ar1->type, op), registers);
						push_register_front(make_register(boolean, op), registers);
						break;
					}
				case fn_branch:
					{
						func_t* fn = virtual_choose_func(op->op->funcs);
						if (fn->returns)
							pop_register_front(registers);
						val_list_t* reversed_args = reverse(fn->args);
						for ( val_list_t* a = reversed_args ; a ; a = a->next )
							push_register_front(make_register(a->val->type, op), registers);
						if (fn->stack) assert(registers->len == 0);
						push_register_front(make_register(boolean, op), registers);
						break;
					}
				case var:
					{
						val_type_t t = pop_register_front(registers)->type;
						if (t != any
								&& !op->op->word->used_early
								&& op->op->word->val->type != t)
						{
							if (op->op->word->val->type != any) __builtin_trap(); // type error
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
						func_t* fn = call_to_func(op->op);
						if (fn->returns)
							pop_register_front(registers);
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
					break;
				default: __builtin_trap();
			}

		}
		for (val_list_t* v = func->func->args; v ; v = v->next)
		{
			val_type_t t = args->val->type;
			if (t != any && v->val->type != t)
			{
				if (v->val->type != any) __builtin_trap(); // type error
				v->val->type = t;
				changed = 1;
			}
			args = args->next;
		}

	}
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
							insert_op_before(make_op(from_any, (void*)boolean), op);
						} else __builtin_trap();
						insert_op_before(make_op(unpick, NULL), op);
						val_type_t res = any;
						if (ar2->type == ar3->type)
						{
							res = ar2->type;
							insert_op_before(make_op(unpick, NULL), op);
						}
						else if (ar2->type == any)
						{
							insert_op_before(make_op(unpick, NULL), op);
							insert_op_before(make_op(to_any, (void*)ar3->type), op);
						}
						else if (ar3->type == any)
						{
							insert_op_before(make_op(to_any, (void*)ar2->type), op);
							insert_op_before(make_op(unpick, NULL), op);
						}
						else
						{
							insert_op_before(make_op(to_any, (void*)ar2->type), op);
							insert_op_before(make_op(unpick, NULL), op);
							insert_op_before(make_op(to_any, (void*)ar3->type), op);
						}
						insert_op_before(make_op(pick, NULL), op);
						insert_op_before(make_op(pick, NULL), op);
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
							insert_op_before(make_op(to_any, (void*)reg->type), op);
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
							insert_op_before(make_op(from_any, (void*)boolean), op);
						} else __builtin_trap();
						func_t* fn = virtual_choose_func(op->op->funcs);
						size_t i = 0;
						for ( val_list_t* a = fn->args ; a ; a = a->next )
						{
							reg_t* reg = pop_register_front(registers);
							val_type_t expected = a->val->type;
							val_type_t got = reg->type;
							if (expected == got) { }
							else if (expected == any)
								insert_op_before(make_op(to_any, (void*)got), op);
							else if (got == any)
								insert_op_before(make_op(from_any, (void*)expected), op);
							else __builtin_trap();
							insert_op_before(make_op(unpick, NULL), op);
						}
						for ( int i = 0 ; i < fn->argc ; ++i )
							insert_op_before(make_op(pick, NULL), op);
						if (fn->returns)
							push_register_front(make_register(fn->rettype, op), registers);
						break;

					}
				case call:
				case static_call:
					{
						func_t* fn = call_to_func(op->op);
						size_t i = 0;
						for ( val_list_t* a = fn->args ; a ; a = a->next )
						{
							reg_t* reg = pop_register_front(registers);
							val_type_t expected = a->val->type;
							val_type_t got = reg->type;
							if (expected == got) { }
							else if (expected == any)
								insert_op_before(make_op(to_any, (void*)got), op);
							else if (got == any)
								insert_op_before(make_op(from_any, (void*)expected), op);
							else __builtin_trap();
							insert_op_before(make_op(unpick, NULL), op);
						}
						for ( size_t i = 0 ; i < fn->argc ; ++i )
							insert_op_before(make_op(pick, NULL), op);
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
							insert_op_before(make_op(to_any, (void*)got), op);
						else if (got == any)
							insert_op_before(make_op(from_any, (void*)expected), op);
						else __builtin_trap();
						break;
					}
				case bind:
					{
						reg_t* reg1 = pop_register_front(registers);
						val_type_t expected = op->op->word->val->type;
						val_type_t got = reg1->type;
						if (expected == got) { }
						else if (expected == any)
							insert_op_before(make_op(to_any, (void*)got), op);
						else if (got == any)
							insert_op_before(make_op(from_any, (void*)expected), op);
						else __builtin_trap();
						break;
					}
				case none: break;
				case define: break;
				default: __builtin_trap();
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
		printf("======== %s ========\n", f->func->name);
		printf("( ");
		for (word_list_t* w = f->func->captures ; w ; w = w->next)
			printf("%s ", w->word->name);
		printf(")\n");
		if (f->func->stack) printf("NEEDS STACK\n");
		print_ast(f->func->ops, 0);
	}
}

/* wait i don't think i need this lol
word_list_t* clone_words(word_list_t* words, word_list_t* locals_from, word_list_t* locals_to)
{
	if (!words) return NULL;
	size_t i = 0;
	for (word_list_t* W = locals_from ; W ; W = W->next, i++)
		if (W->word) goto found;
	return push_word(words->word, clone_words(words->next, locals_from, locals_to));
found:;
	word_list_t* w = locals_to;
	for (; i ; w = w->next, i--);
	return push_word(w->word, clone_words(words->next, locals_from, locals_to));
}
*/

void* assoc_push(void* from, void* to, ptr_assoc_t* assoc)
{
	ptr_assoc_t* new = malloc(sizeof *new);
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

ast_list_t* clone_func(ast_list_t* ops, ptr_assoc_t* assoc, func_list_t** funcs)
{
	ast_list_t* l = malloc (sizeof *l);
	l->op = malloc(sizeof *(l->op));
	l->op->type = none;
	l->prev = NULL;
	ast_list_t* prev = l;
	for (ast_list_t* op = ops->next ; op ; op = op->next)
	{
		ast_list_t* node = malloc(sizeof *node);
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
									assoc)));
					assoc = assoc_push(op->op->word, new_word, assoc);
					node->op = make_op(define, new_word);
					break;
				}
			case bind:
			case var:
			case call:
				{
					size_t i = 0;
					word_t* new = ptr_assoc_lookup(op->op->word, assoc);
					if (!new)
					{
						node->op = make_op(op->op->type, op->op->word);
						break;
					}
					node->op = make_op(op->op->type, new);
					break;
				}
			case closure:
				{
					func_t* fn = make_func(
							clone_func(op->op->func->ops, assoc, funcs),
							make_func_name());
					*funcs = push_func(fn, *funcs);
					node->op = make_op(closure, fn);
					break;
				}
			default:
				node->op = op->op;
				node->op = make_op(op->op->type, op->op->data);
				break;
		}
		assoc = assoc_push(op, node, assoc);
	}
	return l;
}

bool does_call(word_t* target, ast_list_t* start)
{
	for (ast_list_t* node = start ; node ; node = node->next)
	{
		if (node->op->type == call && node->op->word == target)
			return true;
		else if (node->op->type == closure
				&& does_call(target, node->op->func->ops))
			return true;
	}
	return false;
}

void _compute_sources(ast_list_t* a)
{
	reg_dequeue_t* regs = make_register_dequeue();
	for (ast_list_t* node = a ; node ; node = node->next)
	{
		switch (node->op->type)
		{
			case closure:
				_compute_sources(node->op->func->ops);
			case literal:
			case var:
				{
					reg_t* r = make_register(any, node);
					push_register_front(r, regs);
					// TODO shorten references
				}
				break;
			case bind:
				{
					reg_t* r = pop_register_front(regs);
					if (r && r->source)
						node->op->word->val->source = r->source;
					break;
				}
			case call:
				{
					func_t* fn = word_func(node->op->word);
					if (fn->stack) clear_registers(regs);
					else for (size_t i = 0 ; i < fn->argc ; ++i)
						pop_register_front(regs);
					if (fn->returns)
						push_register_front(make_register(any, node), regs);
				}
			default: break;
		}
	}
}

void compute_sources(module_t* m)
{
	for (func_list_t* f = m->funcs ; f ; f = f->next)
		_compute_sources(f->func->ops);
}

void _inline_functions(ast_list_t* a, func_list_t** funcs)
{
	for (bool changed = 1 ; changed ;)
	{
		changed = 0;
		_compute_sources(a);
		for (ast_list_t* node = a ; node ; node = node->next)
		{
			switch (node->op->type)
			{
				case closure:
					_inline_functions(node->op->func->ops, funcs);
					break;
				case call:
					{
						func_t* fn = word_func(node->op->word);
						if (fn != &unknown_func)
						{
							if (!fn->ops) break;
							size_t n = 0;
							for (ast_list_t* inl = fn->ops ; inl ; inl = inl->next) n++;
							if (n > 20) break;
							if (does_call(node->op->word, fn->ops)) break;
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
	_inline_functions(m->entry->ops, &m->funcs);
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
					func_t* called = call_to_func(a->op);
					if (_compute_stack(called))
					{
						f->stack = true;
						return true;
					}
				}
				break;
			case fn_branch:
				{
					func_t* called = virtual_choose_func(a->op->funcs);
					if (called->stack)
					{
						f->stack = true;
						return true;
					}
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

void compute_stack(module_t* m)
{
	for (func_list_t* f = m->funcs ; f ; f = f->next)
		f->func->stack = false, f->func->has_stack = false;
	for (func_list_t* f = m->funcs ; f ; f = f->next)
		_compute_stack(f->func);
}

void compute_captures(func_t* f, word_list_t** ret, word_list_t** locals, func_list_t** tried)
{
	for (ast_list_t* a = f->ops ; a ; a = a->next)
	{
		switch (a->op->type)
		{
			default: break;
			case define:
				*locals = push_word(a->op->word, *locals);
				break;
			case static_call:
				{
					func_t* called = a->op->func;
					for (func_list_t* r = *tried ; r ; r=r->next)
						if (called == r->func) goto end;
					*tried = push_func(called, *tried);
					if (called != f)
						compute_captures(called, ret, locals, tried);
					break;
				}
			case fn_branch:
				{
					for (func_list_t* called = a->op->funcs ; called ; called = called->next)
					{
						for (func_list_t* r = *tried ; r ; r=r->next)
							if (called->func == r->func) goto end;
						*tried = push_func(called->func, *tried);
						if (called->func != f)
							compute_captures(called->func, ret, locals, tried);
					}
					break;
				}

			case call:
			case var:
				for (word_list_t* r = *ret ; r ; r=r->next)
					if (r->word == a->op->word) goto end;
				for (word_list_t* r = *locals ; r ; r=r->next)
					if (r->word == a->op->word) goto end;
				*ret = push_word(a->op->word, *ret);
				end:
				break;
			case closure:
				compute_captures(a->op->func, ret, locals, tried);
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

void compute_variables(module_t* m)
{
	bool changed = 1;
	while (changed)
	{
		changed = 0;
		for (func_list_t* f = m->funcs ; f ; f = f->next)
			compute_captures(
					f->func,
					&f->func->captures,
					&f->func->locals,
					&f->func->calls);
		mark_all_unused(m);
		mark_used_vars(m);
		for (func_list_t* f = m->funcs ; f ; f = f->next)
			changed |= remove_unused_vars(f->func);
	}
}

/*
word_list_t* _compute_variables(func_t* f)
{
	if (f->has_captures) return f->captures;
	f->has_captures = true;
	assert(!f->captures);
	bool changed = 1;
	while (changed)
	{
		f->locals = NULL;
		f->captures = NULL;
		changed = false;
		for (ast_list_t* node = f->ops ; node ; node = node->next)
			if (node->op->type == define) node->op->word->used = false;
		for (ast_list_t* node = f->ops ; node ; node = node->next)
		{
			switch (node->op->type)
			{
				case define:
					for (word_list_t* w = f->locals ; w ; w = w->next)
						if (w->word == node->op->word) goto end; // very O(n^2)
					f->locals = push_word(node->op->word, f->locals);
					break;
				case call:
				case var:
					{
						func_t* fn = word_func(node->op->word);
						if (fn == &unknown_func || node->op->type == var)
						{
							node->op->word->used = true;
							for (word_list_t* w = f->captures ; w ; w = w->next)
								if (w->word == node->op->word) goto end;
							for (word_list_t* w = f->locals ; w ; w = w->next)
								if (w->word == node->op->word) goto end;
							f->captures = push_word(node->op->word, f->captures);
						}
						else
						{
							for (func_list_t* calls = fn->calls ; calls ; calls = calls->next)
							{
								for (word_list_t* e = calls->func->captures ; e ; e = e->next)
								{
									e->word->used = true;
									for (word_list_t* w = f->captures ; w ; w = w->next)
										if (w->word == e->word) goto end; // O(n^2)
									for (word_list_t* w = f->locals ; w ; w = w->next)
										if (w->word == e->word) goto end; // very O(n^2)
									f->captures = push_word(e->word, f->captures);
								}
							}
						}
						break;
					}
				case closure:
					{
						word_list_t* extra = _compute_variables(node->op->func);
						for (word_list_t* e = extra ; e ; e = e->next)
						{
							e->word->used = true;
							for (word_list_t* w = f->captures ; w ; w = w->next)
								if (w->word == e->word) goto end; // O(n^2)
							for (word_list_t* w = f->locals ; w ; w = w->next)
								if (w->word == e->word) goto end; // very O(n^2)
							f->captures = push_word(e->word, f->captures);
						}
						break;
					}
				end: break;
				default: break;
			}
		}
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
				case builtin_var:
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
				case builtin_call:
					{
						func_t* fn = word_func(node->op->word);
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
	}
	if (f->generic_variant) f->generic_variant->captures = f->captures;
	return f->captures;
}
*/

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
			bool cons = consistent(n->op->funcs->func, n->op->funcs->next->func);
			for (func_list_t* f = n->op->funcs ; f ; f = f->next)
			{
				mark_used_funcs(f->func);
				if (!cons) mark_used_funcs(f->func->noargs_variant);
			}
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

/*
void add_function_ops(module_t* m)
{
	print_funcs(m->funcs);
	for (func_t* f = m->funcs ; f ; f = f->next)
	{
		for (ast_t* n = f->ops ; n ; n = n->next)
		{
			if (n->type == builtin_call || n->type == call)
			{
				if (n->word->op) puts("wow");
				//n->type = n->word->op;
			}
		}
	}
}
*/

void _add_backlinks(ast_list_t** t)
{
	ast_list_t* b1 = ast_single(none, NULL);
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
	ast_list_t* b2 = ast_single(none, NULL);
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
			if (a->op->type == var && a->op->word->val->source->op->type == literal)
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
				func_t* called = word_func(a->op->word);
				if (called == &unknown_func) continue;
				a->op->type = static_call;
				a->op->func = called;
			}
		}
	}
}

ast_list_t* make_astlist()
{
	ast_list_t* a = malloc(sizeof *a);
	ast_list_t* b = malloc(sizeof *b);
	ast_t* A = malloc(sizeof *A);
	ast_t* B = malloc(sizeof *B);
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
				default: __builtin_trap();
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
						if (i1 && i2 &&
								i1->source->op->type == closure
								&& i2->source->op->type == closure)
						{
							func_list_t* fl
								= push_func(i1->source->op->func,
										push_func(i2->source->op->func,
											NULL));
							ast_list_t* op = make_astlist();
							insert_op_after(make_op(fn_branch, fl), op);
							func_t* F = make_func(op, make_func_name());
							a->op = make_op(closure, F);
							m->funcs = push_func(F, m->funcs);
							remove_op(i1->source);
							remove_op(i2->source);
						}
						push_register_front(make_register(any, a), regs);
						break;
					}
				case call:
				case static_call:
					{
						func_t* f = word_func(a->op->word);
						for ( size_t i = 0 ; i < f->argc ; ++i )
							pop_register_front(regs);
						if (f->returns)
							push_register_front(make_register(f->rettype, a), regs);
						break;
					}
				case bind:
					pop_register_front(regs);
					break;
				case push:
				case ret:
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
				symbol_list_t* S = malloc(sizeof *S);
				S->text = (char*)ast->op->literal->string;
				S->next = m->symbols;
				m->symbols = S;
			}
next:;
		}
	}
}

int main(int argc, char** argv)
{
	(void)argc; (void)argv;
	assert(argc == 2);
	module_t* m = create_module(argv[1]);
	void(*stages[])(module_t*)
	= {
		module_parse,
		add_backlinks,
		fold_defs,
		predeclare,
		resolve_scope,
		flatten_ast,
		inline_functions,
		compute_sources,
		static_branches,
		compute_sources,
		static_calls,
		add_arguments,
		add_generics,
		add_noargs,
		compute_stack,
		add_registers,
		shorten_references,
		inline_values,
		compute_variables,
		remove_unused_funcs,
		resolve_early_use,
		add_var_types,
		add_typechecks,
		merge_symbols,
		to_c,
		to_exe
	};
	for (size_t i = 0 ; i < sizeof stages / sizeof stages[0] ; ++i)
		stages[i](m);
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

ast_list_t* ast_single(type_t type, void* data)
{
	ast_list_t* n = malloc(sizeof *n);
	ast_t* a = malloc (sizeof *a);
	*a = (ast_t) {.type=type, .data=data};
	n->op = a;
	n->next = n->prev = NULL;
	return n;
}

lit_t* mk_lit(val_type_t t, const char* str)
{
	lit_t* l = malloc(sizeof *l);
	l->type = t;
	l->string = strdup(str);
	return l;
}

word_list_t* builtins()
{
	static builtin_t b[] =
	{
		#include "builtins.c"
	};

	word_list_t* words = NULL;

	for (size_t i = 0; b[i].name ; ++i)
	{
		func_t* fn = malloc(sizeof *fn);
		fn->name = prefix(sanitize(b[i].name));
		fn->returns = b[i].returns;
		fn->stack = b[i].stack;
		fn->rettype = b[i].rettype;
		fn->argc = b[i].argc;
		fn->args = NULL;
		fn->locals = NULL;
		fn->ops = NULL;
		fn->generic_variant = NULL;
		fn->captures = NULL;
		fn->calls = NULL;
		fn->has_captures = true;
		fn->has_args = true;
		type_t calltype = b[i].calltype;
		for (int ii = b[i].argc-1 ; ii >= 0 ; --ii)
			fn->args = push_val(make_value(b[i].args[ii], NULL), fn->args);
		words = push_word(
				make_word(b[i].name, calltype,
					make_value(block,
						ast_single(closure, fn))), words);
	}
	return words;
}

