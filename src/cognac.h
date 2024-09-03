#pragma once

#include <stdio.h>
#include <stdbool.h>

typedef struct _ptr_assoc_t ptr_assoc_t;
typedef struct _func_t func_t;
typedef struct _module_t module_t;
typedef struct _ast_t ast_t;
typedef struct _word_t word_t;
typedef struct _builtin_t builtin_t;
typedef struct _word_list_t word_list_t;
typedef struct _func_list_t func_list_t;
typedef struct _symbol_list_t symbol_list_t;
typedef struct _ast_list_t ast_list_t;
typedef struct _lit_t lit_t;
typedef struct _reg_dequeue_t reg_dequeue_t;
typedef struct _val_t val_t;
typedef struct _val_list_t val_list_t;
typedef struct _reg_t reg_t;
typedef struct _where_t where_t;
typedef struct _where_list_t where_list_t;
typedef struct _module_list_t module_list_t;

typedef enum _type_t
{
	none = 0,
	// original AST only
	let, // STRING
	def, // STRING
	use, // STRING
	braces, // CHILD
	identifier, // STRING
	module_identifier, // STRING
	// Both
	literal,
	// IR specific
	closure, // FUNC
	var, // WORD
	call, // WORD
	define, // WORD
	bind, // WORD
	to_any, // VAL_TYPE
	from_any, // VAL_TYPE
	pop, // none
	push, // none
	pick, // none
	unpick, // none
	branch, // none
	fn_branch, // FUNCS
	load, // none
	ret, // none
	static_call, // FUNC
	drop, // none
	backtrace_push, // WORD
	backtrace_pop,
} type_t;

typedef enum _val_type_t
{
	NIL=0,
	number,
	symbol,
	table,
	string,
	boolean,
	block,
	list,
	box,
	io,
	any,
	strong_any,
} val_type_t;

struct _ptr_assoc_t
{
	void* from;
	void* to;
	ptr_assoc_t* next;
};

struct _reg_dequeue_t
{
	reg_t* front;
	reg_t* rear;
	size_t len;
};

struct _reg_t
{
	reg_t* next;
	reg_t* prev;
	ast_list_t* source;
	size_t id;
	val_type_t type;
	// registers and values should be the same thing
	// just make values dequeue-able
	// and add an id field for C generation
};

struct _lit_t
{
	const char* string;
	val_type_t type;
};

struct _word_list_t
{
	word_t* word;
	word_list_t* next;
};

struct _builtin_t
{
	char* name;
	char argc;
	val_type_t args[3];
	bool stack;
	bool returns;
	type_t calltype;
	val_type_t storagetype;
	val_type_t rettype;
	val_type_t overloads[10];
	bool overload;
	//val_type_t checks;
};

struct _val_list_t
{
	val_t* val;
	val_list_t* next;
};

struct _val_t
{
	val_type_t type;
	ast_list_t* source;
};

struct _word_t
{
	char* name;
	size_t shadow_id;
	module_t* mod;
	type_t calltype;
	val_t* val;
	bool used_early;
	bool used;
	// See decl_list from old compiler
};

struct _ast_list_t
{
	ast_t* op;
	ast_list_t* next;
	ast_list_t* prev;
};

struct _ast_t
{
	union
	{
		void* data;
		char* string;
		func_t* func;
		func_list_t* funcs;
		word_t* word;
		lit_t* literal;
		ast_list_t* child;
		module_t* mod;
		val_type_t val_type;
	};
	where_t* where;
	size_t sqnum;
	type_t type;
};

struct _func_list_t
{
	func_t* func;
	func_list_t* next;
};

struct _func_t
{
	func_t* generic_variant;
	ast_list_t* ops;
	word_list_t* captures;
	word_list_t* locals;
	func_list_t* calls;
	val_list_t* args;
	val_type_t rettype;
	val_type_t checks;
	val_type_t tentative_rettype;
	char* unmangled_name;
	size_t argc;
	char* name;
	val_type_t overloads[10];
	val_type_t overloaded_to;
	bool overload;
	bool returns;
	bool stack;
	bool used;
	bool entry;
	bool has_args;
	bool has_regs;
	bool has_stack;
	bool generic;
	bool branch;
	bool unique;
	bool builtin;
};

struct _symbol_list_t
{
	char* text;
	struct _symbol_list_t* next;
};

struct _module_t
{
	char* path;
	char* prefix;
	char* dir;
	FILE* file;
	ast_list_t* tree;
	func_list_t* funcs;
	func_t* entry;
	symbol_list_t* symbols;
	module_list_t* uses;
	where_t* first_ref;
	size_t num_lines;
	char** lines;
};

struct _module_list_t
{
	module_t* mod;
	module_list_t* next;
};

struct _where_t
{
	module_t* mod;
	size_t line;
	size_t col;
	char* symbol;
	char* line_str;
};

struct _where_list_t
{
	where_t* where;
	where_list_t* next;
};

ast_list_t* join_ast(ast_list_t*, ast_list_t*);
ast_list_t* push_ast(ast_t*, ast_list_t*);
ast_list_t* ast_single(type_t, void*, where_t*);
char* lowercase(const char*);
int main(int, char**);
void print_funcs (module_t*);
void print_ast(ast_list_t*, int);
void flatten_ast(module_t*);
void add_captures(module_t*);
void resolve_scope(module_t*);
void module_parse(module_t*);
module_t* create_module(char*);
void fold_defs(module_t*);
lit_t* mk_lit(val_type_t, const char*);
word_list_t* builtins(void);
const char* c_val_type(val_type_t);
const char* print_val_type(val_type_t);
_Noreturn void throw_error(char*, where_t*);
void load_preludes(void);

extern FILE* yyin;
extern ast_list_t* full_ast;
extern where_t* parse_pos(char*);
int yylex(void);
int yyparse (void);
void yyerror(char*);
char* lc(char*);
