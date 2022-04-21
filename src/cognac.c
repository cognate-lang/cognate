#include "cognac.h"
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <locale.h>
#include <unistd.h>
#include <sys/wait.h>
#include <limits.h>

int record_id = 0;

FILE* outfile;
size_t current_register = 0;
sym_list* declared_symbols = NULL;
ast* full_ast;
bool release = false;
bool gc_test = false;

char* restrict_chars(char* in)
{
	// Sanitise variable names so they won't upset the C compiler.
	// This could be a lot faster
	char replace[] = {'-', '!', '?', '=', '<', '>', '+', '*', '/'};
	char* with[]	 = {"DASH", "XMARK", "QMARK", "EQ", "LT", "GT", "PLUS", "STAR", "SLASH"};
	size_t sz = 1;
	for (char* ptr = in; *ptr; ++ptr)
	{
		for (size_t i = 0; i < sizeof replace; ++i)
		{
			if (*ptr == replace[i]) sz += strlen(with[i]);
			else ++sz;
		}
	}
	char* out = malloc (sz);
	for (char* ptr = in; *ptr; ++ptr)
	{
		for (size_t i = 0; i < sizeof replace; ++i)
		{
			if (*ptr == replace[i])
			{
				strcat(out, with[i]);
				goto br;
			}
		}
		strncat(out, ptr, 1);
br:;
	}
	return out;
}

char* type_as_str[8][2] =
{
	[any]     = { "any",		 "ANY"	 },
	[block]   = { "block",	 "BLOCK"	 },
	[string]  = { "string",  "STRING" },
	[number]  = { "number",  "NUMBER" },
	[symbol]  = { "symbol",  "SYMBOL" },
	[boolean] = { "boolean", "BOOLEAN"},
	[list]    = { "list",	 "LIST"	 },
	[record]  = { "record",	 "RECORD" },
};

reg_list* assert_registers(size_t lower, size_t upper, reg_list* registers)
{
	if (registers)
	{
		if (upper)
		{
			registers->next = assert_registers(lower ? lower - 1 : 0, upper - 1, registers->next);
			return registers;
		}
		assert_registers(0, 0, registers->next);
		fputs("push(", outfile);
		registers = emit_register(any, registers);
		fputs(");", outfile);
	}
	else if (lower)
	{
		reg_list* r = add_register(any, NULL);
		fputs("pop();", outfile);
		r->next = assert_registers(lower - 1, upper - 1, NULL);
		return r;
	}
	return NULL;
}

decl_list* lookup_word(char* name, decl_list* defs)
{
	for (decl_list *ptr = defs; ptr; ptr = ptr->next)
		if (strcmp(ptr->name, name) == 0) return ptr;
	return NULL;
}

void add_symbols(ast* tree)
{
	for (; tree ; tree = tree->next)
	{
		if (tree->type == value)
		{
			if (tree->val_type == symbol)
			{
				bool already_declared = false;
				for (sym_list *ptr = declared_symbols; ptr; ptr = ptr->next)
				{
					if (strcmp(ptr->name, tree->text) == 0) already_declared = true;
				}
				if (!already_declared)
				{
					fprintf(outfile, "const SYMBOL SYM(%s)=\"%s\";", restrict_chars(tree->text), tree->text);
					sym_list* s = malloc(sizeof *s);
					s->name = tree->text;
					s->next = declared_symbols;
					declared_symbols = s;
				}
			}
			else if (tree->val_type == block)
			{
				add_symbols(tree->data);
			}
		}
	}
}

void print_cognate_string(char* str)
{
	fputc('"', outfile);
	for (size_t i = 1; str[i+1] != '\0';)
	{
		int len = mblen(str + i, MB_CUR_MAX);
		if (len == -1) yyerror("string processing error");
		if (len != 1) goto utf8;
		switch (str[i])
		{
			case '\\':
				switch (str[i+1])
				{
					case 'n': fputs("\\n", outfile); break;
					case 'r': fputs("\\r", outfile); break;
					case 'e': fputs("\\033", outfile); break;
					case 't': fputs("\\t", outfile); break;
					case 'v': fputs("\\v", outfile); break;
					case '\\': fputs("\\\\", outfile); break;
					case '\'': fputs("'", outfile); break;
					default: yyerror("invalid escape sequence");
				}
				i+=2;
				break;
			case '\n':
				fputs("\\n", outfile);
				str++;
				break;
			case '"':
				fputc('\\', outfile);
				fputc(str[i], outfile);
				str++;
				break;
			default:
utf8:
				fprintf(outfile, "%.*s", len, str + i);
				str += len;
				break;
		}
	}
	fputc('"', outfile);
}

void emit_record_info(ast* tree)
{
	for (;tree;tree=tree->next)
	{
		if (tree->type==value&&tree->val_type==block) emit_record_info(tree->child);
		else if (tree->type == type)
		{
			record_t* r = tree->record;
			fputs("{",outfile);
			for (;r;r=r->next)
				fprintf(outfile,"\"%s\",", r->name);
			fputs("NULL},",outfile);
		}
	}
}

decl_list* predeclare(ast* head, decl_list* defs)
{
	/*
	 * Iterate over the ast, outputting a PREDEFINE for each function declaration.
	 * Predefining means that functions can reference functions not yet declared.
	 * However, these functions cannot be called until declaration.
	 * TODO Function calls to guaranteed undeclared functions should be compile errors.
	 */
	decl_list* already_predeclared = NULL;
	for (ast* tree = head; tree; tree=tree->next)
	{
		yylloc.first_column = tree->col;
		yylloc.first_line = tree->line;
		if (tree->type == def)
		{
			if (lookup_word(tree->text, already_predeclared)) yyerror("already defined in this block");
			// We are leaking memory here.
			// This could be stack memory with alloca() is we moved the allocation.
			decl_list* def	=  malloc(sizeof *def);
			decl_list* def2 = malloc(sizeof *def);
			*def = (decl_list){
				.type=(tree->type == let) ? var : func,
				.next = defs, .name = tree->text,
				.needs_stack = true,
				.rets = false,
				.ret = any, .predecl = true, .mut=mutable};
			fprintf(outfile, "PREDEF(%s);", restrict_chars(tree->text));
			// We use already_predeclared to prevent shadowing.
			*def2 = *def;
			def2->next = already_predeclared;
			defs = def;
			already_predeclared = def2;
		}
	}
	while (already_predeclared) already_predeclared = already_predeclared->next;
	return defs;
}

bool is_mutated(ast* tree, decl_list v)
{
	for (; tree ; tree = tree->next)
	{
		switch(tree->type)
		{
			case set: if (!strcmp(v.name, tree->text)) return true; break;
			case let: case def: if (!strcmp(v.name, tree->text)) return false; break;
			case value: if (tree->val_type == block && is_mutated(tree->child, v)) return true; break;
			default:;
		}
	}
	return false;
}


reg_list* emit_register(value_type type, reg_list* regs)
{
	if (!regs) __builtin_trap();
	else if (regs->type == type)fprintf(outfile, "r%zi", regs->id);
	else if (regs->type == any) fprintf(outfile, "unbox_%s(r%zi)", type_as_str[type][false], regs->id);
	else if (type == any)       fprintf(outfile, "box_%s(r%zi)", type_as_str[regs->type][false], regs->id);
	else
	{
		char error_msg[256];
		sprintf(error_msg, "expected a %s but got a %s", type_as_str[type][false], type_as_str[regs->type][false]);
		yyerror(error_msg);
	}
	return drop_register(regs);
}

reg_list* add_register(value_type type, reg_list* next)
{
	reg_list* r = malloc(sizeof *r);
	*r = (reg_list){.type = type, .id = current_register++, .next=next};
	fprintf(outfile, "const %s r%zi=", type_as_str[r->type][true], r->id);
	return r;
}

void compile(ast* tree, reg_list* registers, decl_list* defs)
{
	if (!tree)
	{
		assert_registers(0, 0, registers);
		return;
	}
	const char* footer = "";
	yylloc.first_column = tree->col; // This lets us use yyerror()
	yylloc.first_line = tree->line;
	fprintf(outfile, "\n#line %zi\n", tree->line);
	if (gc_test)
	{
		fprintf(outfile, "gc_collect();");
		registers = assert_registers(0, 0, registers);
	}
	switch (tree->type)
	{
		case type:
		{
			decl_list* cons = malloc(sizeof *cons);
			*cons = (decl_list)
			{
				.name = tree->record->name,
				.next = defs,
				.type = func,
				.needs_stack = true,
				.rets = true,
				.ret = record,
				.builtin = false,
				.argc = 0 // This could be faster if we took real args TODO
			};
			int i = 0;
			for (record_t*f=tree->record->next;f;f=f->next) ++i;

			fprintf(outfile,"RECORD(^VAR(%s))()=Block_copy(^{RECORD R=ALLOC_RECORD(%i);R->id=%i;",
				cons->name,i,record_id);

			for (int j = 0; j < i; ++j)
				fprintf(outfile, "R->items[%i]=pop();", j);

			fputs("return R;});", outfile);

			defs = cons;

			int j = 0;
			for (record_t*f=tree->record->next;f;f=f->next)
			{
				decl_list* field = malloc(sizeof *field);
				*field = (decl_list)
				{
					.name = f->name,
					.next = defs,
					.type = func,
					.ret = any,
					.rets = true,
					.builtin = false,
					.argc = 1,
					.args = {record}
				};
				fprintf(outfile,"ANY(^VAR(%s))(RECORD)=Block_copy(^(RECORD R){check_record_id(%i,R);return R->items[%i];});", f->name,record_id,j);
				defs = field;
				++j;
			}
			record_id++;

		}
		break;
		case identifier:
		{
			decl_list* d = lookup_word(tree->text, defs);
			reg_list* return_register = NULL;
			if (!d) yyerror("undefined word");
			registers = assert_registers(d->argc, d->needs_stack ? d->argc : LONG_MAX, registers);
			if (d->builtin && !strcmp(tree->text, "if") && registers && registers->next && registers->next->next
					&& registers->next->type == registers->next->next->type)
			{
				value_type t = registers->next->type;
				return_register = add_register(t, NULL);
				registers = emit_register(boolean, registers);
				fputs("?", outfile);
				registers = emit_register(t, registers);
				fputs(":", outfile);
				registers = emit_register(t, registers);
				fputs(";", outfile);
				return_register->next = registers;
				registers = return_register;
				break;
			}
			if (d->rets) return_register = add_register(d->ret, NULL);
			switch(d->type)
			{
				case func:
					fprintf(outfile,"%s(%s,(", release ? "CALL" : "CALLDEBUG", restrict_chars(d->name));
					for (unsigned short i = 0; i < d->argc; ++i)
					{
						registers = emit_register(d->args[i], registers);
						if (i + 1 < d->argc) fputc(',', outfile);
					}
					fputs("));", outfile);
					break;
				case var:
					fprintf(outfile, "VAR(%s);", restrict_chars(d->name));
					break;
				case stack_op:
					registers = d->stack_shuffle(registers);
					break;
			}
			if (d->rets)
			{
				return_register->next = registers;
				registers = return_register;
			}
		}
		break;
		case value:
		{
			reg_list* return_register = add_register(tree->val_type, registers);
			switch (tree->val_type)
			{
				case block:
					fputs("^{check_function_stack_size();", outfile);
					compile(tree->data, NULL, predeclare(tree->data, defs));
					fputs("}", outfile);
					break;
				case string:
					print_cognate_string(tree->text);
					break;
				case symbol:
					fprintf(outfile, "SYM(%s)", restrict_chars(tree->text));
					break;
				case number:
					if (strchr(tree->text, '.'))
						fprintf(outfile, "%sl", tree->text);
					else
						fprintf(outfile, "%s.0l", tree->text);
					break;
				default:
					fputs(tree->text, outfile);
					break;
			}
			fputc(';', outfile);
			registers = return_register;
		}
		break;
		case let:
		{
			registers = assert_registers(1, LONG_MAX, registers);
			decl_list* d = malloc(sizeof *d);
			*d = (decl_list)
			{
				.name = tree->text,
				.next = defs,
				.type = var,
				.ret = registers->type,
				.rets = true,
				.builtin = false
			};
			char* tag = "const";
			if (is_mutated(tree->next, *d)) d->ret = any, tag = "__block";
			fprintf(outfile, "%s %s VAR(%s)=", tag, type_as_str[d->ret][true], restrict_chars(d->name));
			if (d->ret == block) fputs("Block_copy(", outfile);
			registers = emit_register(d->ret, registers);
			if (d->ret == block) fputs(")", outfile);
			fputs(";{", outfile);
			defs = d;
			footer = "}";
		}
		break;
		case def:
		{
			decl_list* d = lookup_word(tree->text, defs);
			if (!d->predecl) yyerror("already defined in this block");
			d -> predecl = false;
			registers = assert_registers(1, LONG_MAX, registers);
			fprintf(outfile, "VAR(%s)=Block_copy(", restrict_chars(tree->text));
			registers = emit_register(block, registers);
			fputs(");{", outfile);
			footer = "}";
		}
		break;
		case set:
		{
			decl_list* d = lookup_word(tree->text, defs);
			if (d -> type == stack_op || d -> type == func) { yyerror("cannot mutate function"); }
			if (d -> predecl) fprintf(outfile, "VAR(%s);", restrict_chars(tree->text));
			registers = assert_registers(1, LONG_MAX, registers);
			fprintf(outfile, "SET(%s,", restrict_chars(tree->text));
			registers = emit_register(any, registers);
			fputs(");", outfile);
		}
	}
	// Free the ast node.
	compile(tree->next, registers, defs);
	fputs(footer, outfile);
}

reg_list* twin_register(reg_list* registers)
{
	registers = assert_registers(1, LONG_MAX, registers);
	reg_list* r1 = malloc(sizeof *r1);
	*r1 = *registers;
	r1->next = registers;
	return r1;
}

reg_list* drop_register(reg_list* registers)
{
	reg_list* r = assert_registers(1, LONG_MAX, registers);
	reg_list* next = r->next;
	return next;
}

reg_list* triplet_register(reg_list* registers)
{
	registers = assert_registers(1, LONG_MAX, registers);
	reg_list* r1 = malloc(sizeof *r1);
	reg_list* r2 = malloc(sizeof *r2);
	*r1 = *registers;
	*r2 = *registers;
	r1->next = registers;
	r2->next = r1;
	return r2;
}

reg_list* swap_register(reg_list* registers)
{
	registers = assert_registers(2, LONG_MAX, registers);
	reg_list* r1 = registers;
	reg_list* r2 = registers->next;
	r1->next = r2->next;
	r2->next = r1;
	return r2;
}

decl_list* builtins(void)
{
	static decl_list b[] =
	{
		#include "builtins.c"
	};
	for (size_t i = 0; i < sizeof(b) / sizeof(b[0]) - 1; ++i)
	{
		// This just creates a linked list.
		b[i].builtin = true;
		//b[i].name = restrict_chars(b[i].name);
		b[i].next = b + i + 1;
	}
	return b;
}

int main(int argc, char** argv)
{
	setlocale(LC_ALL, "");
	if (argc < 2 || !strchr(argv[1], '.') || strcmp(".cog", strchr(argv[1], '.'))) // Will not work with ./ or ../ filepaths TODO
	{
		fprintf(stderr, "Usage: %s filename.cog -option1 -option2\n", argv[0]);
		return EXIT_FAILURE;
	}
	bool run = false;
	char* source_file_path = argv[1];
	size_t len = strlen(source_file_path);
	char* c_file_path		  = strdup(source_file_path); c_file_path[len-2] = '\0';
	char* binary_file_path = strdup(source_file_path); binary_file_path[len-4] = '\0';
	outfile = fopen(c_file_path, "w");
	yyin = fopen(source_file_path, "r");
	if (!yyin) { fprintf(stderr, "File %s not found\n", source_file_path); return EXIT_FAILURE; }
	for (argv += 2 ; *argv ; argv++)
	{
		if			(!strcmp(*argv, "-output")) { binary_file_path = argv[1]; argv++; }
		else if (!strcmp(*argv, "-release")) release = true;
		else if (!strcmp(*argv, "-run")) { run = true; argv[0] = binary_file_path; /* TODO prepend path with ./ */ break; }
		else { fprintf(stderr, "Invalid option: %s\n", *argv); return EXIT_FAILURE; }
	}
	yyparse();
	fputs("#include<cognate.h>\n",outfile);
	fputs("char* record_info[][64] = {", outfile);
	emit_record_info(full_ast);
	fputs("{NULL}};\n", outfile);
	fprintf(outfile, "#line 1 \"%s\"\n", source_file_path);
	fputs("int main(int argc,char** argv){init(argc,argv);",outfile);
	add_symbols(full_ast);
	compile(full_ast, NULL, predeclare(full_ast, builtins()));
	fputs("cleanup();}\n", outfile);
	char* args[] =
	{
		"clang", c_file_path, "-o", binary_file_path, "-fblocks", "-I"INCLUDEDIR, "-lBlocksRuntime",
		"-lpthread", release ? "-Ofast" : "-O1", "-Wall", "-Wextra", "-Werror", "-Wno-unused", "-pedantic-errors",
		"-std=c11", "-lm", "-g0", release ? "-s" : "-ggdb3", "-fuse-ld=lld", NULL
	};
	fflush(outfile);
	if (fork() == 0) execvp(args[0], args); else wait(NULL);
	if (!run) return EXIT_SUCCESS;
	char prog_name[strlen(argv[0])];
	strcpy(prog_name, "./");
	strcpy(prog_name + 2, argv[0]);
	argv[0] = prog_name;
	execvp(argv[0], argv);
}
