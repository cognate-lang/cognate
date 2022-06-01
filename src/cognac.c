#include "cognac.h"
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <locale.h>
#include <unistd.h>
#include <sys/wait.h>
#include <limits.h>

#include "runtime.h"

int record_id = 0;

FILE* outfile;
size_t current_register = 0;
sym_list* declared_symbols = NULL;
ast* full_ast;
bool release = false;
bool gc_test = false;

char * restrict_chars(char* in)
{
	size_t len = strlen(in);
	char* out = strdup(in);
	for (size_t index = 0; index < len; index++)
	{
		switch(in[index])
		{
			case '-':
				out[index] = 'D';
				break;
			case '!':
				out[index] = 'X';
				break;
			case '?':
				out[index] = 'Q';
				break;
			case '=':
				out[index] = 'E';
				break;
			case '<':
				out[index] = 'L';
				break;
			case '>':
				out[index] = 'G';
				break;
			case '+':
				out[index] = 'P';
				break;
			case '*':
				out[index] = 'M';
				break;
			case '/':
				out[index] = 'S';
				break;
			default:
				break;
		}
	}
	return out;
}

char* type_as_str[9][2] =
{
	[any]     = { "any",		 "ANY"	 },
	[block]   = { "block",	 "BLOCK"	 },
	[string]  = { "string",  "STRING" },
	[number]  = { "number",  "NUMBER" },
	[symbol]  = { "symbol",  "SYMBOL" },
	[boolean] = { "boolean", "BOOLEAN"},
	[list]    = { "list",	 "LIST"	 },
	[record]  = { "record",	 "RECORD" },
	[box]     = { "box",		 "BOX"	 },
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
		emit_register(any, registers);
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
					char* str = restrict_chars(tree->text);
					fprintf(outfile, "const SYMBOL SYM(%s)=\"%s\";", str, tree->text);
					free(str);
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
					case 'e': fputs("\\033", outfile); break;
					case 'n':
					case 'r':
					case 't':
					case 'v':
					case 'f':
					case 'a':
					case 'b':
						fprintf(outfile, "\\%c", str[i+1]);
						break;
					case '\\': fputs("\\\\", outfile); break;
					case '"': fputs("\\\"", outfile); break;
					default: yyerror("invalid escape sequence");
				}
				i+=2;
				break;
			case '\n':
				fputs("\\n", outfile);
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
			char* str = restrict_chars(tree->text);
			fprintf(outfile, "PREDEF(%s);", str);
			free(str);
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


reg_list* emit_register(value_type type, reg_list* regs)
{
	if (!regs) __builtin_trap();
	else if (regs->type == type)fprintf(outfile, "r%zi", (ssize_t)regs->id);
	else if (regs->type == any) fprintf(outfile, "unbox_%s(r%zi)", type_as_str[type][false], (ssize_t)regs->id);
	else if (type == any)       fprintf(outfile, "box_%s(r%zi)", type_as_str[regs->type][false], (ssize_t)regs->id);
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
	fprintf(outfile, "const %s r%zi=", type_as_str[r->type][true], (ssize_t)r->id);
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
	static size_t lineno = 0;
	if (!release && lineno != tree->line)
	{
		lineno = tree->line;
		fprintf(outfile, "check_breakpoint(%zu);", tree->line);
	}
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

			char* predname = malloc(strlen(tree->record->name)+2);
			predname[0]='\0';
			strcpy(predname, tree->record->name);
			strcat(predname, "?");
			decl_list* pred = malloc(sizeof *pred);
			*pred = (decl_list)
			{
				.name = predname,
				.next = defs,
				.type = func,
				.needs_stack = false,
				.rets = true,
				.ret = boolean,
				.builtin = false,
				.argc = 1,
				.args={any}
			};


			fprintf(outfile, "BOOLEAN(^VAR(%sQMARK))(ANY) = Block_copy(^BOOLEAN(ANY R){return get_type(R)==record&&unbox_record(R)->id==%i;});", cons->name, record_id);

			defs = pred;


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
			fputs("{", outfile);
			footer = "}";

		}
		break;
		case identifier:
		{
			decl_list* d = lookup_word(tree->text, defs);
			if (!d) yyerror("undefined word");
			if (!release)
			{
				registers = assert_registers(0, 0, registers);
				fprintf(outfile, "BACKTRACE_PUSH(%s,%zu,%zu);", d->name,tree->line,tree->col);
				fprintf(outfile,"debugger_step();");
			}
			reg_list* return_register = NULL;
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
			char* res = restrict_chars(d->name);
			switch(d->type)
			{
				case func:
					fprintf(outfile,"CALL(%s,(", res);
					for (unsigned short i = 0; i < d->argc; ++i)
					{
						registers = emit_register(d->args[i], registers);
						if (i + 1 < d->argc) fputc(',', outfile);
					}
					fputs("));", outfile);
					break;
				case var:
					fprintf(outfile, "VAR(%s);", res);
					break;
				case stack_op:
					registers = d->stack_shuffle(registers);
					break;
			}
			free(res);
			if (d->rets)
			{
				return_register->next = registers;
				registers = return_register;
			}
			if (!release) fputs("BACKTRACE_POP();", outfile);
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
				{
					char* str = restrict_chars(tree->text);
					fprintf(outfile, "SYM(%s)", str);
					free(str);
					break;
				}
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
			if (!release)
			{
				registers = assert_registers(0, 0, registers);
				fprintf(outfile, "BACKTRACE_PUSH(%s,%zu,%zu);",tree->text, tree->line,tree->col);
				fprintf(outfile,"debugger_step();");
			}
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
			char* name = restrict_chars(d->name);
			fprintf(outfile, "const %s VAR(%s)=", type_as_str[d->ret][true], name);
			if (d->ret == block) fputs("Block_copy(", outfile);
			registers = emit_register(d->ret, registers);
			if (d->ret == block) fputs(")", outfile);
			fputs(";", outfile);
			if (!release)
			{
				fprintf(outfile, "VARS_PUSH(%s, %s);", d->name, name);
				fputs("BACKTRACE_POP();", outfile);
			}
			fputs("{", outfile);
			defs = d;
			footer = release ? "}" : "VARS_POP();}";
			free(name);
		}
		break;
		case def:
		{
			if (!release)
			{
				registers = assert_registers(0, 0, registers);
				fprintf(outfile, "BACKTRACE_PUSH(%s,%zu,%zu);",tree->text,tree->line,tree->col);
				fprintf(outfile,"debugger_step();");
			}
			decl_list* d = lookup_word(tree->text, defs);
			if (!d->predecl) yyerror("already defined in this block");
			d -> predecl = false;
			registers = assert_registers(1, LONG_MAX, registers);
			char* name = restrict_chars(tree->text);
			fprintf(outfile, "VAR(%s)=Block_copy(", name);
			free(name);
			registers = emit_register(block, registers);
			fputs(");", outfile);
			if (!release) fputs("BACKTRACE_POP()", outfile);
			fputs("{", outfile);
			footer = "}";
		}
		break;
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

void emit_source_string(FILE* yyin)
{
	int buf_sz = 255;
	char buf[buf_sz];

	if (release) return;
	fputs("char* source_file_lines[]={", outfile);

	rewind(yyin);
	size_t lines = 0;
	while(fgets(buf, buf_sz, yyin)) {
		lines ++;
		fputs("\"", outfile);
		for (size_t i = 0; buf[i] != '\n' && buf[i];)
		{
			int len = mblen(buf+i, MB_CUR_MAX);
			if (len == 1)
			{
				switch(buf[i])
				{
					case '\\':
					case '"':
						fputs("\\", outfile);
						break;
					default:;
				}
			}
			fprintf(outfile, "%.*s", len, buf + i);
			i += len;
		}

		fputs("\",",outfile);
	}

	fputs("NULL};\n", outfile);
	fprintf(outfile, "const size_t source_line_num=%zu;", lines);
	fprintf(outfile, "_Bool breakpoints[%zu] = {0};", lines);
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
	yyin = fopen(source_file_path, "r");
	if (!yyin) { fprintf(stderr, "File %s not found\n", source_file_path); return EXIT_FAILURE; }
	outfile = fopen(c_file_path, "w");
	for (argv += 2 ; *argv ; argv++)
	{
		if			(!strcmp(*argv, "-output")) { binary_file_path = argv[1]; argv++; }
		else if (!strcmp(*argv, "-release")) release = true;
		else if (!strcmp(*argv, "-run")) { run = true; argv[0] = binary_file_path; /* TODO prepend path with ./ */ break; }
		else { fprintf(stderr, "Invalid option: %s\n", *argv); return EXIT_FAILURE; }
	}
	yyparse();
	//fputs("#include<cognate.c>\n",outfile);
	if (!release) fputs("#define DEBUG 1\n", outfile);
	fprintf(outfile, "%.*s", src_runtime_c_len, (char*)src_runtime_c);
	fputs("char* record_info[][64] = {", outfile);
	emit_record_info(full_ast);
	fputs("{NULL}};\n", outfile);
	emit_source_string(yyin);
	fputs("int main(int argc,char** argv){init(argc,argv);",outfile);
	add_symbols(full_ast);
	compile(full_ast, NULL, predeclare(full_ast, builtins()));
	fputs("cleanup();}\n", outfile);
	fflush(outfile);
	int status;
	if (fork() == 0)
	{
		char* args[] =
		{
			"clang", c_file_path, "-o", binary_file_path, "-fblocks",
#ifndef __APPLE__
			"-lBlocksRuntime",
#endif
			"-lpthread", release ? "-Ofast" : "-O1", "-Wall", "-Wextra", "-Werror", "-Wno-unused", "-pedantic-errors",
			"-std=c11", "-lm", "-g0", "-s", NULL
			// "-flto" and "-fuse-ld=lld" give smaller binaries but make installation a pain
		};
		execvp(args[0], args);
	}
	else wait(&status);
	//free(c_file_path);
	//free(binary_file_path);
	if (!run || status) return status;
	char prog_name[strlen(argv[0])+3];
	strcpy(prog_name, "./");
	strcpy(prog_name + 2, argv[0]);
	argv[0] = prog_name;
	execvp(argv[0], argv);
}
