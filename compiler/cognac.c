#include "cognac.h"
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <limits.h>

FILE* outfile;
size_t current_register = 0;
size_t current_symbol = 0;
size_t num_symbols = 0;
sym_list* declared_symbols = NULL;
ast* full_ast;
bool optimize = false;
bool debug = false;

char* type_as_str(value_type typ, _Bool up)
{
  switch (typ)
  {
    case any:     return up ? "ANY"     : "any";
    case block:   return up ? "BLOCK"   : "block";
    case string:  return up ? "STRING"  : "string";
    case number:  return up ? "NUMBER"  : "number";
    case symbol:  return up ? "SYMBOL"  : "symbol";
    case boolean: return up ? "BOOLEAN" : "boolean";
    case list:    return up ? "LIST"    : "list";
  }
}

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
  while (tree)
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
          fprintf(outfile, "const SYMBOL SYM(%s)=%zi;symtable[%zi]=\"%s\";", tree->text, current_symbol, current_symbol, tree->text);
          ++current_symbol;
          sym_list* s = malloc(sizeof(*s));
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
    tree = tree->next;
  }
}

void print_cognate_string(char* str)
{
  fputc('"', outfile);
  for (size_t i = 1; str[i + 1] != '\0'; ++i)
  {
    if (str[i] == '"') fputc('\\', outfile);
    fputc(str[i], outfile);
  }
  fputc('"', outfile);
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
    if (tree->type == define)
    {
      if (lookup_word(tree->text, already_predeclared)) yyerror("already defined in this block");
      // We are leaking memory here.
      // This could be stack memory with alloca() is we moved the allocation.
      decl_list* def  = malloc(sizeof(*def));
      decl_list* def2 = malloc(sizeof(*def2));
      *def = (decl_list){.type=(tree->type == let) ? var : func,
                         .next = defs, .name = tree->text,
                         .needs_stack = true,
                         .rets = false,
                         .ret = any, .predecl = true, .mut=mutable};
      fprintf(outfile, "PREDEF(%s);", tree->text);
      // We use already_predeclared to prevent shadowing.
      *def2 = *def;
      def2->next = already_predeclared;
      defs = def;
      already_predeclared = def2;
    }
  }
  while (already_predeclared)
  {
    decl_list* tmp = already_predeclared;
    already_predeclared = already_predeclared->next;
    free(tmp);
  }
  return defs;
}

bool is_mutated(ast* tree, decl_list def)
{
  for (; tree ; tree = tree->next)
  {
    switch(tree->type)
    {
      case set: if (!strcmp(def.name, tree->text)) return true; break;
      case let:
      case define: if (!strcmp(def.name, tree->text)) return false; break;
      case value: if (tree->val_type == block && is_mutated(tree->child, def)) return true; break;
      default:;
    }
  }
  return false;
}

reg_list* emit_register(value_type type, reg_list* regs)
{
  if (regs->type == type)     fprintf(outfile, "r%zi",                                           regs->id);
  else if (regs->type == any) fprintf(outfile, "CHECK(%s,r%zi)", type_as_str(type, false),       regs->id);
  else if (type == any)       fprintf(outfile, "OBJ(%s,r%zi)",   type_as_str(regs->type, false), regs->id);
  else
  {
    char error_msg[256];
    sprintf(error_msg, "expected %s got %s", type_as_str(type, false), type_as_str(regs->type, false));
    yyerror(error_msg);
  }
  return drop_register(regs);
}

reg_list* add_register(value_type type, reg_list* next)
{
  reg_list* r = malloc(sizeof(*r));
  *r = (reg_list){.type = type, .id = current_register++, .next=next};
  fprintf(outfile, "const %s r%zi=", type_as_str(r->type, true), r->id);
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
  if (debug) fprintf(outfile, "\n#line %zi\n", tree->line);
  switch (tree->type)
  {
    case identifier:
    {
      decl_list* def = lookup_word(tree->text, defs);
      reg_list* return_register = NULL;
      if (!def) yyerror("undefined word");
      registers = assert_registers(def->argc, def->needs_stack ? def->argc : LONG_MAX, registers);
      if (def->rets) return_register = add_register(def->ret, NULL);
      switch(def->type)
      {
        case func:
          fprintf(outfile,"CALL(%s,(", def->name);
          for (unsigned short i = 0; i < def->argc; ++i)
          {
            registers = emit_register(def->args[i], registers);
            if (i + 1 < def->argc) fputc(',', outfile);
          }
          fputs("));", outfile);
          break;
        case var:
          fprintf(outfile, "VAR(%s);", def->name);
          break;
        case stack_op:
          registers = def->stack_shuffle(registers);
          break;
      }
      if (def->rets)
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
          tree->data = NULL; // Prevent double free.
          fputc('}', outfile);
          break;
        case string:
          print_cognate_string(tree->text);
          break;
        case symbol:
          fprintf(outfile, "SYM(%s)", tree->text);
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
      decl_list d = (decl_list)
      {
        .name = tree->text,
        .next = defs,
        .ret = registers ? registers -> type : any,
        .type = var,
        .rets = true
      };
      bool mutated = is_mutated(tree->next, d);
      if (mutated) d.ret = any;
      registers = assert_registers(1, LONG_MAX, registers);
      fprintf(outfile, "%s %s VAR(%s)=", mutated ? "__block" : "const", type_as_str(d.ret, true), d.name);
      registers = emit_register(d.ret, registers);
      fputs(";{", outfile);
      defs = &d;
      footer = "}";
    }
    break;
    case define:
    {
      decl_list* d = lookup_word(tree->text, defs);
      d -> predecl = false;
      registers = assert_registers(1, LONG_MAX, registers);
      fprintf(outfile, "VAR(%s)=", tree->text);
      registers = emit_register(block, registers);
      fputs(";{", outfile);
      footer = "}";
    }
    break;
    case set:
    {
      decl_list* d = lookup_word(tree->text, defs);
      if (d -> type == stack_op || d -> type == func) { yyerror("cannot mutate function"); }
      if (d -> predecl) fprintf(outfile, "VAR(%s);", tree->text);
      registers = assert_registers(1, LONG_MAX, registers);
      fprintf(outfile, "SET(%s,", tree->text);
      registers = emit_register(any, registers);
      fputs(");", outfile);
    }
  }
  // Free the ast node.
  compile(tree->next, registers, defs);
  fputs(footer, outfile);
  free(tree->data);
  free(tree);
}

reg_list* twin_register(reg_list* registers)
{
  registers = assert_registers(1, LONG_MAX, registers);
  reg_list* r1 = malloc(sizeof(*r1));
  *r1 = *registers;
  r1->next = registers;
  return r1;
}

reg_list* drop_register(reg_list* registers)
{
  reg_list* r = assert_registers(1, LONG_MAX, registers);
  reg_list* next = r->next;
  free(r);
  return next;
}

reg_list* triplet_register(reg_list* registers)
{
  registers = assert_registers(1, LONG_MAX, registers);
  reg_list* r1 = malloc(sizeof(*r1));
  reg_list* r2 = malloc(sizeof(*r2));
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
    b[i].next = b + i + 1;
  }
  return b;
}

int main(int argc, char** argv)
{
  if (argc < 2 || !strchr(argv[1], '.') || strcmp(".cog", strchr(argv[1], '.')))
  {
    fprintf(stderr, "Usage: %s filename.cog -option1 -option2\n", argv[0]);
    return EXIT_FAILURE;
  }
  bool run = false;
  char* source_file_path = argv[1];
  size_t len = strlen(source_file_path);
  char* c_file_path      = strdup(source_file_path); c_file_path[len-2] = '\0';
  char* binary_file_path = strdup(source_file_path); binary_file_path[len-4] = '\0';
  outfile = fopen(c_file_path, "w");
  yyin = fopen(source_file_path, "r");
  if (!yyin) { fprintf(stderr, "File %s not found\n", source_file_path); return EXIT_FAILURE; }
  for (argv += 2 ;; argv++)
  {
    char* opt = *argv;
    if (!opt) break;
    else if (!strcmp(opt, "-output")) { binary_file_path = argv[1]; argv++; }
    else if (!strcmp(opt, "-optimize")) optimize = true;
    else if (!strcmp(opt, "-debug")) debug = true;
    else if (!strcmp(opt, "-run")) { run = true; argv[0] = binary_file_path; /* TODO prepend path with ./ */ break; }
    else { fprintf(stderr, "Invalid option: %s\n", opt); return EXIT_FAILURE; }
  }
  yyparse();
  fputs("#include\"runtime.h\"\n",outfile);
  if (debug) fprintf(outfile, "#line 1 \"%s\"\n", source_file_path);
  fprintf(outfile, "const char* symtable[%zi]={0};", num_symbols + 2);
  fputs("int main(int argc,char** argv){init(argc,argv);",outfile);
  add_symbols(full_ast);
  compile(full_ast, NULL, predeclare(full_ast, builtins()));
  fputs("cleanup();}\n", outfile);
#ifdef __APPLE__
  char* args[] = { "clang", c_file_path, "-o", binary_file_path, "-fblocks", "-Iruntime", "runtime/runtime.o", "runtime/functions.o",
                   "-lgc", optimize ? "-Ofast" : "-O0", "-Wall", "-Wextra", "-Werror", "-Wno-unused", "-pedantic-errors",
                   "-std=c11", "-lm", "-g0", optimize ? "-flto" : "-fno-lto", debug ? "-g" : "", NULL };
#else
  char* args[] = { "clang", c_file_path, "-o", binary_file_path, "-fblocks", "-Iruntime", "runtime/runtime.o", "runtime/functions.o", "-lBlocksRuntime",
                   "-l:libgc.so", optimize ? "-Ofast" : "-O0", "-Wall", "-Wextra", "-Werror", "-Wno-unused", "-pedantic-errors",
                   "-std=c11", "-lm", "-g0", "-fuse-ld=lld", optimize ? "-flto" : "-fno-lto", debug ? "-ggdb3" : "", NULL };
#endif
  fflush(outfile);
  if (fork() == 0) execvp(args[0], args); else wait(NULL);
  if (run) execvp(argv[0], argv);
}
