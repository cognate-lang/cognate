#include "cognac.h"
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <limits.h>
#include <gc/gc.h>

FILE* outfile;
size_t current_register = 0;
sym_list* declared_symbols = NULL;
ast* full_ast;
bool release = false;

char* restrict_chars(char* in)
{
  // Sanitise variable names so they won't upset the C compiler.
  // This could be a lot faster
  char replace[] = {'-', '!', '?', '=', '<', '>', '+', '*', '/'};
  char* with[]   = {"DASH", "XMARK", "QMARK", "EQ", "LT", "GT", "PLUS", "STAR", "SLASH"};
  size_t sz = 1;
  for (char* ptr = in; *ptr; ++ptr)
  {
    for (size_t i = 0; i < sizeof replace; ++i)
    {
      if (*ptr == replace[i]) sz += strlen(with[i]);
      else ++sz;
    }
  }
  char* out = GC_MALLOC(sz);
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
  [any]     = {  "any",    "ANY"     },
  [block]   = { "block",   "BLOCK"   },
  [string]  = { "string",  "STRING"  },
  [number]  = { "number",  "NUMBER"  },
  [symbol]  = { "symbol",  "SYMBOL"  },
  [boolean] = { "boolean", "BOOLEAN" },
  [list]    = { "list",    "LIST"    },
  [group]   = { "group",   "GROUP"   },
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
          sym_list* s = GC_NEW(sym_list);
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
      decl_list* def  = GC_NEW(decl_list);
      decl_list* def2 = GC_NEW(decl_list);
      *def = (decl_list){.type=(tree->type == let) ? var : func,
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

bool is_mutated(ast* tree, decl_list def)
{
  for (; tree ; tree = tree->next)
  {
    switch(tree->type)
    {
      case set: if (!strcmp(def.name, tree->text)) return true; break;
      case let: case define: if (!strcmp(def.name, tree->text)) return false; break;
      case value: if (tree->val_type == block && is_mutated(tree->child, def)) return true; break;
      default:;
    }
  }
  return false;
}

bool needs_block_tag(ast* tree, decl_list def)
{
  for (; tree ; tree = tree->next)
  {
    if (tree->type == value && tree->val_type == block && is_mutated(tree->child, def)) return true;
  }
  return false;
}

reg_list* emit_register(value_type type, reg_list* regs)
{
  if (regs->type == type)     fprintf(outfile, "r%zi",                                           regs->id);
  else if (regs->type == any) fprintf(outfile, "CHECK(%s,r%zi)", type_as_str[type][false],       regs->id);
  else if (type == any)       fprintf(outfile, "OBJ(%s,r%zi)",   type_as_str[regs->type][false], regs->id);
  else
  {
    char error_msg[256];
    sprintf(error_msg, "expected %s got %s", type_as_str[type][false], type_as_str[regs->type][false]);
    yyerror(error_msg);
  }
  return drop_register(regs);
}

reg_list* add_register(value_type type, reg_list* next)
{
  reg_list* r = GC_NEW(reg_list);
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
  if (!release) fprintf(outfile, "\n#line %zi\n", tree->line);
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
          fprintf(outfile,"%s(%s,(", release ? "CALL" : "CALLDEBUG", restrict_chars(def->name));
          for (unsigned short i = 0; i < def->argc; ++i)
          {
            registers = emit_register(def->args[i], registers);
            if (i + 1 < def->argc) fputc(',', outfile);
          }
          fputs("));", outfile);
          break;
        case var:
          fprintf(outfile, "VAR(%s);", restrict_chars(def->name));
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
      if (tree->val_type == block && tree->next && tree->next->type == value && tree->next->val_type == block
        && tree->next->next && tree->next->next->type == value && tree->next->next->val_type == block
        && tree->next->next->next && tree->next->next->next->type == identifier
        && tree->next->next->next->next && tree->next->next->next->next->type == identifier)
      {
        decl_list* ifdef = lookup_word(tree->next->next->next->text, defs);
        decl_list* dodef = lookup_word(tree->next->next->next->next->text, defs);
        if (ifdef && ifdef->builtin && strcmp(ifdef->name, "if") == 0
         && dodef && dodef->builtin && strcmp(dodef->name, "do") == 0)
        {
          registers = assert_registers(0, 0, registers);
          fputs("{", outfile);
          compile(tree->next->next->data, NULL, predeclare(tree->next->next->data, defs));
          fputs("}if(CHECK(boolean,pop())){", outfile);
          compile(tree->next->data, NULL, predeclare(tree->next->data, defs));
          fputs("}else{", outfile);
          compile(tree->data, NULL, predeclare(tree->next->next->data, defs));
          fputs("}", outfile);
          tree = tree->next->next->next->next;
        }
        break;
      }
      reg_list* return_register = add_register(tree->val_type, registers);
      switch (tree->val_type)
      {
        case block:
          fputs("^{check_function_stack_size();", outfile);
          compile(tree->data, NULL, predeclare(tree->data, defs));
          fputc('}', outfile);
          break;
        case string:
          print_cognate_string(tree->text);
          break;
        case symbol:
          fprintf(outfile, "SYM(%s)", restrict_chars(tree->text));
          break;
        case number:
          fprintf(outfile, "%sl", tree->text);
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
      decl_list* d = GC_NEW(decl_list);
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
      if (is_mutated(tree->next, *d)) d->ret = any, tag = "";
      if (needs_block_tag(tree->next, *d)) tag = "__block";
      fprintf(outfile, "%s %s VAR(%s)=", tag, type_as_str[d->ret][true], restrict_chars(d->name));
      registers = emit_register(d->ret, registers);
      fputs(";{", outfile);
      defs = d;
      footer = "}";
    }
    break;
    case define:
    {
      decl_list* d = lookup_word(tree->text, defs);
      d -> predecl = false;
      registers = assert_registers(1, LONG_MAX, registers);
      fprintf(outfile, "VAR(%s)=", restrict_chars(tree->text));
      registers = emit_register(block, registers);
      fputs(";{", outfile);
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
  reg_list* r1 = GC_NEW(reg_list);
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
  reg_list* r1 = GC_NEW(reg_list);
  reg_list* r2 = GC_NEW(reg_list);
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
  if (argc < 2 || !strchr(argv[1], '.') || strcmp(".cog", strchr(argv[1], '.'))) // Will not work with ./ or ../ filepaths TODO
  {
    fprintf(stderr, "Usage: %s filename.cog -option1 -option2\n", argv[0]);
    return EXIT_FAILURE;
  }
  bool run = false;
  char* source_file_path = argv[1];
  size_t len = strlen(source_file_path);
  char* c_file_path      = GC_STRDUP(source_file_path); c_file_path[len-2] = '\0';
  char* binary_file_path = GC_STRDUP(source_file_path); binary_file_path[len-4] = '\0';
  outfile = fopen(c_file_path, "w");
  yyin = fopen(source_file_path, "r");
  if (!yyin) { fprintf(stderr, "File %s not found\n", source_file_path); return EXIT_FAILURE; }
  for (argv += 2 ; *argv ; argv++)
  {
    if      (!strcmp(*argv, "-output")) { binary_file_path = argv[1]; argv++; }
    else if (!strcmp(*argv, "-release")) release = true;
    else if (!strcmp(*argv, "-run")) { run = true; argv[0] = binary_file_path; /* TODO prepend path with ./ */ break; }
    else { fprintf(stderr, "Invalid option: %s\n", *argv); return EXIT_FAILURE; }
  }
  yyparse();
  fputs("#include<cognate/runtime.h>\n",outfile);
  if (!release) fprintf(outfile, "#line 1 \"%s\"\n", source_file_path);
  fputs("int main(int argc,char** argv){init(argc,argv);",outfile);
  add_symbols(full_ast);
  compile(full_ast, NULL, predeclare(full_ast, builtins()));
  fputs("cleanup();}\n", outfile);
  char* args[] = { "clang", c_file_path, "-o", binary_file_path, "-fblocks", "-L/usr/local/lib", "-l:libcognate.a",
                   "-lgc", release ? "-Ofast" : "-O1", "-Wall", "-Wextra", "-Werror", "-Wno-unused", "-pedantic-errors",
                   "-std=c11", "-lm", "-g0", "-flto", release ? "-s" : "-ggdb3", "-fuse-ld=lld", NULL };
  fflush(outfile);
  if (fork() == 0) execvp(args[0], args); else wait(NULL);
  if (!run) return EXIT_SUCCESS;
  char prog_name[strlen(argv[0])];
  strcpy(prog_name, "./");
  strcpy(prog_name + 2, argv[0]);
  argv[0] = prog_name;
  execvp(argv[0], argv);
}
