#include "cognac.h"
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

FILE* outfile;
size_t current_register = 0;
ast* full_ast;

int col;
int line;

char* type_as_str(value_type typ, _Bool uppercase)
{
  switch(typ)
  {
    case any:     return uppercase ? "ANY"     : "any";
    case block:   return uppercase ? "BLOCK"   : "block";
    case string:  return uppercase ? "STRING"  : "string";
    case number:  return uppercase ? "NUMBER"  : "number";
    case symbol:  return uppercase ? "SYMBOL"  : "symbol";
    case boolean: return uppercase ? "BOOLEAN" : "boolean";
    case list:    return uppercase ? "LIST"    : "list";
  }
}

reg_list* flush_registers_to_stack(reg_list* registers, unsigned short exclude)
{
  /*
   * Flush all registers to the stack, except for a number of excluded registers.
   */
  if (registers)
  {
    if (exclude)
    {
      registers->next = flush_registers_to_stack(registers->next, --exclude);
      return registers;
    }
    flush_registers_to_stack(registers->next, 0);
    fputs("push(", outfile);
    get_register(any, registers);
    fputs(");", outfile);
  }
  return NULL;
}

decl_list* lookup_word(char* name, decl_list* defs)
{
  for (decl_list *ptr = defs; ptr; ptr = ptr->next)
  {
    if (strcmp(ptr->name, name) == 0) return ptr;
  }
  return NULL;
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

decl_list* predeclare(ast* tree, decl_list* defs)
{
  /*
   * Iterate over the ast, outputting a PREDEFINE for each function declaration.
   * Predefining means that functions can reference functions not yet declared.
   * However, these functions cannot be called until declaration.
   * TODO Function calls to guaranteed undeclared functions should be compile errors.
   */
  decl_list* already_predeclared = NULL;
  for (; tree; tree=tree->next)
  {
    col = tree->col;
    line = tree->line;
    if (tree->type == define || tree->type == let)
    {
      if (lookup_word(tree->text, already_predeclared))
      {
        // Do not allow same-block shadowing.
        yylloc.first_column = tree->col;
        yylloc.first_line = tree->line;
        yyerror("already defined in this block");
      }
      fprintf(outfile, "PREDEF_%s(%s);", tree->type == let ? "LET" : "DEFINE", tree->text);
      // We are leaking memory here.
      // This could be stack memory with alloca() is we moved the allocation.
      decl_list* def  = malloc(sizeof(*def));
      decl_list* def2 = malloc(sizeof(*def2));
      *def2 = *def = (decl_list){.type=(tree->type == let) ? var : func,
                                 .next = defs, .name = tree->text,
                                 .needs_stack = tree->type == let ? false : true,
                                 .rets = tree->type == let ? true : false,
                                 .ret = any, .predecl = true};
      def2->next = already_predeclared;
      defs = def;
      already_predeclared = def2;
    }
  }
  return defs;
}

reg_list* get_register(value_type type, reg_list* regs)
{
  if (regs)
  {
    if (regs->type == type)     fprintf(outfile, "r%zi",                                           regs->id);
    else if (regs->type == any) fprintf(outfile, "CHECK(%s,r%zi)", type_as_str(type, false),       regs->id);
    else if (type == any)       fprintf(outfile, "OBJ(%s,r%zi)",   type_as_str(regs->type, false), regs->id);
    else
    {
      char error_msg[256];
      sprintf(error_msg, "expected '%s' but got '%s'", type_as_str(type, false), type_as_str(regs->type, false));
      yylloc.first_column = col;
      yylloc.first_line = line;
      yyerror(error_msg);
    }
    return regs->next;
  }
  else
  {
    if (type == any) fputs("pop()", outfile);
    else fprintf(outfile, "CHECK(%s,pop())", type_as_str(type, false));
    return NULL;
  }
}

void compile(ast* tree, reg_list* registers, decl_list* defs)
{
  // TODO split this up into many smaller more maintainable functions.
  if (!tree)
  {
    flush_registers_to_stack(registers, 0);
    return;
  }
  col = tree->col;
  line = tree->line;
  switch (tree->type)
  {
    case identifier:
    {
      decl_list* def = lookup_word(tree->text, defs);
      if (!def)
      {
        yylloc.first_line = line;
        yylloc.first_column = col;
        yyerror("undefined word");
      }
      if (def->needs_stack) registers = flush_registers_to_stack(registers, def->argc);
      if (def->type == func)
      {
        if (def->rets) fprintf(outfile,"%s r%zi=", type_as_str(def->ret, true), current_register);
        fprintf(outfile,"CALL(%s,(", def->name);
        for (unsigned short i = 0; i < def->argc; ++i)
        {
          registers = get_register(def->args[i], registers);
          if (i + 1 < def->argc) fputc(',', outfile);
        }
        fputs("));", outfile);
      }
      else if (def->type == var)
      {
        if (def -> predecl) fprintf(outfile, "ANY r%zi=CHECK_VAR(%s);", current_register, def->name);
        else fprintf(outfile,"ANY r%zi=VAR(%s);", current_register, def->name);
      }
      if (def->rets)
      {
        reg_list new_reg;
        new_reg = (reg_list){.id=current_register++, .next=registers, .type=def->ret};
        compile(tree->next, &new_reg, defs);
      }
      else compile(tree->next, registers, defs);
    }
    break;
    case value:
    {
      size_t reg_id = current_register++;
      fprintf(outfile, "%s r%zi=", type_as_str(tree->val_type, true), reg_id);
      switch (tree->val_type)
      {
        case block:
          fputs("MAKE_BLOCK(", outfile);
          compile(tree->data, NULL, predeclare(tree->data, defs));
          tree->data = NULL; // Prevent double free.
          fputs(")", outfile);
          break;
        case string:
          print_cognate_string(tree->text);
          break;
        default:
          fputs(tree->text, outfile);
          break;
      }
      fputc(';', outfile);
      reg_list new_reg = {.id=reg_id, .next=registers, .type=tree->val_type};
      compile(tree->next, &new_reg, defs);
      break;
    }
    case let:
    {
      decl_list* d = lookup_word(tree->text, defs);
      d -> predecl = false;
      fprintf(outfile, "LET(%s,", tree->text);
      registers = get_register(any, registers);
      fputs(");{", outfile);
      // TODO when there are no registers to use, add function args instead of  using the stack.
      compile(tree->next, registers, defs);
      fputc('}', outfile);
    }
    break;
    case set: break; // TODO mutable variables.
    case define:
    {
      // TODO define macro takes function body as arg instead of a block for performance reasons.
      // We can set the function directly to the block, but we lose function name tracking.
      decl_list* d = lookup_word(tree->text, defs);
      d -> predecl = false;
      fprintf(outfile,"DEFINE(%s,", tree->text);
      registers = get_register(block, registers);
      fputs(");{",outfile);
      compile(tree->next, registers, defs);
      fputc('}',outfile);
    }
    break;
  }
  // Free the ast node.
  free(tree->text);
  free(tree);
}

decl_list* builtins(void)
{
  static decl_list b[] =
  {
    // List of all builtin declarations.
    // TODO put in a different file and #include it
    {.name="ADD", .type=func, .argc=2, .args={number, number}, .rets=true, .ret=number},
    {.name="SUB", .type=func, .argc=2, .args={number, number}, .rets=true, .ret=number},
    {.name="MUL", .type=func, .argc=2, .args={number, number}, .rets=true, .ret=number},
    {.name="DIV", .type=func, .argc=2, .args={number, number}, .rets=true, .ret=number},
    {.name="GT",  .type=func, .argc=2, .args={number, number}, .rets=true, .ret=boolean},
    {.name="GTE", .type=func, .argc=2, .args={number, number}, .rets=true, .ret=boolean},
    {.name="LT",  .type=func, .argc=2, .args={number, number}, .rets=true, .ret=boolean},
    {.name="LTE", .type=func, .argc=2, .args={number, number}, .rets=true, .ret=boolean},
    {.name="EQ",  .type=func, .argc=2, .args={any, any},       .rets=true, .ret=boolean},
    {.name="NEQ", .type=func, .argc=2, .args={any, any},       .rets=true, .ret=boolean},
    {.name="if",    .type=func, .argc=3, .args={block, any, any},.rets=true, .ret=any, .needs_stack=true},
    {.name="while", .type=func, .argc=2, .args={block, block}, .needs_stack=true},
    {.name="print", .type=func, .argc=1, .args={any}},
    {.name="put",   .type=func, .argc=1, .args={any}},
    {.name="do", .type=func, .argc=1, .args={block}, .needs_stack=true},
    {.name="random", .type=func, .argc=3, .args={number, number, number}, .rets=true, .ret=number},
    {.name="modulo", .type=func, .argc=2, .args={number, number}, .rets=true, .ret=number},
    {.name="drop", .type=func, .argc=1, .args={any}},
    {.name="twin", .type=func, .argc=1, .args={any}, .rets=true, .ret=any},
    {.name="triplet", .type=func, .argc=1, .args={any}, .rets=true, .ret=any},
    {.name="swap", .type=func, .argc=2, .args={any,any}, .rets=true, .ret=any},
    {.name="clear", .type=func, .argc=0, .needs_stack=true},
    {.name="true", .type=var, .rets=true, .ret=any},
    {.name="false", .type=var, .rets=true, .ret=any},
    {.name="either", .type=func, .argc=2, .args={boolean, boolean}, .rets=true, .ret=boolean},
    {.name="both", .type=func, .argc=2, .args={boolean, boolean}, .rets=true, .ret=boolean},
    {.name="one_of", .type=func, .argc=2, .args={boolean, boolean}, .rets=true, .ret=boolean},
    {.name="not", .type=func, .argc=1, .args={boolean}, .rets=true, .ret=boolean},
    {.name="number_", .type=func, .argc=1, .args={any}, .rets=true, .ret=boolean},
    {.name="list_", .type=func, .argc=1, .args={any}, .rets=true, .ret=boolean},
    {.name="string_", .type=func, .argc=1, .args={any}, .rets=true, .ret=boolean},
    {.name="block_", .type=func, .argc=1, .args={any}, .rets=true, .ret=boolean},
    {.name="boolean_", .type=func, .argc=1, .args={any}, .rets=true, .ret=boolean},
    {.name="first", .type=func, .argc=1, .args={list}, .rets=true, .ret=any},
    {.name="rest", .type=func, .argc=1, .args={list}, .rets=true, .ret=list},
    {.name="head", .type=func, .argc=1, .args={string}, .rets=true, .ret=string},
    {.name="tail", .type=func, .argc=1, .args={string}, .rets=true, .ret=string},
    {.name="push", .type=func, .argc=2, .args={any, list}, .rets=true, .ret=list},
    {.name="empty_", .type=func, .argc=1, .args={list}, .rets=true, .ret=boolean},
    {.name="join", .type=func, .argc=1, .args={number}, .rets=true, .ret=string, .needs_stack=true},
    {.name="string_length", .type=func, .argc=1, .args={string}, .rets=true, .ret=number},
    {.name="substring", .type=func, .argc=3, .args={number, number, string}, .rets=true, .ret=string},
    {.name="match", .type=func, .argc=2, .args={string, string}, .rets=true, .ret=boolean},
    {.name="ordinal", .type=func, .argc=1, .args={string}, .rets=true, .ret=number},
    {.name="character", .type=func, .argc=1, .args={number}, .rets=true, .ret=string},
    {.name="floor", .type=func, .argc=1, .args={number}, .rets=true, .ret=number},
    {.name="round", .type=func, .argc=1, .args={number}, .rets=true, .ret=number},
    {.name="ceiling", .type=func, .argc=1, .args={number}, .rets=true, .ret=number},
    {.name="assert", .type=func, .argc=1, .args={string,boolean}},
    {.name="error", .type=func, .argc=1, .args={string}},
    {.name="path", .type=func, .rets=true, .ret=string},
    {.name="stack", .type=func, .rets=true, .ret=list, .needs_stack=true},
    {.name="parameters", .type=func, .rets=true, .ret=list},
    {.name="input", .type=func, .rets=true, .ret=string},
    {.name="read", .type=func, .argc=1, .args={string}, .rets=true, .ret=string},
    {.name="write", .type=func, .argc=1, .args={string,any}},
    {.name="list", .type=func, .argc=1, .args={block}, .rets=true, .ret=list},
    {.name="number", .type=func, .argc=1, .args={string}, .rets=true, .ret=number},
    {.name="stop"},
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
  if (argc != 2)
  {
    puts("Invalid number of args!");
    return EXIT_FAILURE;
  }
  yyin = fopen(argv[1], "r"); // TODO handle file not found error.
  // TODO this assumes that yyin ends in .cog
  char outfile_name[strlen(argv[1])+1];
  char binary_name[strlen(argv[1])+1];
  strcpy(outfile_name, argv[1]);
  strcpy(binary_name, argv[1]);
  outfile_name[strlen(argv[1])-2] = '\0';
  binary_name[strlen(argv[1])-4] = '\0';
  outfile = fopen(outfile_name, "w");
  yyparse();
  fputs("#include\"cognate.h\"\nPROGRAM(",outfile);
  compile(full_ast, NULL, predeclare(full_ast, builtins()));
  fputs(")\n", outfile);
  char args[] = "clang %s -o %s -fblocks -I. runtime.c functions.c -lBlocksRuntime"
                " -l:libgc.so -Ofast -Wall -Wextra -Werror -Wno-unused"
                " -pedantic-errors -std=c11 -lm -g0 -fuse-ld=lld -flto=full";
  char cmd[strlen(args) + strlen(outfile_name) + strlen(binary_name) - strlen("%s")*2 + 1];
  sprintf(cmd, args, outfile_name, binary_name);
  fflush(outfile);
  system(cmd);
}
