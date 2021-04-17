#pragma once

#include "parser.tab.h"
#include <stdio.h>
#include <stdbool.h>

// OR types so we can do bitwise typechecking? TODO?
typedef enum {block, string, number, symbol, boolean, list, any} value_type;

typedef enum {identifier, value, define, let, set} token_type;

typedef enum {func, var} decl_type;

typedef struct decl_list
{
  struct decl_list* next;
  char* name;
  value_type args[3];
  value_type ret;
  decl_type type;
  unsigned short argc;
  _Bool needs_stack;
  _Bool rets;
  _Bool mut;
  // TODO flags and a bitmask for boolean things?
} decl_list;

typedef struct ast
{
  union
  {
    char* text;
    struct ast* child;
    void* data;
  };
  size_t line;
  size_t col;
  struct ast* next;
  token_type type;
  value_type val_type;
} ast;

typedef struct reg_list
{
  size_t id;
  struct reg_list* next;
  value_type type;
} reg_list;

extern FILE* yyin;
extern FILE* outfile;
extern size_t current_register;
extern ast* full_ast;

decl_list* builtins(void);
decl_list* lookup_word(char*, decl_list*);
void compile(ast*, reg_list*, decl_list*); // TODO output file arg.
void print_cognate_string(char*);
decl_list* predefine(ast*, decl_list*);
reg_list* flush_registers_to_stack(reg_list*, unsigned short);

int yylex(void);
void yyerror(char*);
char* lc(char*);
