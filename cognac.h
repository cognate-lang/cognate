#pragma once

#define _GNU_SOURCE
#define _FORTIFY_SOURCE 2

#include "parser.tab.h"
#include <stdio.h>
#include <stdbool.h>

// OR types so we can do bitwise typechecking? TODO?
typedef enum {block, string, number, symbol, boolean, list, any} value_type;

typedef enum {identifier, value, define, let, set} token_type;

typedef enum {func, var, stack_op} decl_type;

typedef struct reg_list
{
  size_t id;
  struct reg_list* next;
  value_type type;
} reg_list;

typedef struct decl_list
{
  struct decl_list* next;
  char* name;
  value_type args[3];
  value_type ret;
  unsigned short argc;
  bool predecl;
  bool needs_stack;
  bool rets;
  enum {immutable, mutable} mut;
  reg_list* (*stack_shuffle)(reg_list*);
  decl_type type;
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

extern FILE* yyin;
extern FILE* outfile;
extern size_t current_register;
extern ast* full_ast;

decl_list* builtins(void);
decl_list* lookup_word(char*, decl_list*);
reg_list* emit_register(value_type, reg_list*);
void compile(ast*, reg_list*, decl_list*); // TODO output file arg.
void print_cognate_string(char*);
decl_list* predefine(ast*, decl_list*);
void check_for_mutation(ast*, decl_list*);
reg_list* add_register(value_type, reg_list*);
reg_list* assert_registers(size_t, size_t, reg_list*);
reg_list* swap_register(reg_list*);
reg_list* triplet_register(reg_list*);
reg_list* twin_register(reg_list*);
reg_list* drop_register(reg_list*);

int yylex(void);
void yyerror(char*);
char* lc(char*);
