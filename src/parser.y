%{
#include "cognac.h"
#include <stdlib.h>

ast* ast_join(ast* a, ast* b)
{
	if (!a) return b;
	ast* ptr = a;
	while (ptr->next) ptr=ptr->next;
	ptr->next = b;
	return a;
}

ast* alloc_ast(token_type type, value_type val_type, void* data)
{
	ast* a = malloc (sizeof *a);
	*a = (ast){.type=type, .val_type=val_type, .data=data, .line=yylloc.first_line, .col=yylloc.first_column, .next=NULL};
	return a;
}

record_t* alloc_record(char* name, record_t* next)
{
  record_t* r = malloc(sizeof *r);
  *r = (record_t){.name=name, .next=next};
  return r;
}
%}

%locations

%union {
	char* text;
	struct ast* tree;
	struct record_t* record;
}

%token
	<text> NUMBER
	<text> IDENTIFIER
	<text> STRING
	<text> SYMBOL
	TYPE
	DEF
	LET
	';'
	'('
	')'
	;

%type <tree> STATEMENT;
%type <tree> EXPRESSION;
%type <tree> TOKEN;
%type <record> TYPEBODY;

%start ENTRY;
%%

ENTRY:
	  EXPRESSION { full_ast = $1; }
	;

TYPEBODY:
	  IDENTIFIER          { $$ = alloc_record($1, NULL); }
	| IDENTIFIER TYPEBODY { $$ = alloc_record($1, $2); }
	;

EXPRESSION:
	  STATEMENT ';' EXPRESSION     { $$ = ast_join($1, $3);         }
	| TYPE TYPEBODY ';' EXPRESSION { $$ = ast_join(alloc_ast(type, any, $2), $4); }
	| STATEMENT                    { $$ = $1;                       }
	;

STATEMENT:
	  TOKEN STATEMENT { $$ = ast_join($2, $1); }
	| /* Empty */     { $$ = NULL;             }
	;

TOKEN:
	  IDENTIFIER         { $$ = alloc_ast(identifier, any, $1); }
	| '(' EXPRESSION ')' { $$ = alloc_ast(value, block,    $2); }
	| NUMBER             { $$ = alloc_ast(value, number,   $1); }
	| STRING             { $$ = alloc_ast(value, string,   $1); }
	| SYMBOL             { $$ = alloc_ast(value, symbol,   $1); }
	| DEF IDENTIFIER     { $$ = alloc_ast(def, any,        $2); }
	| LET IDENTIFIER     { $$ = alloc_ast(let, any,        $2); }
	;

%%
