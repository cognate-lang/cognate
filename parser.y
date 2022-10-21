%{
#include "cognac.h"
%}

%locations

%union {
	char* text;
	ast_list_t* tree;
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

%start ENTRY;
%%

ENTRY:
	  EXPRESSION { full_ast = $1; }
	;

EXPRESSION:
	  STATEMENT ';' EXPRESSION     { $$ = join_ast($1, $3);         }
	| STATEMENT                    { $$ = $1;                       }
	;

STATEMENT:
	  TOKEN STATEMENT { $$ = join_ast($2, $1); }
	| /* Empty */     { $$ = NULL;             }
	;

TOKEN:
	  IDENTIFIER         { $$ = ast_single(identifier, (void*)lowercase($1), pmod); }
	| '(' EXPRESSION ')' { $$ = ast_single(braces, (void*)$2, pmod); }
	| NUMBER             { $$ = ast_single(literal, mk_lit(number, $1), pmod); }
	| STRING             { $$ = ast_single(literal, mk_lit(string, $1), pmod); }
	| SYMBOL             { $$ = ast_single(literal, mk_lit(symbol, $1), pmod); }
	| DEF IDENTIFIER     { $$ = ast_single(def, (void*)lowercase($2), pmod); }
	| LET IDENTIFIER     { $$ = ast_single(let, (void*)lowercase($2), pmod); }
	;

%%
