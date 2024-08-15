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
	<text> MODULE_IDENTIFIER
	<text> STRING
	<text> SYMBOL
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
	  IDENTIFIER         { $$ = ast_single(identifier, (void*)lowercase($1), parse_pos($1)); }
	| MODULE_IDENTIFIER  { $$ = ast_single(module_identifier, (void*)lowercase($1), parse_pos($1)); }
	| '(' EXPRESSION ')' { $$ = ast_single(braces, (void*)$2, parse_pos($1)); }
	| NUMBER             { $$ = ast_single(literal, mk_lit(number, $1), parse_pos($1)); }
	| STRING             { $$ = ast_single(literal, mk_lit(string, $1), parse_pos($1)); }
	| SYMBOL             { $$ = ast_single(literal, mk_lit(symbol, $1), parse_pos($1)); }
	| DEF IDENTIFIER     { $$ = ast_single(def, (void*)lowercase($2), parse_pos($2)); }
	| LET IDENTIFIER     { $$ = ast_single(let, (void*)lowercase($2), parse_pos($2)); }
	;

%%
