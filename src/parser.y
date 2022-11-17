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
	USE
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
	  IDENTIFIER         { $$ = ast_single(identifier, (void*)lowercase($1), parse_pos()); }
	| MODULE_IDENTIFIER  { $$ = ast_single(module_identifier, (void*)lowercase($1), parse_pos()); }
	| '(' EXPRESSION ')' { $$ = ast_single(braces, (void*)$2, parse_pos()); }
	| NUMBER             { $$ = ast_single(literal, mk_lit(number, $1), parse_pos()); }
	| STRING             { $$ = ast_single(literal, mk_lit(string, $1), parse_pos()); }
	| SYMBOL             { $$ = ast_single(literal, mk_lit(symbol, $1), parse_pos()); }
	| DEF IDENTIFIER     { $$ = ast_single(def, (void*)lowercase($2), parse_pos()); }
	| LET IDENTIFIER     { $$ = ast_single(let, (void*)lowercase($2), parse_pos()); }
	;

%%
