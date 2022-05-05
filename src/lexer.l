%option noyywrap noinput nounput nodefault
%{
#define YY_USER_ACTION yylloc.first_column += yyleng;

#include "cognac.h"
#include <ctype.h>
#include <stdlib.h>
#include <limits.h>

char* lc(char* s)
{
	for (int i = 0; s[i] != '\0'; ++i) s[i] = tolower(s[i]);
	return s;
}

void yyerror(char* str)
{
	char header[20];
	int line_n = yylloc.first_line;
	sprintf(header, "Line %d: ", line_n);
	int col_n = yylloc.first_column + strlen(header) - 1;
	fprintf(stderr, "\033[0;2m%s", header);
	char* line = NULL;
	rewind(yyin);
	while (--line_n)
	{
		char c;
		do { c = fgetc(yyin); } while (c != '\n');
	}
	size_t n = SIZE_MAX;
	getline(&line, &n, yyin);
	int whitespace = 0;
	while (isspace(line[whitespace])) whitespace++;
	line += whitespace;
	col_n -= whitespace;
	fprintf(stderr, "\033[0;1m%s", line);
	while (--col_n) fputc(' ', stderr);
	fprintf(stderr, "\033[31;1m↳ %s\033[0m\n", str);
	exit(EXIT_FAILURE);
}

int brace_depth = 0;
int last_openbrace_line;
int last_openbrace_col;
%}

%x BLOCK_COMMENT

ALLOWCHAR [A-Za-z0-9\-\?\!\'\+\/\*]
FORMAL    [A-Z\-\?\!\+\/\*]

%%
D[eE][fF] return DEF;
L[eE][tT] return LET;
S[eE][tT] return SET;
T[yY][pP][eE] return TYPE;

\-?[0-9]+(\.[0-9]+)? yylval.text=strdup(yytext);       return NUMBER;
{FORMAL}{ALLOWCHAR}* yylval.text=lc(strdup(yytext));   return IDENTIFIER;
\\{ALLOWCHAR}+       yylval.text=lc(strdup(yytext+1)); return SYMBOL;
\"(\\.|[^\n"\\])*\"  yylval.text=strdup(yytext);       return STRING;
\"                   yyerror("unterminated string"); // TODO error message is printing with wrong column.

">"  yylval.text=strdup(">");  return IDENTIFIER;
">=" yylval.text=strdup(">="); return IDENTIFIER;
"<"  yylval.text=strdup("<");  return IDENTIFIER;
"<=" yylval.text=strdup("<="); return IDENTIFIER;
"==" yylval.text=strdup("=="); return IDENTIFIER;
"/=" yylval.text=strdup("/="); return IDENTIFIER;
"+"  yylval.text=strdup("+");  return IDENTIFIER;
"-"  yylval.text=strdup("-");  return IDENTIFIER;
"*"  yylval.text=strdup("*");  return IDENTIFIER;
"/"  yylval.text=strdup("/");  return IDENTIFIER;

; return ';';
\( brace_depth++; last_openbrace_line = yylloc.first_line; last_openbrace_col = yylloc.first_column; return '(';

\) if (brace_depth--) return ')'; yyerror("unbalanced brace");

<<EOF>> if (brace_depth) { yylloc.first_line = last_openbrace_line;
	yylloc.first_column = last_openbrace_col;
	yyerror("unbalanced brace"); } else yyterminate();

\n yylloc.first_line++; yylloc.first_column = 1;

[a-z][a-z0-9\'\-\?\!\.]* /* Ignore informal syntax */
[[:space:]]              /* Ignore whitespace      */
\~\~.*                   /* Ignore line comments   */

\~                BEGIN(BLOCK_COMMENT);
<BLOCK_COMMENT>\~ BEGIN(INITIAL);
<BLOCK_COMMENT>\n yylloc.first_line++; yylloc.first_column = 1;
<BLOCK_COMMENT>.

. yyerror("invalid token");
%%
