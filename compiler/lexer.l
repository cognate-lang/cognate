%option noyywrap noinput nounput nodefault
%{
#define YY_USER_ACTION yylloc.first_column += yyleng;

#include "cognac.h"
#include <ctype.h>
#include <stdlib.h>
#include <gc/gc.h>

char* lc(char* s)
{
  for (int i = 0; s[i] != '\0'; ++i) s[i] = tolower(s[i]);
  return s;
}

void yyerror(char* str)
{
  char header[20];
  sprintf(header, "Line %d: ", yylloc.first_line);
  fprintf(stderr, "\033[0;2m%s", header);
  char* line = NULL;
  rewind(yyin);
  while (--yylloc.first_line) fgets(NULL, 0, yyin);
  size_t n = SIZE_MAX;
  getline(&line, &n, yyin);
  fprintf(stderr, "\033[0;1m%s", line);
  yylloc.first_column += strlen(header) - 1;
  while (--yylloc.first_column) fputc(' ', stderr);
  fprintf(stderr, "\033[31;1m↳ %s\033[0m\n", str);
  exit(EXIT_FAILURE);
}

int brace_depth = 0;
int last_openbrace_line;
int last_openbrace_col;
%}

%x BLOCK_COMMENT

ALLOWCHAR [A-Za-z0-9\-\?\!\>\<\=\+\/]

%%
D[eE][fF][iI][nN][eE] return DEFINE;
L[eE][tT]             return LET;
S[eE][tT]             return SET;

[A-Z]{ALLOWCHAR}*    yylval.text=lc(GC_STRDUP(yytext));   return IDENTIFIER;
\\{ALLOWCHAR}+       yylval.text=lc(GC_STRDUP(yytext+1)); return SYMBOL;
\-?[0-9]+(\.[0-9]+)? yylval.text=GC_STRDUP(yytext);       return NUMBER;
\'(\\.|[^'\\])*\'    yylval.text=GC_STRDUP(yytext);       return STRING;
\'                   yyerror("unterminated string"); // TODO error message is printing with wrong column.

">"  yylval.text=GC_STRDUP("GT");  return IDENTIFIER;
">=" yylval.text=GC_STRDUP("GTE"); return IDENTIFIER;
"<"  yylval.text=GC_STRDUP("LT");  return IDENTIFIER;
"<=" yylval.text=GC_STRDUP("LTE"); return IDENTIFIER;
"==" yylval.text=GC_STRDUP("EQ");  return IDENTIFIER;
"/=" yylval.text=GC_STRDUP("NEQ"); return IDENTIFIER;
"+"  yylval.text=GC_STRDUP("ADD"); return IDENTIFIER;
"-"  yylval.text=GC_STRDUP("SUB"); return IDENTIFIER;
"*"  yylval.text=GC_STRDUP("MUL"); return IDENTIFIER;
"/"  yylval.text=GC_STRDUP("DIV"); return IDENTIFIER;

;|, return ';';
\(  brace_depth++; last_openbrace_line = yylloc.first_line; last_openbrace_col = yylloc.first_column; return '(';

\)  if (brace_depth--) return ')'; yyerror("unbalanced brace");

<<EOF>> if (brace_depth) { yylloc.first_line = last_openbrace_line;
                           yylloc.first_column = last_openbrace_col;
                           yyerror("unbalanced brace"); } else yyterminate();

\n yylloc.first_line++; yylloc.first_column = 1;

[a-z][A-Za-z_0-9]*  /* Ignore informal syntax */
[[:space:]]         /* Ignore whitespace      */
\~\~.*              /* Ignore line comments   */

\~                 BEGIN(BLOCK_COMMENT);
<BLOCK_COMMENT>\~  BEGIN(INITIAL);
<BLOCK_COMMENT>\n  yylloc.first_line++; yylloc.first_column = 1;
<BLOCK_COMMENT>.

. yyerror("invalid token");
%%
