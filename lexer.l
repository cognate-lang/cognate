%option noyywrap noinput nounput nodefault
%{
#define YY_USER_ACTION yylloc.first_column += yyleng;

#include "cognac.h"
#include <ctype.h>
#include <stdlib.h>

char* lc(char* s)
{
  for (int i = 0; s[i] != '\0'; ++i) s[i] = tolower(s[i]);
  return s;
}

void yyerror(char* str)
{
  char header[20];
  sprintf(header, "Line %d: ", yylloc.first_line, yylloc.first_column);
  fprintf(stderr, "\033[0;2m%s", header);
  char line[80];
  rewind(yyin);
  while (yylloc.first_line--) fgets(line, sizeof line, yyin);
  fprintf(stderr, "\033[0;1m%s", line);
  yylloc.first_column += strlen(header) - 1;
  while (--yylloc.first_column) fputc(' ', stderr);
  fprintf(stderr, "\033[31;1m^ %s\n", str);
  exit(EXIT_FAILURE);
}

int brace_depth = 0;
int last_openbrace_line;
int last_openbrace_col;
%}

%x BLOCK_COMMENT

%%
">"  yylval.text=strdup("GT");  return IDENTIFIER;
">=" yylval.text=strdup("GTE"); return IDENTIFIER;
"<"  yylval.text=strdup("LT");  return IDENTIFIER;
"<=" yylval.text=strdup("LTE"); return IDENTIFIER;
"==" yylval.text=strdup("EQ");  return IDENTIFIER;
"/=" yylval.text=strdup("NEQ"); return IDENTIFIER;
"+"  yylval.text=strdup("ADD"); return IDENTIFIER;
"-"  yylval.text=strdup("SUB"); return IDENTIFIER;
"*"  yylval.text=strdup("MUL"); return IDENTIFIER;
"/"  yylval.text=strdup("DIV"); return IDENTIFIER;

D[eE][fF][iI][nN][eE] return DEFINE;
L[eE][tT]             return LET;
S[eE][tT]             return SET;

[A-Z][A-Za-z_0-9]*   yylval.text=lc(strdup(yytext));   return IDENTIFIER;
\\[A-Za-z_0-9]*      yylval.text=lc(strdup(yytext+1)); return SYMBOL;
\-?[0-9]+(\.[0-9]+)? yylval.text=strdup(yytext);       return NUMBER;
\'(\\.|[^'\\])*\'    yylval.text=strdup(yytext);       return STRING;
\'                   yyerror("unterminated string"); // TODO error message is printing with wrong column.

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
