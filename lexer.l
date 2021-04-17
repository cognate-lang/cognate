%option noyywrap noinput nounput nodefault
%{
#define YY_USER_ACTION yylloc.first_line = yylloc.last_line = yylineno; \
                       yylloc.first_column = yycolumn; yylloc.last_column = yycolumn + yyleng - 1; \
                       yycolumn += yyleng;

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
  fprintf(stderr, "Error @ %d:%d >> %s\n", yylloc.first_line, yylloc.first_column, str);
  exit(EXIT_FAILURE);
}

int yycolumn = 1;
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

;|, return ';';
\(  return '(';
\)  return ')';

\n yylineno++; yycolumn=1;

[a-z][A-Za-z_0-9]*  /* Ignore informal syntax */
[[:space:]]         /* Ignore whitespace      */
\~\~.*              /* Ignore line comments   */

\~                 BEGIN(BLOCK_COMMENT);
<BLOCK_COMMENT>\~  BEGIN(INITIAL);
<BLOCK_COMMENT>\n  yylineno++; yycolumn=1;
<BLOCK_COMMENT>.

. yyerror("Invalid token");
%%
