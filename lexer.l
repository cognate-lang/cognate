%option yylineno
%{
#include "cognac.h"
%}

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

[A-Z][A-Za-z_0-9]*   yylval.text=lc(yytext); return IDENTIFIER;
\\[A-Za-z_0-9]*      yylval.text=lc(yytext+1); return SYMBOL;
\-?[0-9]+(\.[0-9]+)? yylval.text=strdup(yytext); return NUMBER;
\'(\\.|[^'\\])*\'    yylval.text=strdup(yytext); return STRING;

;|, return ';';
\(  return '(';
\)  return ')';

[a-z][A-Za-z_0-9]*    /* Ignore informal syntax */
[[:space:]]           /* Ignore whitespace      */
\~\~.*                /* Ignore line comments   */

. printf("Invalid token '%s'\n", yytext); exit(EXIT_FAILURE);
%%


