%{
#include <stdlib.h>

#include "picrin.h"
#include "y.tab.h"

struct parser_control {
  pic_state *pic;
  pic_value value;
};

#define YY_DECL int yylex (struct parser_control *p)
%}

%option nounput

  /* boolean */
boolean		#t|#f|#true|#false

  /* symbol */
identifier	[a-z0-9A-Z+-/*?<=>_]+

  /* number */
digit		[0-9]
real		{sign}{ureal}|{infnan}
ureal		{digit}+|\.{digit}+|{digit}+\.{digit}*
sign		[+-]?
infnan		"+inf.0"|"-inf.0"|"+nan.0"|"-nan.0"

%%

[ \t\n\r]	/* skip whitespace */
"("		return tLPAREN;
")"		return tRPAREN;
{boolean}	{ yylval.datum = pic_bool_value(strcmp(yytext, "#t") == 0 || strcmp(yytext, "#true") == 0); return tBOOLEAN; }
{real} 		{ yylval.datum = pic_float_value(atof(yytext)); return tNUMBER; }
{identifier}	{ yylval.datum = pic_intern_cstr(p->pic, yytext); return tSYMBOL; }

%%

int
yywrap()
{
  return 1;
}
