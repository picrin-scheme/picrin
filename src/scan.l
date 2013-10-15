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

%%

[ \t\n\r]	/* skip whitespace */
"("	return tLPAREN;
")"	return tRPAREN;
[1-9][0-9]*	{ yylval.datum = pic_float_value(atoi(yytext)); return tNUMBER; }
[a-z0-9A-Z]+	{ yylval.datum = pic_intern_cstr(p->pic, yytext); return tSYMBOL; }

%%

int
yywrap()
{
  return 1;
}
