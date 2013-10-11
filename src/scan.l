%{
#include "picrin.h"
#include "y.tab.h"

struct parser_control {
  pic_state *pic;
  pic_value value;
};

#define YY_DECL int yylex (struct parser_control *p)
%}

%%

"("	return tLPAREN;
")"	return tRPAREN;
[a-z]+	{ yylval.datum = pic_intern_cstr(p->pic, yytext); return tSYMBOL; }

%%

int
yywrap()
{
  return 1;
}
