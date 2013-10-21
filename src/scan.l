%{
#include <stdlib.h>
#include <string.h>

#include "picrin.h"
#include "y.tab.h"

struct parser_control {
  pic_state *pic;
  pic_value value;
  bool incomp;
};

static pic_value escape_string(pic_state *, const char *);

#define YY_DECL int yylex (struct parser_control *p)
%}

%option noinput
%option nounput

  /* boolean */
boolean		#t|#f|#true|#false

  /* symbol */
identifier	[a-z0-9A-Z+-/*?<=>_.]+

  /* number */
digit		[0-9]
real		{sign}{ureal}|{infnan}
ureal		{digit}+|\.{digit}+|{digit}+\.{digit}*
sign		[+-]?
infnan		"+inf.0"|"-inf.0"|"+nan.0"|"-nan.0"

  /* string */
string		\"{string_elem}*\"
string_elem	[^\"\\]|"\\\""|"\\\\"

%%

[ \t\n\r]	/* skip whitespace */
"."		return tDOT;
"("		return tLPAREN;
")"		return tRPAREN;
"'"		return tQUOTE;
{boolean}	{ yylval.datum = pic_bool_value(strcmp(yytext, "#t") == 0 || strcmp(yytext, "#true") == 0); return tBOOLEAN; }
{real} 		{ yylval.datum = pic_float_value(atof(yytext)); return tNUMBER; }
{identifier}	{ yylval.datum = pic_intern_cstr(p->pic, yytext); return tSYMBOL; }
{string}	{
		  yylval.datum = escape_string(p->pic, yytext);
		  return tSTRING;
		}

%%

int
yywrap()
{
  return 1;
}

static pic_value
escape_string(pic_state *pic, const char *str)
{
  size_t len;
  char *new_str;
  int i;
  pic_value v;

  len = strlen(str);
  new_str = (char *)pic_alloc(pic, len + 1);
  for (i = 1; i < len - 1; ++i) {
    new_str[i-1] = str[i];
  }
  new_str[i] = '\0';

  v = pic_str_new_cstr(pic, new_str);
  pic_free(pic, new_str);
  return v;
}

void
init_scanner(const char *str)
{
  yy_scan_string(str);
}

void
final_scanner()
{
  yylex_destroy();
}
