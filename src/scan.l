%{
#include <stdlib.h>
#include <string.h>

#include "picrin.h"
#include "y.tab.h"

struct parser_control {
  pic_state *pic;
  void *yyscanner;
  pic_value value;
  bool incomp;
  int yynerrs;
};

static pic_value new_escaped_string(pic_state *, const char *);

#define YY_DECL int yylex_ (YYSTYPE *yylvalp, yyscan_t yyscanner, struct parser_control *p)
%}

%option reentrant
%option noinput
%option nounput

  /* boolean */
boolean		#t|#f|#true|#false

  /* symbol */
identifier	[a-z0-9A-Z+-/*!$%&:@^~?<=>_.]+

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
{boolean}	{ yylvalp->datum = pic_bool_value(strcmp(yytext, "#t") == 0 || strcmp(yytext, "#true") == 0); return tBOOLEAN; }
{real} 		{ yylvalp->datum = pic_float_value(atof(yytext)); return tNUMBER; }
{identifier}	{ yylvalp->datum = pic_intern_cstr(p->pic, yytext); return tSYMBOL; }
{string}	{
		  yylvalp->datum = new_escaped_string(p->pic, yytext);
		  return tSTRING;
		}

%%

int
yywrap(yyscan_t yyscanner)
{
  return 1;
}

static pic_value
new_escaped_string(pic_state *pic, const char *str)
{
  size_t len;
  char *new_str;
  int i,j;
  pic_value v;

  len = strlen(str);
  new_str = (char *)pic_alloc(pic, len + 1);
  for (j = 0, i = 1; i < len - 1; ++i, ++j) {
    if (str[i] == '\\') {
      ++i;
      new_str[j] = str[i];
      continue;
    }
    new_str[j] = str[i];
  }
  new_str[j] = '\0';

  v = pic_str_new_cstr(pic, new_str);
  pic_free(pic, new_str);
  return v;
}
