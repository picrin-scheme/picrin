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

void yyerror(struct parser_control *p, const char *msg);

#define YY_DECL int yylex_ (YYSTYPE *yylvalp, yyscan_t yyscanner, struct parser_control *p)
%}

%option reentrant
%option noinput
%option nounput

  /* boolean */
boolean		#t|#f|#true|#false

  /* symbol */
identifier	[a-z0-9A-Z+/*!$%&:@^~?<=>_.-]+

  /* number */
digit		[0-9]
real		{sign}{ureal}|{infnan}
ureal		{uinteger}|\.{digit}+|{digit}+\.{digit}*
integer		{sign}{uinteger}
uinteger	{digit}+
sign		[+-]?
infnan		"+inf.0"|"-inf.0"|"+nan.0"|"-nan.0"

  /* string */
%x STRING

%%

[ \t\n\r]	/* skip whitespace */
"."		return tDOT;
"("		return tLPAREN;
")"		return tRPAREN;
"'"		return tQUOTE;
"`"		return tQUASIQUOTE;
","		return tUNQUOTE;
",@"		return tUNQUOTE_SPLICING;
{boolean}	{ yylvalp->datum = pic_bool_value(strcmp(yytext, "#t") == 0 || strcmp(yytext, "#true") == 0); return tBOOLEAN; }
{integer}	{ yylvalp->datum = pic_int_value(atoi(yytext)); return tNUMBER; }
{real} 		{ yylvalp->datum = pic_float_value(atof(yytext)); return tNUMBER; }
{identifier}	{ yylvalp->datum = pic_intern_cstr(p->pic, yytext); return tSYMBOL; }
"\""		BEGIN(STRING);
<STRING>{
	[^\\"]*		yymore();
	<<EOF>>		{ yyerror(p, "eof in string"); BEGIN(INITIAL); }
	"\""		{
			  yytext[yyleng-1] = '\0';
			  yylvalp->datum = pic_str_new_cstr(p->pic, yytext);
			  BEGIN(INITIAL);
			  return tSTRING;
			}
}

%%

int
yywrap(yyscan_t yyscanner)
{
  return 1;
}
