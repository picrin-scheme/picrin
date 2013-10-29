%{
#include <stdlib.h>
#include <string.h>

#include "picrin.h"
#include "y.tab.h"

#define YY_DECL int yylex_(YYSTYPE *yylvalp, yyscan_t yyscanner)
%}

%option reentrant
%option noinput
%option nounput

  /* comment */
comment		;.*$

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
{comment}	/* skip comment */

"."		return tDOT;
"("		return tLPAREN;
")"		return tRPAREN;
"#("		return tVPAREN;
"'"		return tQUOTE;
"`"		return tQUASIQUOTE;
","		return tUNQUOTE;
",@"		return tUNQUOTE_SPLICING;

{boolean}	{
  yylvalp->i = (yytext[1] == 't');
  return tBOOLEAN;
}

{integer}	{
  yylvalp->i = atoi(yytext);
  return tINT;
}

{real} 		{
  yylvalp->f = atof(yytext);
  return tFLOAT;
}

{identifier}	{
  yylvalp->cstr = strdup(yytext);
  return tSYMBOL;
}

"\""		BEGIN(STRING);
<STRING>{
	[^\\"]*		yymore();
	"\""		{
			  yytext[yyleng-1] = '\0';
			  yylvalp->cstr = strdup(yytext);
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
