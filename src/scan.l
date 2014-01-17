/**
 * See Copyright Notice in picrin.h
 */

%{
#include <stdlib.h>
#include <string.h>

#include "picrin.h"
#include "picrin/parse.h"
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

  /* char */
%x CHAR

  /* string */
%x STRING

  /* bytevector */
%x BYTEVECTOR

%%

[ \t\n\r]	/* skip whitespace */
{comment}	/* skip comment */

"#;"		return tDATUM_COMMENT;
"."		return tDOT;
"("		return tLPAREN;
")"		return tRPAREN;
"["		return tLBRACKET;
"]"		return tRBRACKET;
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

#\\		{
  BEGIN(CHAR);
}
<CHAR>alarm	{ yylvalp->c = '\a'; BEGIN(INITIAL); return tCHAR; }
<CHAR>backspace	{ yylvalp->c = '\b'; BEGIN(INITIAL); return tCHAR; }
<CHAR>delete	{ yylvalp->c = 0x7f; BEGIN(INITIAL); return tCHAR; }
<CHAR>escape	{ yylvalp->c = 0x1b; BEGIN(INITIAL); return tCHAR; }
<CHAR>newline	{ yylvalp->c = '\n'; BEGIN(INITIAL); return tCHAR; }
<CHAR>null	{ yylvalp->c = '\0'; BEGIN(INITIAL); return tCHAR; }
<CHAR>return	{ yylvalp->c = '\r'; BEGIN(INITIAL); return tCHAR; }
<CHAR>space	{ yylvalp->c = ' '; BEGIN(INITIAL); return tCHAR; }
<CHAR>tab	{ yylvalp->c = '\t'; BEGIN(INITIAL); return tCHAR; }
<CHAR>.		{ yylvalp->c = yytext[0]; BEGIN(INITIAL); return tCHAR; }

"#u8("		{
  BEGIN(BYTEVECTOR);
  yylvalp->blob.len = 0;
  yylvalp->blob.capa = 10;
  yylvalp->blob.dat = calloc(10, 1);
}
<BYTEVECTOR>[ \r\n\t]
<BYTEVECTOR>{uinteger}	{
  int i = atoi(yytext);
  if (0 > i || i > 255)
    REJECT;
  yylvalp->blob.dat[yylvalp->blob.len++] = (char)i;
  if (yylvalp->blob.len > yylvalp->blob.capa) {
    yylvalp->blob.capa *= 2;
    yylvalp->blob.dat = realloc(yylvalp->blob.dat, yylvalp->blob.capa);
  }
}
<BYTEVECTOR>")"	{
  BEGIN(INITIAL);
  return tBYTEVECTOR;
}

%%

int
yywrap(yyscan_t yyscanner)
{
  return 1;
}
