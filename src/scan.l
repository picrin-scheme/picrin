/**
 * See Copyright Notice in picrin.h
 */

%{
#include "picrin.h"
#include "picrin/parse.h"

#define yylval (yyextra->yylval)

#define YY_NO_UNISTD_H

/* NOTE:
 *  An internal function `yy_fatal_error` takes yyscanner for its second
 * argument but doesn't use it. This invokes a `unused variable` compiler
 * warning and it became super unusable if `-Werror` is turned on the system.
 * Since there's no flag to switch `yy_fatal_error` off and replace it with
 * a user-defined function, we modify this macro constant to use yyscanner
 * at least once avoiding get flex affected in any condition.
 */
#define YY_EXIT_FAILURE ( (void)yyscanner, 2 )
char *yycheckprefix(char *, int *, bool *);
%}

%option reentrant

%option noyyalloc
%option noyyrealloc
%option noyyfree
%option noinput
%option nounput
%option noyywrap

%option extra-type="struct parser_control *"
%option never-interactive

  /* comment */
comment		;.*$

  /* boolean */
boolean		#t|#f|#true|#false

  /* symbol */
identifier	[a-z0-9A-Z+/*!$%&:@^~?<=>_.-]+

  /* number */
bit		[01]
octal		[0-7]
digit		[0-9]
hexadecimal	[0-9a-fA-F]
real		{exponential}|{rational}|{float}|{integer}|{infnan}
exponential	{float}{exptsuffix}
rational	{radix2rational}|{radix8rational}|{radix10rational}|{radix16rational}
radix2rational	{bitprefix}{sign}{bit}+\/{bit}+
radix8rational	{octalprefix}{sign}{octal}+\/{octal}+
radix10rational	{digitprefix}{sign}{uinteger}\/{uinteger}
radix16rational	{hexprefix}{sign}{hexadecimal}+\/{hexadecimal}+
float		{digitprefix}\.{digit}+|{digitprefix}{digit}+\.{digit}*
integer		{radix2int}|{radix8int}|{radix10int}|{radix16int}
radix2int	{bitprefix}{sign}{bit}+
radix8int	{octalprefix}{sign}{octal}+
radix10int	{digitprefix}{sign}{uinteger}
radix16int	{hexprefix}{sign}{hexadecimal}+
uinteger	{digit}+
sign		[+-]?
exptsuffix	{exptmark}{sign}{digit}+
exptmark	"e"
exactness	"#e"|"#i"
bitprefix	{exactness}"#b"|"#b"{exactness}|"#b"
octalprefix	{exactness}"#o"|"#o"{exactness}|"#o"
digitprefix	{exactness}"#d"|"#d"{exactness}|"#d"?|{exactness}
hexprefix	{exactness}"#x"|"#x"{exactness}|"#x"
infnan		"+inf.0"|"-inf.0"|"+nan.0"|"-nan.0"

  /* char */
%x CHAR

  /* string */
%x STRING

  /* bytevector */
%x BYTEVECTOR

  /* block comment */
%x BLOCK_COMMENT

  /* datum label */
label		#{uinteger}
%x DATUM_LABEL

%%

[ \t\n\r]	/* skip whitespace */
{comment}	/* skip comment */

"#|"		{
  BEGIN(BLOCK_COMMENT);
  yylval.i = 0;
}
<BLOCK_COMMENT>"#|" 	{
  yylval.i++;
}
<BLOCK_COMMENT>"|#"	{
  if (yylval.i == 0)
    BEGIN(INITIAL);
  else
    yylval.i--;
}
<BLOCK_COMMENT>.|\n	{
  /* skip block comment */
}

{label}		{
  BEGIN(DATUM_LABEL);
  yylval.i = atoi(yytext + 1);
}
<DATUM_LABEL>=	{
  BEGIN(INITIAL);
  return tLABEL_SET;
}
<DATUM_LABEL>#	{
  BEGIN(INITIAL);
  return tLABEL_REF;
}

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
  yylval.i = (yytext[1] == 't');
  return tBOOLEAN;
}

{integer}	{
  bool exactp; int radix;
  char *str = yycheckprefix(yytext, &radix, &exactp);
  if(exactp){
    yylval.buf.dat = str;
    yylval.buf.len = radix;
    return tBIGINT;    
  }
  else{
    mpz_t num;
    mpz_init(num);
    mpz_set_str(num, str, radix);
    yylval.i = (int) mpz_get_si(num);
    return tINT;
  }
}

{rational}	{
  bool exactp; int radix;
  char *str = yycheckprefix(yytext, &radix, &exactp);
  if(exactp){
    yylval.buf.dat = str;
    yylval.buf.len = radix;
    return tRATIONAL;
  }
  else{
    mpq_t num;
    mpq_init(num);
    mpq_set_str(num, str, radix);
    yylval.f = mpq_get_d(num);
    return tFLOAT;
  }
}
{real}		{
  bool exactp; int radix;
  char *str = yycheckprefix(yytext, &radix, &exactp);
  if(exactp){
    yylval.buf.dat = str;
    yylval.buf.len = radix;
    return tBIGFLOAT;
  }
  else{
    mpf_t num;
    mpf_init(num);
    mpf_set_str(num, str, radix);
    yylval.f = mpf_get_d(num);
    return tFLOAT;
  }
}

{identifier}	{
  yylval.buf.dat = yytext;
  yylval.buf.len = yyleng;
  return tSYMBOL;
}

"\""		{
  BEGIN(STRING);
  yylval.buf.len = 0;
  yylval.buf.dat = yyalloc(yylval.buf.len + 1, yyscanner);
  strcpy(yylval.buf.dat, "");
}
<STRING>[^\\"]+	{
  yylval.buf.len += yyleng;
  yylval.buf.dat = yyrealloc(yylval.buf.dat, yylval.buf.len + 1, yyscanner);
  strcpy(yylval.buf.dat + yylval.buf.len - yyleng, yytext);
}
<STRING>\\.	{
  yylval.buf.len += 1;
  yylval.buf.dat = yyrealloc(yylval.buf.dat, yylval.buf.len + 1, yyscanner);
  yylval.buf.dat[yylval.buf.len] = '\0';

  switch (yytext[yyleng - 1]) {
  case 'a': yylval.buf.dat[yylval.buf.len - 1] = '\a'; break;
  case 'b': yylval.buf.dat[yylval.buf.len - 1] = '\b'; break;
  case 't': yylval.buf.dat[yylval.buf.len - 1] = '\t'; break;
  case 'n': yylval.buf.dat[yylval.buf.len - 1] = '\n'; break;
  case 'r': yylval.buf.dat[yylval.buf.len - 1] = '\r'; break;
  default: yylval.buf.dat[yylval.buf.len - 1] = yytext[yyleng - 1]; break;
  }
}
<STRING>\\[:blank:]*\n[:blank:]* {
  /* skip intraline whitespaces */
}
<STRING>\"	{
  BEGIN(INITIAL);
  return tSTRING;
}

#\\		{
  BEGIN(CHAR);
}
<CHAR>alarm	{ yylval.c = '\a'; BEGIN(INITIAL); return tCHAR; }
<CHAR>backspace	{ yylval.c = '\b'; BEGIN(INITIAL); return tCHAR; }
<CHAR>delete	{ yylval.c = 0x7f; BEGIN(INITIAL); return tCHAR; }
<CHAR>escape	{ yylval.c = 0x1b; BEGIN(INITIAL); return tCHAR; }
<CHAR>newline	{ yylval.c = '\n'; BEGIN(INITIAL); return tCHAR; }
<CHAR>null	{ yylval.c = '\0'; BEGIN(INITIAL); return tCHAR; }
<CHAR>return	{ yylval.c = '\r'; BEGIN(INITIAL); return tCHAR; }
<CHAR>space	{ yylval.c = ' ';  BEGIN(INITIAL); return tCHAR; }
<CHAR>tab	{ yylval.c = '\t'; BEGIN(INITIAL); return tCHAR; }
<CHAR>.		{ yylval.c = yytext[0]; BEGIN(INITIAL); return tCHAR; }

"#u8("		{
  BEGIN(BYTEVECTOR);
  yylval.buf.len = 0;
  yylval.buf.dat = NULL;
}
<BYTEVECTOR>[ \r\n\t]	{
  /* skip whitespace */
}
<BYTEVECTOR>{uinteger}	{
  int i = atoi(yytext);
  if (0 > i || i > 255) {
    yyfree(yylval.buf.dat, yyscanner);
    REJECT;
  }
  yylval.buf.len += 1;
  yylval.buf.dat = yyrealloc(yylval.buf.dat, yylval.buf.len, yyscanner);
  yylval.buf.dat[yylval.buf.len - 1] = (char)i;
}
<BYTEVECTOR>")"	{
  BEGIN(INITIAL);
  return tBYTEVECTOR;
}

%%

void *
yyalloc(size_t bytes, yyscan_t yyscanner)
{
  return pic_alloc(yyget_extra(yyscanner)->pic, bytes);
}

void *
yyrealloc(void *ptr, size_t bytes, yyscan_t yyscanner)
{
  return pic_realloc(yyget_extra(yyscanner)->pic, ptr, bytes);
}

void
yyfree(void * ptr, yyscan_t yyscanner)
{
  return pic_free(yyget_extra(yyscanner)->pic, ptr);
}


char *
yycheckprefix(char *str, int *radix, bool *exactp){
  int start = 0;
  *exactp = true; *radix = 10;
  if(str[start] == '#'){
    switch(str[start + 1]){
    case 'e':
      break;
    case 'i':
      *exactp = false; break;
    case 'b':
      *radix = 2; break;
    case 'o':
      *radix = 8; break;
    case 'd':
      break;
    case 'x':
      *radix = 16; break;
    }
    start += 2;
  }
  if(str[start] == '#'){
    switch(str[start + 1]){
    case 'e':
      break;
    case 'i':
      *exactp = false; break;
    case 'b':
      *radix = 2; break;
    case 'o':
      *radix = 8; break;
    case 'd':
      break;
    case 'x':
      *radix = 16; break;
    }
    start += 2;
  }
  return str + start;
}
