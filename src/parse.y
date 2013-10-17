%{
#include <stdlib.h>
#include <stdio.h>

#include "picrin.h"

#define YYERROR_VERBOSE 1

struct parser_control {
  pic_state *pic;
  pic_value value;
};

%}

%parse-param {struct parser_control *p}
%lex-param {struct parser_control *p}

%union {
  pic_value datum;
}

%token tLPAREN tRPAREN tDOT
%token <datum> tSYMBOL tNUMBER tBOOLEAN

%type <datum> datum simple_datum symbol compound_datum
%type <datum> number list list_data

%%

program
	:
	{
	  p->value = pic_undef_value(p->pic);
	}
	| datum
	{
	  p->value = $1;
	}
;

datum
	: simple_datum
	| compound_datum
;

simple_datum
	: symbol
	| number
	| boolean
;

symbol
	: tSYMBOL
	{
	  $$ = $1;
	}
;

number
	: tNUMBER
;

boolean
	: tBOOLEAN
;

compound_datum
	: list
;

list
	: tLPAREN list_data tRPAREN
	{
	  $$ = $2;
	}
;

list_data
	: /* none */
	{
	  $$ = pic_nil_value();
	}
	| datum tDOT datum
	{
	  $$ = pic_cons(p->pic, $1, $3);
	}
	| datum list_data
	{
	  $$ = pic_cons(p->pic, $1, $2);
	}
;

%%

int
yyerror(struct parser_control *p, const char *msg)
{
  puts(msg);
}

pic_value
pic_parse(pic_state *pic, const char *str)
{
  struct parser_control p;

  p.pic = pic;

  yy_scan_string(str);
  yyparse(&p);
  yylex_destroy();

  if (yynerrs > 0) {
    p.value = pic_undef_value();
  }

  return p.value;
}
