%{
#include <stdlib.h>
#include <stdio.h>

#include "picrin.h"

struct parser_control {
  pic_state *pic;
  pic_value value;
};

#define YYDEBUG 1

%}

%parse-param {struct parser_control *p}
%lex-param {struct parser_control *p}

%union {
  pic_value datum;
}

%token tLPAREN tRPAREN tDOT
%token <datum> tSYMBOL tINT

%type <datum> datum simple_datum symbol compound_datum
%type <datum> number integer list list_tail

%%

program
	:
	{
	  p->value = pic_nil_value(p->pic);
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
;

symbol
	: tSYMBOL
	{
	  $$ = $1;
	}
;

number
	: integer
;

integer
	: tINT
	{
	  $$ = $1;
	}
;

compound_datum
	: list
;

list
	: tLPAREN list_tail
	{
	  $$ = $2;
	}
;

list_tail
	: tRPAREN
	{
	  $$ = pic_nil_value();
	}
	| datum tDOT datum tRPAREN
	{
	  $$ = pic_cons(p->pic, $1, $3);
	}
	| datum list_tail
	{
	  $$ = pic_cons(p->pic, $1, $2);
	}
;

%%

int
yyerror(struct parser_control *p, const char *msg)
{
  puts(msg);
  abort();
}

pic_value
pic_parse(pic_state *pic, const char *str)
{
  struct parser_control p;

  p.pic = pic;
  p.value = pic_int_value(42);

  yy_scan_string(str);
  yyparse(&p);
  yylex_destroy();

  return p.value;
}
