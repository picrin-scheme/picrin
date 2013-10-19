%{
#include <stdlib.h>
#include <stdio.h>

#include "picrin.h"

#define YYERROR_VERBOSE 1

struct parser_control {
  pic_state *pic;
  pic_value value;
  bool incomp;
};

void init_scanner(const char *);
void final_scanner(void);

void yyerror(struct parser_control *, const char *);
int yylex(struct parser_control *);
%}

%parse-param {struct parser_control *p}
%lex-param {struct parser_control *p}

%union {
  pic_value datum;
}

%token tLPAREN tRPAREN tDOT
%token <datum> tSYMBOL tNUMBER tBOOLEAN

%type <datum> datum simple_datum symbol compound_datum
%type <datum> number boolean list list_data

%%

program
	:
	{
	  p->value = pic_undef_value();
	}
	| datum
	{
	  p->value = $1;
	}
	| incomplete_datum
	{
	  p->incomp = true;
	  p->value = pic_undef_value();
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

incomplete_datum
	: tLPAREN incomplete_data
;

incomplete_data
	:  /* none */
	| datum tDOT
	| datum incomplete_datum
	| datum tDOT incomplete_datum
	| datum incomplete_data
;

%%

void
yyerror(struct parser_control *p, const char *msg)
{
  puts(msg);
}

bool
pic_parse(pic_state *pic, const char *str, pic_value *v)
{
  struct parser_control p;

  p.pic = pic;
  p.incomp = false;

  init_scanner(str);
  yyparse(&p);
  final_scanner();

  if (yynerrs > 0) {
    p.value = pic_undef_value();
  }

  if (! p.incomp) {
    *v = p.value;
  }
  return ! p.incomp;
}
