%{
#include <stdlib.h>
#include <stdio.h>

#include "picrin.h"
#include "picrin/pair.h"

#define YYERROR_VERBOSE 1

struct parser_control {
  pic_state *pic;
  void *yyscanner;
  pic_value value;
  bool incomp;
  int yynerrs;
};

void yyerror(struct parser_control *, const char *);

/* just for supressing warnings. a little bit evil */
int yylex();
int yylex_();
void yylex_init();
void yyset_in();
void yy_scan_string();
void yylex_destroy();
%}

%pure_parser
%parse-param {struct parser_control *p}
%lex-param {struct parser_control *p}

%union {
  int i;
  double f;
  char *cstr;
  pic_value datum;
}

%token tLPAREN tRPAREN tDOT
%token tQUOTE tQUASIQUOTE tUNQUOTE tUNQUOTE_SPLICING
%token <i> tINT tBOOLEAN
%token <f> tFLOAT
%token <cstr> tSYMBOL tSTRING

%type <datum> program_data
%type <datum> datum simple_datum compound_datum abbrev
%type <datum> list list_data

%%

program
	: program_data
	{
	  /* if single? */
	  if (pic_eq_p(p->pic, pic_cdr(p->pic, $1), pic_nil_value())) {
	    p->value = pic_car(p->pic, $1);
	  }
	  /* if multiple? */
	  else {
	    p->value = pic_cons(p->pic, p->pic->sBEGIN, $1);
	  }
	}
	| incomplete_program_data
	{
	  p->incomp = true;
	  p->value = pic_undef_value();
	}
;

program_data
	: datum
	{
	  $$ = pic_cons(p->pic, $1, pic_nil_value());
	}
	| datum program_data
	{
	  $$ = pic_cons(p->pic, $1, $2);
	}
;

incomplete_program_data
	: incomplete_datum
	| datum incomplete_program_data
;

datum
	: simple_datum
	| compound_datum
;

simple_datum
	: tSYMBOL
	{
	  $$ = pic_intern_cstr(p->pic, $1);
	  free($1);
	}
	| tSTRING
	{
	  $$ = pic_str_new_cstr(p->pic, $1);
	  free($1);
	}
	| tINT
	{
	  $$ = pic_int_value($1);
	}
	| tFLOAT
	{
	  $$ = pic_float_value($1);
	}
	| tBOOLEAN
	{
	  $$ = pic_bool_value($1);
	}
;

compound_datum
	: list
	| abbrev
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

abbrev
	: tQUOTE datum
	{
	  $$ = pic_list(p->pic, 2, p->pic->sQUOTE, $2);
	}
	| tQUASIQUOTE datum
	{
	  $$ = pic_list(p->pic, 2, p->pic->sQUASIQUOTE, $2);
	}
	| tUNQUOTE datum
	{
	  $$ = pic_list(p->pic, 2, p->pic->sUNQUOTE, $2);
	}
	| tUNQUOTE_SPLICING datum
	{
	  $$ = pic_list(p->pic, 2, p->pic->sUNQUOTE_SPLICING, $2);
	}
;

incomplete_datum
	: tLPAREN incomplete_data
	| incomplete_abbrev
;

incomplete_tail
	: /* none */
	| incomplete_datum
;

incomplete_data
	: incomplete_tail
	| datum tDOT incomplete_tail
	| datum incomplete_data
;

incomplete_abbrev
	: tQUOTE incomplete_tail
	| tQUASIQUOTE incomplete_tail
	| tUNQUOTE incomplete_tail
	| tUNQUOTE_SPLICING incomplete_tail
;

%%

void
yyerror(struct parser_control *p, const char *msg)
{
  puts(msg);
  p->yynerrs++;
}

int
yylex(YYSTYPE *yylvalp, struct parser_control *p)
{
  return yylex_(yylvalp, p->yyscanner);
}

bool
pic_parse_file(pic_state *pic, FILE *file, pic_value *v)
{
  struct parser_control p;

  p.pic = pic;
  p.incomp = false;
  p.yynerrs = 0;

  yylex_init(&p.yyscanner);
  yyset_in(file, p.yyscanner);
  yyparse(&p);
  yylex_destroy(p.yyscanner);

  if (p.yynerrs > 0) {
    p.value = pic_undef_value();
  }

  if (! p.incomp) {
    *v = p.value;
  }
  return ! p.incomp;
}

bool
pic_parse_cstr(pic_state *pic, const char *str, pic_value *v)
{
  struct parser_control p;

  p.pic = pic;
  p.incomp = false;
  p.yynerrs = 0;

  yylex_init(&p.yyscanner);
  yy_scan_string(str, p.yyscanner);
  yyparse(&p);
  yylex_destroy(p.yyscanner);

  if (p.yynerrs > 0) {
    p.value = pic_undef_value();
  }

  if (! p.incomp) {
    *v = p.value;
  }
  return ! p.incomp;
}
