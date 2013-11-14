%{
#include <stdlib.h>
#include <stdio.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/blob.h"

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
  char c;
  struct {
    char *dat;
    int len, capa;
  } blob;
  pic_value datum;
}

%token tDATUM_COMMENT
%token tLPAREN tRPAREN tLBRACKET tRBRACKET tDOT tVPAREN
%token tQUOTE tQUASIQUOTE tUNQUOTE tUNQUOTE_SPLICING
%token <i> tINT tBOOLEAN
%token <f> tFLOAT
%token <cstr> tSYMBOL tSTRING
%token <c> tCHAR
%token <blob> tBYTEVECTOR

%type <datum> program_data
%type <datum> datum simple_datum compound_datum abbrev
%type <datum> list list_data vector vector_data

%%

program
	: program_data
	{
	  p->value = $1;
	}
	| incomplete_program_data
	{
	  p->incomp = true;
	  p->value = pic_undef_value();
	}
	| /* empty line */
	{
	  p->value = pic_nil_value();
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
	| tDATUM_COMMENT datum
	{
	  $$ = pic_nil_value();
	}
;

incomplete_program_data
	: incomplete_datum
	| datum incomplete_program_data
;

datum
	: simple_datum
	| compound_datum
	| tDATUM_COMMENT datum datum
	{
	  $$ = $3;
	}
;

simple_datum
	: tSYMBOL
	{
	  $$ = pic_symbol_value(pic_intern_cstr(p->pic, $1));
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
	| tCHAR
	{
	  $$ = pic_char_value($1);
	}
	| tBYTEVECTOR
	{
	  $$ = pic_obj_value(pic_blob_new(p->pic, $1.dat, $1.len));
	  free($1.dat);
	}
;

compound_datum
	: list
	| vector
	| abbrev
;

list
	: tLPAREN list_data tRPAREN
	{
	  $$ = $2;
	}
	| tLBRACKET list_data tRBRACKET
	{
	  $$ = $2;
	}
;

list_data
	: /* none */
	{
	  $$ = pic_nil_value();
	}
	| tDATUM_COMMENT datum
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

vector
	: tVPAREN vector_data tRPAREN
	{
	  $$ = pic_obj_value(pic_vec_new_from_list(p->pic, $2));
	}
;

vector_data
	: /* none */
	{
	  $$ = pic_nil_value();
	}
	| tDATUM_COMMENT datum
	{
	  $$ = pic_nil_value();
	}
	| datum vector_data
	{
	  $$ = pic_cons(p->pic, $1, $2);
	}
;

abbrev
	: tQUOTE datum
	{
	  $$ = pic_list(p->pic, 2, pic_symbol_value(p->pic->sQUOTE), $2);
	}
	| tQUASIQUOTE datum
	{
	  $$ = pic_list(p->pic, 2, pic_symbol_value(p->pic->sQUASIQUOTE), $2);
	}
	| tUNQUOTE datum
	{
	  $$ = pic_list(p->pic, 2, pic_symbol_value(p->pic->sUNQUOTE), $2);
	}
	| tUNQUOTE_SPLICING datum
	{
	  $$ = pic_list(p->pic, 2, pic_symbol_value(p->pic->sUNQUOTE_SPLICING), $2);
	}
;

incomplete_datum
	: tLPAREN incomplete_data
	| tLBRACKET incomplete_data
	| tVPAREN incomplete_data
	| incomplete_abbrev
	| tDATUM_COMMENT
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

int
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
    return PIC_PARSER_ERROR;
  }
  if (p.incomp) {
    return PIC_PARSER_INCOMPLETE;
  }

  *v = p.value;
  return pic_length(pic, p.value);
}

enum pic_parser_res
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
    return PIC_PARSER_ERROR;
  }
  if (p.incomp) {
    return PIC_PARSER_INCOMPLETE;
  }

  *v = p.value;
  return pic_length(pic, p.value);
}
