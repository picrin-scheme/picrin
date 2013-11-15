%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/blob.h"

#define YYERROR_VERBOSE 1

/* just for supressing warnings. a little bit evil */
int yylex();
int yylex_();
void yylex_init();
void yyset_in();
void yy_scan_string();
void yylex_destroy();

struct parser_control {
  pic_state *pic;
  void *yyscanner;
  pic_value value;
  bool incomp;
  int yynerrs;
  struct pic_vector *yy_arena;
  int yy_arena_idx;
};

#define YY_ARENA_SIZE 50

struct parser_control *
parser_control_new(pic_state *pic)
{
  struct parser_control *p;

  p = (struct parser_control *)pic_alloc(pic, sizeof(struct parser_control));
  p->pic = pic;
  p->incomp = false;
  p->yynerrs = 0;
  p->yy_arena = pic_vec_new(pic, YY_ARENA_SIZE);
  p->yy_arena_idx = 0;
  yylex_init(&p->yyscanner);

  return p;
}

void
parser_control_destroy(struct parser_control *p)
{
  pic_state *pic = p->pic;

  yylex_destroy(p->yyscanner);
  pic_free(pic, p);
}

void
yy_obj_protect(struct parser_control *p, struct pic_object *obj)
{
  if (p->yy_arena_idx >= p->yy_arena->len) {
    pic_vec_extend_ip(p->pic, p->yy_arena, p->yy_arena->len * 2);
  }
  p->yy_arena->data[p->yy_arena_idx++] = pic_obj_value(obj);
}

struct pic_object *
yy_obj_alloc(struct parser_control *p, size_t size, enum pic_tt tt)
{
  struct pic_object *obj;

  obj = pic_obj_alloc_unsafe(p->pic, size, tt);
  yy_obj_protect(p, obj);
  return obj;
}

pic_value
yy_cons(struct parser_control *p, pic_value car, pic_value cdr)
{
  struct pic_pair *pair;

  pair = (struct pic_pair *)yy_obj_alloc(p, sizeof(struct pic_pair), PIC_TT_PAIR);
  pair->car = car;
  pair->cdr = cdr;

  return pic_obj_value(pair);
}

pic_value
yy_abbrev(struct parser_control *p, pic_sym sym, pic_value datum)
{
  return yy_cons(p, pic_symbol_value(sym), yy_cons(p, datum, pic_nil_value()));
}

pic_value
yy_str_new_cstr(struct parser_control *p, const char *cstr)
{
  struct pic_string *str;

  str = (struct pic_string *)yy_obj_alloc(p, sizeof(struct pic_string), PIC_TT_STRING);
  str->len = strlen(cstr);
  str->str = strdup(cstr);

  return pic_obj_value(str);
}

pic_value
yy_vec_new_from_list(struct parser_control *p, pic_value data)
{
  struct pic_vector *vec;
  int i, len;

  len = pic_length(p->pic, data);

  vec = (struct pic_vector *)yy_obj_alloc(p, sizeof(struct pic_vector), PIC_TT_VECTOR);
  vec->len = len;
  vec->data = (pic_value *)pic_alloc(p->pic, sizeof(pic_value) * len);
  for (i = 0; i < len; ++i) {
    vec->data[i] = pic_car(p->pic, data);
    data = pic_cdr(p->pic, data);
  }
  return pic_obj_value(vec);
}

pic_value
yy_blob_new(struct parser_control *p, char *dat, int len)
{
  struct pic_blob *bv;

  bv = (struct pic_blob *)yy_obj_alloc(p, sizeof(struct pic_blob), PIC_TT_BLOB);
  bv->data = strndup(dat, len);
  bv->len = len;
  return pic_obj_value(bv);
}

void yyerror(struct parser_control *, const char *);
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
	  $$ = yy_cons(p, $1, pic_nil_value());
	}
	| datum program_data
	{
	  $$ = yy_cons(p, $1, $2);
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
	  $$ = yy_str_new_cstr(p, $1);
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
	  $$ = yy_blob_new(p, $1.dat, $1.len);
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
	  $$ = yy_cons(p, $1, $3);
	}
	| datum list_data
	{
	  $$ = yy_cons(p, $1, $2);
	}
;

vector
	: tVPAREN vector_data tRPAREN
	{
	  $$ = yy_vec_new_from_list(p, $2);
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
	  $$ = yy_cons(p, $1, $2);
	}
;

abbrev
	: tQUOTE datum
	{
	  $$ = yy_abbrev(p, p->pic->sQUOTE, $2);
	}
	| tQUASIQUOTE datum
	{
	  $$ = yy_abbrev(p, p->pic->sQUASIQUOTE, $2);
	}
	| tUNQUOTE datum
	{
	  $$ = yy_abbrev(p, p->pic->sUNQUOTE, $2);
	}
	| tUNQUOTE_SPLICING datum
	{
	  $$ = yy_abbrev(p, p->pic->sUNQUOTE_SPLICING, $2);
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
  struct parser_control *p;
  int r, ai = pic_gc_arena_preserve(pic);

  p = parser_control_new(pic);

  yyset_in(file, p->yyscanner);
  yyparse(p);

  if (p->yynerrs > 0) {
    p->value = pic_undef_value();
    r = PIC_PARSER_ERROR;
  }
  else if (p->incomp) {
    r = PIC_PARSER_INCOMPLETE;
  }
  else {
    r = pic_length(pic, p->value);
  }

  parser_control_destroy(p);

#if DEBUG
  if (pic_gc_arena_preserve(pic) != ai + 1) {
    puts("**logic flaw! yy obj protection failure!**");
  }
#endif

  pic_gc_arena_restore(pic, ai);

  *v = p->value;
  pic_gc_protect(pic, p->value);

  return r;
}

enum pic_parser_res
pic_parse_cstr(pic_state *pic, const char *str, pic_value *v)
{
  struct parser_control *p;
  int r, ai = pic_gc_arena_preserve(pic);

  p = parser_control_new(pic);

  yy_scan_string(str, p->yyscanner);
  yyparse(p);

  if (p->yynerrs > 0) {
    p->value = pic_undef_value();
    r = PIC_PARSER_ERROR;
  }
  else if (p->incomp) {
    r = PIC_PARSER_INCOMPLETE;
  }
  else {
    r = pic_length(pic, p->value);
  }

  parser_control_destroy(p);

#if DEBUG
  if (pic_gc_arena_preserve(pic) != ai + 1) {
    puts("**logic flaw! yy obj protection failure!**");
    printf("%d\n", pic_gc_arena_preserve(pic));
  }
#endif

  pic_gc_arena_restore(pic, ai);

  *v = p->value;
  pic_gc_protect(pic, p->value);

  return r;
}
