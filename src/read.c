/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/parse.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/vector.h"
#include "picrin/blob.h"
#include "picrin/port.h"

#define YY_NO_UNISTD_H
#include "lex.yy.h"

static pic_value read(int, yyscan_t);

#define pic (yyget_extra(scanner)->pic)
#define yylval (yyget_extra(scanner)->yylval)
#define yylabels (yyget_extra(scanner)->labels)
#define yymsg (yyget_extra(scanner)->msg)
#define yyjmp (yyget_extra(scanner)->jmp)

static void
error(const char *msg, yyscan_t scanner)
{
  yymsg = msg;
  longjmp(yyjmp, 1);
}

static int
gettok(yyscan_t scanner)
{
  int tok;

  while ((tok = yylex(scanner)) == tDATUM_COMMENT) {
    read(gettok(scanner), scanner); /* discard */
  }
  return tok;
}

static pic_value
read_label_set(int i, yyscan_t scanner)
{
  int tok;
  pic_value val;

  switch (tok = gettok(scanner)) {
  case tLPAREN:
  case tLBRACKET:
    {
      pic_value tmp;

      val = pic_cons(pic, pic_none_value(), pic_none_value());

      xh_put_int(&yylabels, i, &val);

      tmp = read(tok, scanner);
      pic_pair_ptr(val)->car = pic_car(pic, tmp);
      pic_pair_ptr(val)->cdr = pic_cdr(pic, tmp);

      return val;
    }
  case tVPAREN:
    {
      pic_vec *tmp;

      val = pic_obj_value(pic_vec_new(pic, 0));

      xh_put_int(&yylabels, i, &val);

      tmp = pic_vec_ptr(read(tok, scanner));
      SWAP(pic_value *, tmp->data, pic_vec_ptr(val)->data);
      SWAP(size_t, tmp->len, pic_vec_ptr(val)->len);

      return val;
    }
  default:
    {
      val = read(tok, scanner);

      xh_put_int(&yylabels, i, &val);

      return val;
    }
  }
}

static pic_value
read_label_ref(int i, yyscan_t scanner)
{
  xh_entry *e;

  e = xh_get_int(&yylabels, i);
  if (! e) {
    error("label of given index not defined", scanner);
  }
  return xh_val(e, pic_value);
}

static pic_value
read_pair(int tOPEN, yyscan_t scanner)
{
  int tok, tCLOSE = (tOPEN == tLPAREN) ? tRPAREN : tRBRACKET;
  pic_value car, cdr;

  tok = gettok(scanner);
  if (tok == tCLOSE) {
    return pic_nil_value();
  }
  if (tok == tDOT) {
    cdr = read(gettok(scanner), scanner);

    if (gettok(scanner) != tCLOSE) {
      error("unmatched parenthesis", scanner);
    }
    return cdr;
  }
  else {
    car = read(tok, scanner);
    cdr = read_pair(tOPEN, scanner);
    return pic_cons(pic, car, cdr);
  }
}

static pic_vec *
read_vect(yyscan_t scanner)
{
  int tok;
  pic_value head, tail;

  head = tail = pic_cons(pic, pic_nil_value(), pic_nil_value());
  while ((tok = gettok(scanner)) != tRPAREN) {
    tail = (pic_pair_ptr(tail)->cdr = pic_cons(pic, read(tok, scanner), pic_nil_value()));
  }
  return pic_vec_new_from_list(pic, pic_pair_ptr(head)->cdr);
}

static pic_value
read_abbrev(pic_sym sym, yyscan_t scanner)
{
  return pic_cons(pic, pic_sym_value(sym), pic_cons(pic, read(gettok(scanner), scanner), pic_nil_value()));
}

static pic_value
read_datum(int tok, yyscan_t scanner)
{
  pic_value val;

  switch (tok) {
  case tLABEL_SET:
    return read_label_set(yylval.i, scanner);

  case tLABEL_REF:
    return read_label_ref(yylval.i, scanner);

  case tSYMBOL:
    return pic_symbol_value(pic_intern(pic, yylval.buf.dat, yylval.buf.len));

  case tINT:
    return pic_int_value(yylval.i);

  case tFLOAT:
    return pic_float_value(yylval.f);

  case tBOOLEAN:
    return pic_bool_value(yylval.i);

  case tCHAR:
    return pic_char_value(yylval.c);

  case tSTRING:
    val = pic_obj_value(pic_str_new(pic, yylval.buf.dat, yylval.buf.len));
    pic_free(pic, yylval.buf.dat);
    return val;

  case tBYTEVECTOR:
    val = pic_obj_value(pic_blob_new(pic, yylval.buf.dat, yylval.buf.len));
    pic_free(pic, yylval.buf.dat);
    return val;

  case tLPAREN:
  case tLBRACKET:
    return read_pair(tok, scanner);

  case tVPAREN:
    return pic_obj_value(read_vect(scanner));

  case tQUOTE:
    return read_abbrev(pic->sQUOTE, scanner);

  case tQUASIQUOTE:
    return read_abbrev(pic->sQUASIQUOTE, scanner);

  case tUNQUOTE:
    return read_abbrev(pic->sUNQUOTE, scanner);

  case tUNQUOTE_SPLICING:
    return read_abbrev(pic->sUNQUOTE_SPLICING, scanner);

  case tRPAREN:
    error("unexpected close parenthesis", scanner);

  case tRBRACKET:
    error("unexpected close bracket", scanner);

  case tDOT:
    error("unexpected '.'", scanner);

  case tEOF:
    error(NULL, scanner);
  }

  UNREACHABLE();
}

static pic_value
read(int tok, yyscan_t scanner)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = read_datum(tok, scanner);

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, val);
  return val;
}

pic_value
read_one(yyscan_t scanner)
{
  int tok;

  if (setjmp(yyjmp) != 0) {
    pic_errorf(pic, "%s", yymsg ? yymsg : "unexpected EOF");
  }

  if ((tok = gettok(scanner)) == tEOF) {
    return pic_undef_value();
  }
  return read(tok, scanner);
}

pic_list
read_many(yyscan_t scanner)
{
  int tok;
  pic_value vals;

  if (setjmp(yyjmp) != 0) {
    if (yymsg) {
      pic_errorf(pic, "%s", yymsg);
    }
    return pic_undef_value();   /* incomplete string */
  }

  vals = pic_nil_value();
  while ((tok = gettok(scanner)) != tEOF) {
    vals = pic_cons(pic, read(tok, scanner), vals);
  }
  return pic_reverse(pic, vals);
}

#undef pic

pic_value
pic_read(pic_state *pic, const char *cstr)
{
  yyscan_t scanner;
  struct parser_control ctrl;
  pic_value val;

  ctrl.pic = pic;
  xh_init_int(&ctrl.labels, sizeof(pic_value));
  yylex_init_extra(&ctrl, &scanner);
  yy_scan_string(cstr, scanner);

  val = read_one(scanner);

  yylex_destroy(scanner);
  xh_destroy(&ctrl.labels);

  return val;
}

pic_list
pic_parse_file(pic_state *pic, FILE *file)
{
  yyscan_t scanner;
  struct parser_control ctrl;
  pic_value vals;

  ctrl.pic = pic;
  xh_init_int(&ctrl.labels, sizeof(pic_value));
  yylex_init_extra(&ctrl, &scanner);
  yyset_in(file, scanner);

  vals = read_many(scanner);

  yylex_destroy(scanner);
  xh_destroy(&ctrl.labels);

  return vals;
}

pic_list
pic_parse_cstr(pic_state *pic, const char *cstr)
{
  yyscan_t scanner;
  struct parser_control ctrl;
  pic_value vals;

  ctrl.pic = pic;
  xh_init_int(&ctrl.labels, sizeof(pic_value));
  yylex_init_extra(&ctrl, &scanner);
  yy_scan_string(cstr, scanner);

  vals = read_many(scanner);

  yylex_destroy(scanner);
  xh_destroy(&ctrl.labels);

  return vals;
}
