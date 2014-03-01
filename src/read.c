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

static pic_value read(int tok, yyscan_t scanner);

#define pic (yyget_extra(scanner)->pic)
#define yylval (yyget_extra(scanner)->yylval)
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
  pic_value val;

  val = pic_nil_value();
  while ((tok = gettok(scanner)) != tRPAREN) {
    val = pic_cons(pic, read(tok, scanner), val);
  }
  return pic_vec_new_from_list(pic, pic_reverse(pic, val));
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
  case tSYMBOL:
    return pic_symbol_value(pic_intern(pic, yylval.str.buf, yylval.str.len));

  case tSTRING:
    return pic_obj_value(pic_str_new(pic, yylval.str.buf, yylval.str.len));

  case tINT:
    return pic_int_value(yylval.i);

  case tFLOAT:
    return pic_float_value(yylval.f);

  case tBOOLEAN:
    return pic_bool_value(yylval.i);

  case tCHAR:
    return pic_char_value(yylval.c);

  case tBYTEVECTOR:
    val = pic_obj_value(pic_blob_new(pic, yylval.blob.dat, yylval.blob.len));
    pic_free(pic, yylval.blob.dat);
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

  /* unreachable */
  return pic_undef_value();
}

static pic_value
read(int tok, yyscan_t scanner)
{
  int ai = pic_gc_arena_preserve(pic);
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
    pic_errorf(pic, "read-error: %s", yymsg ? yymsg : "unexpected EOF");
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
      pic_errorf(pic, "read-error: %s", yymsg);
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
  yylex_init_extra(&ctrl, &scanner);
  yy_scan_string(cstr, scanner);

  val = read_one(scanner);

  yylex_destroy(scanner);

  return val;
}

pic_list
pic_read_file(pic_state *pic, FILE *file)
{
  yyscan_t scanner;
  struct parser_control ctrl;
  pic_value vals;

  ctrl.pic = pic;
  yylex_init_extra(&ctrl, &scanner);
  yyset_in(file, scanner);

  vals = read_many(scanner);

  yylex_destroy(scanner);

  return vals;
}

pic_list
pic_read_cstr(pic_state *pic, const char *cstr)
{
  yyscan_t scanner;
  struct parser_control ctrl;
  pic_value vals;

  ctrl.pic = pic;
  yylex_init_extra(&ctrl, &scanner);
  yy_scan_string(cstr, scanner);

  vals = read_many(scanner);

  yylex_destroy(scanner);

  return vals;
}
