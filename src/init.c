/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/read.h"
#include "picrin/lib.h"
#include "picrin/macro.h"
#include "picrin/proc.h"
#include "picrin/error.h"

static pic_value
pic_features(pic_state *pic)
{
  pic_value features = pic_nil_value();

  pic_get_args(pic, "");

  pic_push(pic, pic_sym_value(pic_intern_cstr(pic, "r7rs")), features);
  pic_push(pic, pic_sym_value(pic_intern_cstr(pic, "ieee-float")), features);
  pic_push(pic, pic_sym_value(pic_intern_cstr(pic, "picrin")), features);

  return features;
}

static pic_value
pic_libraries(pic_state *pic)
{
  pic_value libs = pic_nil_value(), lib;

  pic_get_args(pic, "");

  pic_for_each (lib, pic->libs) {
    libs = pic_cons(pic, pic_car(pic, lib), libs);
  }

  return libs;
}

bool pic_condexpand_clause(pic_state *, pic_value);

bool
pic_condexpand_feature(pic_state *pic, pic_value name)
{
  pic_value proc_features, features, feature;

  proc_features = pic_ref(pic, "features");

  features = pic_apply(pic, pic_proc_ptr(proc_features), pic_nil_value());
  
  pic_for_each(feature, features){
    if(pic_eq_p(feature, name))
      return true;
  }
  return false;
}

bool
pic_condexpand_library(pic_state *pic, pic_value name)
{
  pic_debug(pic, name);

  if(pic_find_library(pic, name))
    return true;
  else
    return false;
}

bool
pic_condexpand_and(pic_state *pic, pic_value clauses)
{
  pic_value clause;

  pic_for_each(clause, clauses){
    if(!pic_condexpand_clause(pic, clause))
      return false;
  }
  return true;
}

bool
pic_condexpand_or(pic_state *pic, pic_value clauses)
{
  pic_value clause;

  pic_for_each(clause, clauses){
    if(pic_condexpand_clause(pic, clause))
      return true;
  }
  return false;
}

bool
pic_condexpand_not(pic_state *pic, pic_value clause)
{
  return ! pic_condexpand_clause(pic, clause);
}

bool
pic_condexpand_clause(pic_state *pic, pic_value clause)
{
  const pic_sym sELSE = pic_intern_cstr(pic, "else");
  const pic_sym sLIBRARY = pic_intern_cstr(pic, "library");
  const pic_sym sOR = pic_intern_cstr(pic, "or");
  const pic_sym sAND = pic_intern_cstr(pic, "and");
  const pic_sym sNOT = pic_intern_cstr(pic, "not");

  if (pic_eq_p(clause, pic_sym_value(sELSE)))
    return true;
  else if (pic_sym_p(clause))
    return pic_condexpand_feature(pic, clause);
  else if (!pic_pair_p(clause))
    pic_errorf(pic, "invalid 'cond-expand' clause ~s", clause);
  else {
    pic_value car = pic_car(pic, clause);
    pic_value cdr = pic_cdr(pic, clause);
    if(pic_eq_p(car, pic_sym_value(sLIBRARY)))
      return pic_condexpand_library(pic, pic_car(pic, cdr));
    else if(pic_eq_p(car, pic_sym_value(sAND)))
      return pic_condexpand_and(pic, cdr);
    else if(pic_eq_p(car, pic_sym_value(sOR)))
      return pic_condexpand_or(pic, cdr);
    else if(pic_eq_p(car, pic_sym_value(sNOT)))
      return pic_condexpand_not(pic, pic_car(pic, cdr));
    else
      pic_errorf(pic, "unknown 'cond-expand' directive ~s", clause);
    UNREACHABLE();
    return false;
  }
}

pic_value
pic_macro_condexpand(pic_state *pic)
{
  pic_value *clauses;
  size_t argc, i;
  
  pic_get_args(pic, "*", &argc, &clauses);

  for (i = 0; i < argc; i++)
    if(pic_condexpand_clause(pic, pic_car(pic, clauses[i])))
      return pic_cons(pic, pic_sym_value(pic->rBEGIN), pic_cdr(pic, clauses[i]));

  return pic_none_value();
}


void pic_init_bool(pic_state *);
void pic_init_pair(pic_state *);
void pic_init_port(pic_state *);
void pic_init_number(pic_state *);
void pic_init_time(pic_state *);
void pic_init_system(pic_state *);
void pic_init_file(pic_state *);
void pic_init_proc(pic_state *);
void pic_init_symbol(pic_state *);
void pic_init_vector(pic_state *);
void pic_init_blob(pic_state *);
void pic_init_cont(pic_state *);
void pic_init_char(pic_state *);
void pic_init_error(pic_state *);
void pic_init_str(pic_state *);
void pic_init_macro(pic_state *);
void pic_init_var(pic_state *);
void pic_init_load(pic_state *);
void pic_init_write(pic_state *);
void pic_init_read(pic_state *);
void pic_init_dict(pic_state *);
void pic_init_record(pic_state *);
void pic_init_eval(pic_state *);
void pic_init_lib(pic_state *);
void pic_init_contrib(pic_state *);

void pic_load_piclib(pic_state *);

#define DONE pic_gc_arena_restore(pic, ai);

void
pic_init_core(pic_state *pic)
{
  size_t ai = pic_gc_arena_preserve(pic);

  pic_init_reader(pic);

  pic_deflibrary (pic, "(picrin base core)") {
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sDEFINE, pic->rDEFINE);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sSETBANG, pic->rSETBANG);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sQUOTE, pic->rQUOTE);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sLAMBDA, pic->rLAMBDA);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sIF, pic->rIF);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sBEGIN, pic->rBEGIN);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sDEFINE_SYNTAX, pic->rDEFINE_SYNTAX);

    pic_defmacro(pic, pic->sCOND_EXPAND, pic->rCOND_EXPAND, pic_macro_condexpand);
  }

  pic_deflibrary (pic, "(picrin library)") {
    pic_defun(pic, "libraries", pic_libraries);
  }

  pic_deflibrary (pic, "(scheme base)") {
    pic_defun(pic, "features", pic_features);

    pic_init_bool(pic); DONE;
    pic_init_pair(pic); DONE;
    pic_init_port(pic); DONE;
    pic_init_number(pic); DONE;
    pic_init_time(pic); DONE;
    pic_init_system(pic); DONE;
    pic_init_file(pic); DONE;
    pic_init_proc(pic); DONE;
    pic_init_symbol(pic); DONE;
    pic_init_vector(pic); DONE;
    pic_init_blob(pic); DONE;
    pic_init_cont(pic); DONE;
    pic_init_char(pic); DONE;
    pic_init_error(pic); DONE;
    pic_init_str(pic); DONE;
    pic_init_macro(pic); DONE;
    pic_init_var(pic); DONE;
    pic_init_load(pic); DONE;
    pic_init_write(pic); DONE;
    pic_init_read(pic); DONE;
    pic_init_dict(pic); DONE;
    pic_init_record(pic); DONE;
    pic_init_eval(pic); DONE;
    pic_init_lib(pic); DONE;

    pic_load_piclib(pic); DONE;
    pic_init_contrib(pic); DONE;
  }
}
