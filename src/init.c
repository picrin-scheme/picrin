/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/lib.h"
#include "picrin/macro.h"
#include "picrin/error.h"

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

void pic_load_piclib(pic_state *);

void
pic_init_contrib(pic_state *pic)
{
  PIC_CONTRIB_INITS
}

#define PUSH_SYM(pic, lst, name)		\
  lst = pic_cons(pic, pic_symbol_value(pic_intern_cstr(pic, name)), lst)

static pic_value
pic_features(pic_state *pic)
{
  pic_value fs = pic_nil_value();

  pic_get_args(pic, "");

  PUSH_SYM(pic, fs, "r7rs");
  PUSH_SYM(pic, fs, "ieee-float");
  PUSH_SYM(pic, fs, "picrin");

  return fs;
}

#define DONE pic_gc_arena_restore(pic, ai);

void
pic_init_core(pic_state *pic)
{
  int ai = pic_gc_arena_preserve(pic);

  pic_deflibrary ("(scheme base)") {

    /* load core syntaces */
    pic->lib->senv = pic_null_syntactic_environment(pic);
    pic_define_syntactic_keyword(pic, pic->lib->senv, pic->sDEFINE);
    pic_define_syntactic_keyword(pic, pic->lib->senv, pic->sSETBANG);
    pic_define_syntactic_keyword(pic, pic->lib->senv, pic->sQUOTE);
    pic_define_syntactic_keyword(pic, pic->lib->senv, pic->sLAMBDA);
    pic_define_syntactic_keyword(pic, pic->lib->senv, pic->sIF);
    pic_define_syntactic_keyword(pic, pic->lib->senv, pic->sBEGIN);
    pic_define_syntactic_keyword(pic, pic->lib->senv, pic->sDEFINE_SYNTAX);

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

    pic_load_piclib(pic); DONE;

    pic_init_contrib(pic); DONE;

    pic_defun(pic, "features", pic_features);

  }
}
