/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/lib.h"
#include "picrin/macro.h"

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

void
pic_load_stdlib(pic_state *pic)
{
  static const char *filename = "piclib/built-in.scm";

  pic_try {

    /* load 'built-in.scm' */
    pic_load(pic, filename);

#if DEBUG
    puts("successfully loaded stdlib");
#endif

  }
  pic_catch {
    /* error! */
    fputs("fatal error: failure in loading built-in.scm\n", stderr);
    fputs(pic_errmsg(pic), stderr);
    abort();
  }
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
    pic->lib->senv = pic_core_syntactic_env(pic);
    pic_export(pic, pic_intern_cstr(pic, "define"));
    pic_export(pic, pic_intern_cstr(pic, "set!"));
    pic_export(pic, pic_intern_cstr(pic, "quote"));
    pic_export(pic, pic_intern_cstr(pic, "lambda"));
    pic_export(pic, pic_intern_cstr(pic, "if"));
    pic_export(pic, pic_intern_cstr(pic, "begin"));
    pic_export(pic, pic_intern_cstr(pic, "define-syntax"));

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

    pic_load_stdlib(pic); DONE;

    pic_defun(pic, "features", pic_features);

  }
}
