#include <stdio.h>
#include <stdlib.h>

#include "picrin.h"
#include "picrin/pair.h"

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

void
pic_load_stdlib(pic_state *pic)
{
  static const char *fn = "piclib/built-in.scm";
  FILE *file;
  int n, i, ai;
  pic_value v, vs;
  struct pic_proc *proc;

  file = fopen(fn, "r");
  if (file == NULL) {
    fputs("fatal error: could not read built-in.scm", stderr);
    abort();
  }

  n = pic_parse_file(pic, file, &vs);
  if (n < 0) {
    fputs("fatal error: built-in.scm broken", stderr);
    abort();
  }

  ai = pic_gc_arena_preserve(pic);
  for (i = 0; i < n; ++i, vs = pic_cdr(pic, vs)) {
    v = pic_car(pic, vs);

    proc = pic_codegen(pic, v);
    if (proc == NULL) {
      fprintf(stderr, "in codegen: %s\n", pic->errmsg);
      fputs("fatal error: built-in.scm compilation failure", stderr);
      abort();
    }

    v = pic_apply(pic, proc, pic_nil_value());
    if (pic_undef_p(v)) {
      fprintf(stderr, "in execute: %s\n", pic->errmsg);
      fputs("fatal error: built-in.scm evaluation failure", stderr);
      abort();
    }

    pic_gc_arena_restore(pic, ai);
  }

#if DEBUG
  puts("successfully loaded stdlib");
#endif
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
  int ai;

  ai = pic_gc_arena_preserve(pic);
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

  pic_load_stdlib(pic); DONE;

  pic_defun(pic, "features", pic_features);
}
