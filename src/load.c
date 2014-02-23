/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"

pic_value
pic_load(pic_state *pic, const char *fn)
{
  FILE *file;
  int n, i, ai;
  pic_value v, vs;
  struct pic_proc *proc;

  file = fopen(fn, "r");
  if (file == NULL) {
    pic_error(pic, "load: could not read file");
  }

  n = pic_parse_file(pic, file, &vs);
  if (n < 0) {
    pic_error(pic, "load: parse failure");
  }

  ai = pic_gc_arena_preserve(pic);
  for (i = 0; i < n; ++i, vs = pic_cdr(pic, vs)) {
    v = pic_car(pic, vs);

    proc = pic_compile(pic, v);
    if (proc == NULL) {
      pic_error(pic, "load: compilation failure");
    }

    v = pic_apply(pic, proc, pic_nil_value());
    if (pic_undef_p(v)) {
      pic_error(pic, "load: evaluation failure");
    }

    pic_gc_arena_restore(pic, ai);
  }

  return pic_none_value();
}

static pic_value
pic_load_load(pic_state *pic)
{
  pic_value envid;
  char *fn;

  pic_get_args(pic, "z|o", &fn, &envid);

  return pic_load(pic, fn);
}

void
pic_init_load(pic_state *pic)
{
  pic_deflibrary ("(scheme load)") {
    pic_defun(pic, "load", pic_load_load);
  }
}
