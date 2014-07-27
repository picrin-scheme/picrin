/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"

pic_value
pic_load_cstr(pic_state *pic, const char *src)
{
  size_t ai;
  pic_value v, exprs;
  struct pic_proc *proc;

  exprs = pic_parse_cstr(pic, src);
  if (pic_undef_p(exprs)) {
    pic_errorf(pic, "load: read failure (%s)", pic_errmsg(pic));
  }

  pic_for_each (v, exprs) {
    ai = pic_gc_arena_preserve(pic);

    proc = pic_compile(pic, v, pic->lib);
    if (proc == NULL) {
      pic_error(pic, "load: compilation failure");
    }

    pic_apply(pic, proc, pic_nil_value());

    pic_gc_arena_restore(pic, ai);
  }

  return pic_none_value();
}

pic_value
pic_load(pic_state *pic, const char *fn)
{
  FILE *file;
  size_t ai;
  pic_value v, exprs;
  struct pic_proc *proc;

  file = fopen(fn, "r");
  if (file == NULL) {
    pic_errorf(pic, "load: could not read file \"%s\"", fn);
  }

  exprs = pic_parse_file(pic, file);
  if (pic_undef_p(exprs)) {
    pic_errorf(pic, "load: read failure (%s)", pic_errmsg(pic));
  }

  pic_for_each (v, exprs) {
    ai = pic_gc_arena_preserve(pic);

    proc = pic_compile(pic, v, pic->lib);
    if (proc == NULL) {
      pic_error(pic, "load: compilation failure");
    }

    pic_apply(pic, proc, pic_nil_value());

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
