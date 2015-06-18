/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

pic_str *
pic_get_backtrace(pic_state *pic)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_callinfo *ci;
  pic_str *trace;

  trace = pic_make_str(pic, NULL, 0);

  for (ci = pic->ci; ci != pic->cibase; --ci) {
    struct pic_proc *proc = pic_proc_ptr(ci->fp[0]);

    trace = pic_str_cat(pic, trace, pic_make_str_cstr(pic, "  at "));
    trace = pic_str_cat(pic, trace, pic_make_str_cstr(pic, pic_symbol_name(pic, pic_proc_name(proc))));

    if (pic_proc_func_p(proc)) {
      trace = pic_str_cat(pic, trace, pic_make_str_cstr(pic, " (native function)\n"));
    } else if (pic_proc_irep_p(proc)) {
      trace = pic_str_cat(pic, trace, pic_make_str_cstr(pic, " (unknown location)\n")); /* TODO */
    }
  }

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, pic_obj_value(trace));

  return trace;
}

void
pic_print_backtrace(pic_state *pic, xFILE *file)
{
  assert(! pic_invalid_p(pic->err));

  if (! pic_error_p(pic->err)) {
    xfprintf(pic, file, "raise: ");
    pic_fwrite(pic, pic->err, file);
  } else {
    struct pic_error *e;

    e = pic_error_ptr(pic->err);
    if (e->type != pic_intern_cstr(pic, "")) {
      pic_fwrite(pic, pic_obj_value(e->type), file);
      xfprintf(pic, file, " ");
    }
    xfprintf(pic, file, "error: ");
    pic_fwrite(pic, pic_obj_value(e->msg), file);
    xfprintf(pic, file, "\n");

    /* TODO: print error irritants */

    xfputs(pic, pic_str_cstr(pic, e->stack), file);
  }
}
