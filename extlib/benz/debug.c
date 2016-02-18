/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/object.h"

struct pic_string *
pic_get_backtrace(pic_state *pic)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_callinfo *ci;
  struct pic_string *trace;

  trace = pic_lit_value(pic, "");

  for (ci = pic->ci; ci != pic->cibase; --ci) {
    struct pic_proc *proc = pic_proc_ptr(ci->fp[0]);

    trace = pic_str_cat(pic, trace, pic_lit_value(pic, "  at "));
    trace = pic_str_cat(pic, trace, pic_lit_value(pic, "(anonymous lambda)"));

    if (pic_proc_func_p(proc)) {
      trace = pic_str_cat(pic, trace, pic_lit_value(pic, " (native function)\n"));
    } else if (pic_proc_irep_p(proc)) {
      trace = pic_str_cat(pic, trace, pic_lit_value(pic, " (unknown location)\n")); /* TODO */
    }
  }

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, pic_obj_value(trace));

  return trace;
}

void
pic_print_backtrace(pic_state *pic, xFILE *file)
{
  assert(! pic_invalid_p(pic, pic->err));

  if (! pic_error_p(pic, pic->err)) {
    xfprintf(pic, file, "raise: ");
    pic_fwrite(pic, pic->err, file);
  } else {
    struct pic_error *e;
    pic_value elem, it;

    e = pic_error_ptr(pic->err);
    if (e->type != pic_intern_lit(pic, "")) {
      pic_fwrite(pic, pic_obj_value(e->type), file);
      xfprintf(pic, file, " ");
    }
    xfprintf(pic, file, "error: ");
    pic_fwrite(pic, pic_obj_value(e->msg), file);

    pic_for_each (elem, e->irrs, it) { /* print error irritants */
      xfprintf(pic, file, " ");
      pic_fwrite(pic, elem, file);
    }
    xfprintf(pic, file, "\n");

    xfputs(pic, pic_str(pic, e->stack), file);
  }
}
