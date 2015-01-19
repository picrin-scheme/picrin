/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/string.h"
#include "picrin/error.h"
#include "picrin/proc.h"

pic_str *
pic_get_backtrace(pic_state *pic)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_callinfo *ci;
  pic_str *trace;

  trace = pic_make_str(pic, NULL, 0);

  for (ci = pic->ci; ci != pic->cibase; --ci) {
    struct pic_proc *proc = pic_proc_ptr(ci->fp[0]);

    trace = pic_strcat(pic, trace, pic_make_str_cstr(pic, "  at "));
    trace = pic_strcat(pic, trace, pic_make_str_cstr(pic, pic_symbol_name(pic, pic_proc_name(proc))));

    if (pic_proc_func_p(proc)) {
      trace = pic_strcat(pic, trace, pic_make_str_cstr(pic, " (native function)\n"));
    } else if (pic_proc_irep_p(proc)) {
      trace = pic_strcat(pic, trace, pic_make_str_cstr(pic, " (unknown location)\n")); /* TODO */
    }
  }

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, pic_obj_value(trace));

  return trace;
}

void
pic_print_backtrace(pic_state *pic)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_str *trace;

  assert(! pic_undef_p(pic->err));

  if (! pic_error_p(pic->err)) {
    trace = pic_format(pic, "raised: ~s", pic->err);
  } else {
    struct pic_error *e;

    e = pic_error_ptr(pic->err);
    if (e->type != pic_intern_cstr(pic, "")) {
      trace = pic_format(pic, "~s ", pic_obj_value(e->type));
    } else {
      trace = pic_make_str(pic, NULL, 0);
    }
    trace = pic_strcat(pic, trace, pic_format(pic, "error: ~s", pic_obj_value(e->msg)));

    /* TODO: print error irritants */

    trace = pic_strcat(pic, trace, pic_make_str(pic, "\n", 1));
    trace = pic_strcat(pic, trace, e->stack);
  }

  /* print! */
  xfprintf(xstderr, "%s", pic_str_cstr(trace));

  pic_gc_arena_restore(pic, ai);
}
