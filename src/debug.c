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
  int ai = pic_gc_arena_preserve(pic);
  pic_callinfo *ci;
  pic_str *trace;

  trace = pic_str_new(pic, NULL, 0);

  for (ci = pic->ci; ci != pic->cibase; --ci) {
    struct pic_proc *proc = pic_proc_ptr(ci->fp[0]);

    trace = pic_strcat(pic, trace, pic_str_new_cstr(pic, "  at "));
    trace = pic_strcat(pic, trace, pic_str_new_cstr(pic, pic_symbol_name(pic, pic_proc_name(proc))));

    if (pic_proc_func_p(proc)) {
      trace = pic_strcat(pic, trace, pic_str_new_cstr(pic, " (native function)\n"));
    } else if (pic_proc_irep_p(proc)) {
      trace = pic_strcat(pic, trace, pic_str_new_cstr(pic, " (unknown location)\n")); /* TODO */
    }
  }

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, pic_obj_value(trace));

  return trace;
}

void
pic_print_backtrace(pic_state *pic, struct pic_error *e)
{
  int ai = pic_gc_arena_preserve(pic);
  pic_str *trace;

  assert(pic->err != NULL);

  trace = pic_str_new(pic, NULL, 0);

  switch (e->type) {
  case PIC_ERROR_OTHER:
    trace = pic_strcat(pic, trace, pic_str_new_cstr(pic, "error: "));
    break;
  case PIC_ERROR_FILE:
    trace = pic_strcat(pic, trace, pic_str_new_cstr(pic, "file error: "));
    break;
  case PIC_ERROR_READ:
    trace = pic_strcat(pic, trace, pic_str_new_cstr(pic, "read error: "));
    break;
  case PIC_ERROR_RAISED:
    trace = pic_strcat(pic, trace, pic_str_new_cstr(pic, "raised: "));
    break;
  }

  trace = pic_strcat(pic, trace, e->msg);

  /* TODO: print error irritants */

  trace = pic_strcat(pic, trace, pic_str_new(pic, "\n", 1));
  trace = pic_strcat(pic, trace, e->stack);

  /* print! */
  printf("%s", pic_str_cstr(trace));

  pic_gc_arena_restore(pic, ai);
}
