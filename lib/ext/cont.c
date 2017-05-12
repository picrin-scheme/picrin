/**
 * See Copyright Notice in picrin.h
 */

#include <picrin.h>
#include "../value.h"
#include "../object.h"
#include "../state.h"

#if PIC_USE_CONT

/*
 * [(reset e)]k = k ([e] halt ())
 * [(shift e)]k = [e] halt (\c x, c (k x))
 */

static pic_value
pic_cont_reset(pic_state *pic)
{
  pic_value thunk, prev = pic_ref(pic, "__picrin_dynenv__");
  struct context cxt;

  pic_get_args(pic, "l", &thunk);

  CONTEXT_INITK(pic, &cxt, thunk, pic->halt, 0, (pic_value *) NULL);
  cxt.reset = 1;
  pic_vm(pic, &cxt);
  pic_set(pic, "__picrin_dynenv__", prev);
  return pic_protect(pic, cxt.fp->regs[1]);
}

static pic_value
shift_call(pic_state *pic)
{
  pic_value x, prev = pic_ref(pic, "__picrin_dynenv__");
  struct context cxt;

  pic_get_args(pic, "o", &x);

  CONTEXT_INIT(pic, &cxt, pic_closure_ref(pic, 0), 1, &x);
  cxt.reset = 1;
  pic_set(pic, "__picrin_dynenv__", pic_closure_ref(pic, 1));
  pic_vm(pic, &cxt);
  pic_set(pic, "__picrin_dynenv__", prev);
  return pic_protect(pic, cxt.fp->regs[1]);
}

static pic_value
pic_cont_shift(pic_state *pic)
{
  pic_value f, k;

  pic_get_args(pic, "l", &f);

  if (! pic->cxt->reset) {
    pic_error(pic, "c function call interleaved in delimited continuation", 0);
  }

  k = pic_lambda(pic, shift_call, 2, pic->cxt->fp->regs[1], pic_ref(pic, "__picrin_dynenv__"));
  CONTEXT_INITK(pic, pic->cxt, f, pic->halt, 1, &k);
  return pic_invalid_value(pic);
}

static pic_value
cont_call(pic_state *pic)
{
  int argc;
  pic_value *argv, k, dyn_env;
  struct context *cxt;

  pic_get_args(pic, "*", &argc, &argv);

  if (! pic_bool(pic, pic_closure_ref(pic, 0))) {
    pic_error(pic, "calling dead escape continuation", 0);
  }

  cxt = pic_data(pic, pic_closure_ref(pic, 1));
  k = pic_closure_ref(pic, 2);
  dyn_env = pic_closure_ref(pic, 3);

  CONTEXT_INIT(pic, cxt, k, argc, argv);

  while (pic->cxt != cxt) {
    pic_value c, it;
    pic_for_each (c, pic->cxt->conts, it) {
      proc_ptr(pic, c)->env->regs[0] = pic_false_value(pic);
    }
    pic->cxt = pic->cxt->prev;
  }
  pic_set(pic, "__picrin_dynenv__", dyn_env);

  longjmp(cxt->jmp, 1);
  PIC_UNREACHABLE();
}

pic_value
pic_make_cont(pic_state *pic, pic_value k)
{
  static const pic_data_type cxt_type = { "cxt", NULL };
  pic_value c;
  c = pic_lambda(pic, cont_call, 4, pic_true_value(pic), pic_data_value(pic, pic->cxt, &cxt_type), k, pic_ref(pic, "__picrin_dynenv__"));
  pic->cxt->conts = pic_cons(pic, c, pic->cxt->conts);
  return c;
}

static pic_value
pic_cont_callcc(pic_state *pic)
{
  pic_value f;

  pic_get_args(pic, "l", &f);

  return pic_callk(pic, f, 1, pic_make_cont(pic, pic->cxt->fp->regs[1]));
}

void
pic_init_cont(pic_state *pic)
{
  pic_defun(pic, "call-with-current-continuation", pic_cont_callcc);
  pic_defun(pic, "call/cc", pic_cont_callcc);
  pic_defun(pic, "shift", pic_cont_shift);
  pic_defun(pic, "reset", pic_cont_reset);
}

#endif  /* PIC_USE_CALCC */
