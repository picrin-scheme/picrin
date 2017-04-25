/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "../object.h"
#include "../state.h"

#if PIC_USE_CALLCC

/*
 * [(reset e)]k = k ([e] halt ())
 * [(shift e)]k = [e] halt (\c x, c (k x))
 */

static pic_value
pic_cont_reset(pic_state *pic)
{
  pic_value thunk;

  pic_get_args(pic, "l", &thunk);

  return pic_call(pic, thunk, 0);
}

static pic_value
shift_call(pic_state *pic)
{
  pic_value x;
  struct context cxt;

  pic_get_args(pic, "o", &x);

  CONTEXT_INIT(pic, &cxt, pic_closure_ref(pic, 0), 1, &x);
  pic_vm(pic, &cxt);
  return pic_protect(pic, cxt.fp->regs[1]);
}

static pic_value
pic_cont_shift(pic_state *pic)
{
  pic_value f, k;

  pic_get_args(pic, "l", &f);

  k = pic_lambda(pic, shift_call, 1, pic->cxt->fp->regs[1]);
  CONTEXT_INITK(pic, pic->cxt, f, pic->halt, 1, &k);
  return pic_invalid_value(pic);
}

static pic_value
cont_call(pic_state *pic)
{
  int argc;
  pic_value *argv, k, dyn_env;
  struct context *cxt, *c;

  pic_get_args(pic, "*", &argc, &argv);

  cxt = pic_data(pic, pic_closure_ref(pic, 0));
  k = pic_closure_ref(pic, 1);
  dyn_env = pic_closure_ref(pic, 2);

  /* check if continuation is alive */
  for (c = pic->cxt; c != NULL; c = c->prev) {
    if (c == cxt) {
      break;
    }
  }
  if (c == NULL) {
    pic_error(pic, "calling dead escape continuation", 0);
  }

  CONTEXT_INIT(pic, cxt, k, argc, argv);

  pic->cxt = cxt;
  pic->dyn_env = dyn_env;

  longjmp(cxt->jmp, 1);
  PIC_UNREACHABLE();
}

pic_value
pic_make_cont(pic_state *pic, pic_value k)
{
  static const pic_data_type cxt_type = { "cxt", NULL };
  return pic_lambda(pic, cont_call, 3, pic_data_value(pic, pic->cxt, &cxt_type), k, pic->dyn_env);
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
