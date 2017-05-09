/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "../value.h"
#include "../object.h"
#include "../state.h"

#if PIC_USE_ERROR

# define pic_exc(pic) pic_ref(pic, "current-exception-handlers")

PIC_JMPBUF *
pic_prepare_try(pic_state *pic)
{
  struct context *cxt = pic_malloc(pic, sizeof(struct context));

  cxt->pc = NULL;
  cxt->fp = NULL;
  cxt->sp = NULL;
  cxt->irep = NULL;
  cxt->conts = pic_nil_value(pic);
  cxt->prev = pic->cxt;
  pic->cxt = cxt;
  return &cxt->jmp;
}

static pic_value
native_exception_handler(pic_state *pic)
{
  pic_value err;

  pic_get_args(pic, "o", &err);

  pic_call(pic, pic_closure_ref(pic, 0), 1, err);
  PIC_UNREACHABLE();
}

void
pic_enter_try(pic_state *pic)
{
  pic_value cont, handler;
  pic_value var, env;

  pic->cxt->ai = pic->ai;

  /* call/cc */
  cont = pic_make_cont(pic, pic_invalid_value(pic));
  handler = pic_lambda(pic, native_exception_handler, 1, cont);
  /* with-exception-handler */
  var = pic_exc(pic);
  env = pic_make_attr(pic);
  pic_attr_set(pic, env, var, pic_cons(pic, handler, pic_call(pic, var, 0)));
  pic->dyn_env = pic_cons(pic, env, pic->dyn_env);

  pic_leave(pic, pic->cxt->ai);
}

void
pic_exit_try(pic_state *pic)
{
  struct context *cxt = pic->cxt;
  pic_value c, it;
  pic->dyn_env = pic_cdr(pic, pic->dyn_env);
  pic_for_each (c, cxt->conts, it) {
    proc_ptr(pic, c)->env->regs[0] = pic_false_value(pic);
  }
  pic->cxt = cxt->prev;
  pic_free(pic, cxt);
  /* don't rewind ai here */
}

pic_value
pic_abort_try(pic_state *pic)
{
  struct context *cxt = pic->cxt;
  pic_value c, it;
  pic_value err = cxt->sp->regs[1];
  pic_for_each (c, cxt->conts, it) {
    proc_ptr(pic, c)->env->regs[0] = pic_false_value(pic);
  }
  pic->cxt = cxt->prev;
  pic_free(pic, cxt);
  pic_protect(pic, err);
  return err;
}

#endif
