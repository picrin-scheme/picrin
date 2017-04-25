/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "../object.h"
#include "../state.h"

#if PIC_USE_CALLCC

static pic_value
cont_call(pic_state *pic)
{
  int argc;
  pic_value *argv, k, dyn_env;
  struct context *cxt, *c;
  int i;

  pic_get_args(pic, "*", &argc, &argv);

  cxt = pic_data(pic, pic_closure_ref(pic, 0));

  /* check if continuation is alive */
  for (c = pic->cxt; c != NULL; c = c->prev) {
    if (c == cxt) {
      break;
    }
  }
  if (c == NULL) {
    pic_error(pic, "calling dead escape continuation", 0);
  }

  k = pic_closure_ref(pic, 1);
  dyn_env = pic_closure_ref(pic, 2);

#define MKCALLK(argc)                                                   \
  (cxt->tmpcode[0] = OP_CALL, cxt->tmpcode[1] = (argc), cxt->tmpcode)

  cxt->pc = MKCALLK(argc);
  cxt->sp = pic_make_frame_unsafe(pic, argc + 2);
  cxt->sp->regs[0] = k;
  for (i = 0; i < argc; ++i) {
    cxt->sp->regs[i + 1] = argv[i];
  }
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
  pic_value f, args[1];

  pic_get_args(pic, "l", &f);

  args[0] = pic_make_cont(pic, pic->cxt->fp->regs[1]);
  return pic_applyk(pic, f, 1, args);
}

void
pic_init_cont(pic_state *pic)
{
  pic_defun(pic, "call-with-current-continuation", pic_cont_callcc);
  pic_defun(pic, "call/cc", pic_cont_callcc);
}

#endif  /* PIC_USE_CALCC */
