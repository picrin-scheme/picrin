/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"
#include "state.h"

#if PIC_USE_CALLCC

static pic_value
cont_call(pic_state *pic)
{
  int argc;
  pic_value *argv;
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

#define MKCALLK(argc)                                                   \
  (cxt->tmpcode[0] = OP_CALL, cxt->tmpcode[1] = (argc), cxt->tmpcode)

  cxt->pc = MKCALLK(argc);
  cxt->sp = pic_make_frame_unsafe(pic, argc + 2);
  cxt->sp->regs[0] = pic_closure_ref(pic, 1); /* cont. */
  for (i = 0; i < argc; ++i) {
    cxt->sp->regs[i + 1] = argv[i];
  }
  pic->cxt = cxt;

  longjmp(cxt->jmp, 1);
  PIC_UNREACHABLE();
}

pic_value
pic_make_cont(pic_state *pic, struct context *cxt, pic_value k)
{
  static const pic_data_type cxt_type = { "cxt", NULL };
  return pic_lambda(pic, cont_call, 2, pic_data_value(pic, cxt, &cxt_type), k);
}

static pic_value
pic_cont_callcc(pic_state *pic)
{
  pic_value f, args[1];

  pic_get_args(pic, "l", &f);

  args[0] = pic_make_cont(pic, pic->cxt, pic->cxt->fp->regs[1]);
  return pic_applyk(pic, f, 1, args);
}

#endif  /* PIC_USE_CALCC */

static pic_value
applyk(pic_state *pic, pic_value proc, pic_value cont, int argc, pic_value *argv)
{
  int i;

#define MKCALL(argc)                                                    \
  (pic->cxt->tmpcode[0] = OP_CALL, pic->cxt->tmpcode[1] = (argc), pic->cxt->tmpcode)

  pic->cxt->pc = MKCALL(argc + 1);
  pic->cxt->sp = pic_make_frame_unsafe(pic, argc + 3);
  pic->cxt->sp->regs[0] = proc;
  pic->cxt->sp->regs[1] = cont;
  for (i = 0; i < argc; ++i) {
    pic->cxt->sp->regs[i + 2] = argv[i];
  }
  return pic_invalid_value(pic);
}

static pic_value
valuesk(pic_state *pic, int argc, pic_value *argv)
{
  int i;

  pic->cxt->pc = MKCALL(argc);
  pic->cxt->sp = pic_make_frame_unsafe(pic, argc + 2);
  pic->cxt->sp->regs[0] = pic->cxt->fp->regs[1];
  for (i = 0; i < argc; ++i) {
    pic->cxt->sp->regs[i + 1] = argv[i];
  }
  return pic_invalid_value(pic);
}

pic_value
pic_values(pic_state *pic, int n, ...)
{
  va_list ap;
  pic_value ret;

  va_start(ap, n);
  ret = pic_vvalues(pic, n, ap);
  va_end(ap);
  return ret;
}

pic_value
pic_vvalues(pic_state *pic, int n, va_list ap)
{
  pic_value *retv;
  int i;

  if (n == 1) {
    return va_arg(ap, pic_value);
  }
  retv = pic_alloca(pic, sizeof(pic_value) * n);
  for (i = 0; i < n; ++i) {
    retv[i] = va_arg(ap, pic_value);
  }
  return valuesk(pic, n, retv);
}

static pic_value
pic_cont_values(pic_state *pic)
{
  int argc;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  if (argc == 1) {
    return argv[0];
  }
  return valuesk(pic, argc, argv);
}

static pic_value
receive_call(pic_state *pic)
{
  int argc = pic->cxt->pc[1];
  pic_value *args = &pic->cxt->fp->regs[1], consumer, cont;

  /* receive_call is an inhabitant in the continuation side.
     You can not use pic_get_args since it implicitly consumes the first argument. */

  consumer = pic_closure_ref(pic, 0);
  cont = pic_closure_ref(pic, 1);

  return applyk(pic, consumer, cont, argc, args);
}

static pic_value
pic_cont_call_with_values(pic_state *pic)
{
  pic_value producer, consumer, k;

  pic_get_args(pic, "ll", &producer, &consumer);

  k = pic_lambda(pic, receive_call, 2, consumer, pic->cxt->fp->regs[1]);

  return applyk(pic, producer, k, 0, NULL);
}

void
pic_init_cont(pic_state *pic)
{
#if PIC_USE_CALLCC
  pic_defun(pic, "call-with-current-continuation", pic_cont_callcc);
  pic_defun(pic, "call/cc", pic_cont_callcc);
#endif
  pic_defun(pic, "values", pic_cont_values);
  pic_defun(pic, "call-with-values", pic_cont_call_with_values);
}
