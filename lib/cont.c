/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"
#include "state.h"

struct cont {
  PIC_JMPBUF *jmp;

  ptrdiff_t sp_offset;
  ptrdiff_t ci_offset;
  size_t arena_idx;
  const struct code *ip;
  pic_value dyn_env;

  int retc;
  pic_value *retv;

  struct cont *prev;
};

static const pic_data_type cont_type = { "cont", NULL };

void
pic_save_point(pic_state *pic, struct cont *cont, PIC_JMPBUF *jmp)
{
  cont->jmp = jmp;

  /* save runtime context */
  cont->sp_offset = pic->sp - pic->stbase;
  cont->ci_offset = pic->ci - pic->cibase;
  cont->arena_idx = pic->arena_idx;
  cont->dyn_env = pic->dyn_env;
  cont->ip = pic->ip;
  cont->prev = pic->cc;
  cont->retc = 0;
  cont->retv = NULL;

  pic->cc = cont;
}

void
pic_load_point(pic_state *pic, struct cont *cont)
{
  /* load runtime context */
  pic->sp = pic->stbase + cont->sp_offset;
  pic->ci = pic->cibase + cont->ci_offset;
  pic->arena_idx = cont->arena_idx;
  pic->dyn_env = cont->dyn_env;
  pic->ip = cont->ip;
  pic->cc = cont->prev;
}

void
pic_exit_point(pic_state *pic)
{
  pic->cc = pic->cc->prev;
}

static pic_value
cont_call(pic_state *pic)
{
  int argc;
  pic_value *argv;
  struct cont *cc, *cont;

  pic_get_args(pic, "*", &argc, &argv);

  cont = pic_data(pic, pic_closure_ref(pic, 0));

  /* check if continuation is alive */
  for (cc = pic->cc; cc != NULL; cc = cc->prev) {
    if (cc == cont) {
      break;
    }
  }
  if (cc == NULL) {
    pic_error(pic, "calling dead escape continuation", 0);
  }

  cont->retc = argc;
  cont->retv = argv;

  pic_load_point(pic, cont);

  PIC_LONGJMP(pic, *cont->jmp, 1);
  PIC_UNREACHABLE();
}

pic_value
pic_make_cont(pic_state *pic, struct cont *cont)
{
  return pic_lambda(pic, cont_call, 1, pic_data_value(pic, cont, &cont_type));
}

struct cont *
pic_alloca_cont(pic_state *pic)
{
  return pic_alloca(pic, sizeof(struct cont));
}

static pic_value
values(pic_state *pic, int argc, pic_value *argv)
{
  int i;

  for (i = 0; i < argc; ++i) {
    pic->sp[i] = argv[i];
  }
  pic->ci->retc = argc;

  return argc == 0 ? pic_undef_value(pic) : pic->sp[0];
}

static pic_value
pic_callcc(pic_state *pic, pic_value proc)
{
  PIC_JMPBUF jmp;
  volatile struct cont *cont = pic_alloca_cont(pic);

  if (PIC_SETJMP(pic, jmp)) {
    return values(pic, cont->retc, cont->retv);
  }
  else {
    pic_value val;

    pic_save_point(pic, (struct cont *)cont, &jmp);

    val = pic_call(pic, proc, 1, pic_make_cont(pic, (struct cont *)cont));

    pic_exit_point(pic);

    return val;
  }
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
  pic_value *retv = pic_alloca(pic, sizeof(pic_value) * n);
  int i;

  for (i = 0; i < n; ++i) {
    retv[i] = va_arg(ap, pic_value);
  }
  return values(pic, n, retv);
}

int
pic_receive(pic_state *pic, int n, pic_value *argv)
{
  struct callinfo *ci;
  int i, retc;

  /* take info from discarded frame */
  ci = pic->ci + 1;
  retc = ci->retc;

  for (i = 0; i < retc && i < n; ++i) {
    argv[i] = ci->fp[i];
  }
  return retc;
}

static pic_value
pic_cont_callcc(pic_state *pic)
{
  pic_value f;

  pic_get_args(pic, "l", &f);

  return pic_callcc(pic, f);
}

static pic_value
pic_cont_values(pic_state *pic)
{
  int argc;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  return values(pic, argc, argv);
}

static pic_value
pic_cont_call_with_values(pic_state *pic)
{
  pic_value producer, consumer, *retv;
  int retc;

  pic_get_args(pic, "ll", &producer, &consumer);

  pic_call(pic, producer, 0);

  retc = pic_receive(pic, 0, NULL);
  retv = pic_alloca(pic, sizeof(pic_value) * retc);

  pic_receive(pic, retc, retv);

  return pic_applyk(pic, consumer, retc, retv);
}

void
pic_init_cont(pic_state *pic)
{
  pic_defun(pic, "call-with-current-continuation", pic_cont_callcc);
  pic_defun(pic, "call/cc", pic_cont_callcc);
  pic_defun(pic, "values", pic_cont_values);
  pic_defun(pic, "call-with-values", pic_cont_call_with_values);
}
