/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "value.h"
#include "object.h"
#include "state.h"

struct cont {
  PIC_JMPBUF *jmp;

  struct checkpoint *cp;
  ptrdiff_t sp_offset;
  ptrdiff_t ci_offset;
  size_t arena_idx;
  const struct code *ip;

  int retc;
  pic_value *retv;

  struct cont *prev;
};

static const pic_data_type cont_type = { "cont", NULL, NULL };

void
pic_save_point(pic_state *pic, struct cont *cont, PIC_JMPBUF *jmp)
{
  cont->jmp = jmp;

  /* save runtime context */
  cont->cp = pic->cp;
  cont->sp_offset = pic->sp - pic->stbase;
  cont->ci_offset = pic->ci - pic->cibase;
  cont->arena_idx = pic->arena_idx;
  cont->ip = pic->ip;
  cont->prev = pic->cc;
  cont->retc = 0;
  cont->retv = NULL;

  pic->cc = cont;
}

void
pic_load_point(pic_state *pic, struct cont *cont)
{
  pic_wind(pic, pic->cp, cont->cp);

  /* load runtime context */
  pic->cp = cont->cp;
  pic->sp = pic->stbase + cont->sp_offset;
  pic->ci = pic->cibase + cont->ci_offset;
  pic->arena_idx = cont->arena_idx;
  pic->ip = cont->ip;
  pic->cc = cont->prev;
}

void
pic_exit_point(pic_state *pic)
{
  pic->cc = pic->cc->prev;
}

void
pic_wind(pic_state *pic, struct checkpoint *here, struct checkpoint *there)
{
  if (here == there)
    return;

  if (here->depth < there->depth) {
    pic_wind(pic, here, there->prev);
    pic_call(pic, obj_value(there->in), 0);
  }
  else {
    pic_call(pic, obj_value(here->out), 0);
    pic_wind(pic, here->prev, there);
  }
}

pic_value
pic_dynamic_wind(pic_state *pic, pic_value in, pic_value thunk, pic_value out)
{
  struct checkpoint *here;
  pic_value val;

  pic_call(pic, in, 0);       /* enter */

  here = pic->cp;
  pic->cp = (struct checkpoint *)pic_obj_alloc(pic, sizeof(struct checkpoint), PIC_TYPE_CP);
  pic->cp->prev = here;
  pic->cp->depth = here->depth + 1;
  pic->cp->in = pic_proc_ptr(pic, in);
  pic->cp->out = pic_proc_ptr(pic, out);

  val = pic_call(pic, thunk, 0);

  pic->cp = here;

  pic_call(pic, out, 0);      /* exit */

  return val;
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
pic_callcc(pic_state *pic, pic_value proc)
{
  PIC_JMPBUF jmp;
  volatile struct cont *cont = pic_alloca_cont(pic);

  if (PIC_SETJMP(pic, jmp)) {
    return pic_valuesk(pic, cont->retc, cont->retv);
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
pic_return(pic_state *pic, int n, ...)
{
  va_list ap;
  pic_value ret;

  va_start(ap, n);
  ret = pic_vreturn(pic, n, ap);
  va_end(ap);
  return ret;
}

pic_value
pic_vreturn(pic_state *pic, int n, va_list ap)
{
  pic_value *retv = pic_alloca(pic, sizeof(pic_value) * n);
  int i;

  for (i = 0; i < n; ++i) {
    retv[i] = va_arg(ap, pic_value);
  }
  return pic_valuesk(pic, n, retv);
}

pic_value
pic_valuesk(pic_state *pic, int argc, pic_value *argv)
{
  int i;

  for (i = 0; i < argc; ++i) {
    pic->sp[i] = argv[i];
  }
  pic->ci->retc = argc;

  return argc == 0 ? pic_undef_value(pic) : pic->sp[0];
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
pic_cont_dynamic_wind(pic_state *pic)
{
  pic_value in, thunk, out;

  pic_get_args(pic, "lll", &in, &thunk, &out);

  return pic_dynamic_wind(pic, in, thunk, out);
}

static pic_value
pic_cont_values(pic_state *pic)
{
  int argc;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  return pic_valuesk(pic, argc, argv);
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
  pic_defun(pic, "escape", pic_cont_callcc);
  pic_defun(pic, "dynamic-wind", pic_cont_dynamic_wind);

  pic_defun(pic, "values", pic_cont_values);
  pic_defun(pic, "call-with-values", pic_cont_call_with_values);
}
