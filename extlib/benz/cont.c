/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/object.h"

void
pic_save_point(pic_state *pic, struct pic_cont *cont)
{
  /* save runtime context */
  cont->cp = pic->cp;
  cont->sp_offset = pic->sp - pic->stbase;
  cont->ci_offset = pic->ci - pic->cibase;
  cont->xp_offset = pic->xp - pic->xpbase;
  cont->arena_idx = pic->arena_idx;
  cont->ip = pic->ip;
  cont->ptable = pic->ptable;
  cont->prev = pic->cc;
  cont->results = pic_make_vec(pic, 0, NULL);
  cont->id = pic->ccnt++;

  pic->cc = cont;
}

void
pic_load_point(pic_state *pic, struct pic_cont *cont)
{
  pic_wind(pic, pic->cp, cont->cp);

  /* load runtime context */
  pic->cp = cont->cp;
  pic->sp = pic->stbase + cont->sp_offset;
  pic->ci = pic->cibase + cont->ci_offset;
  pic->xp = pic->xpbase + cont->xp_offset;
  pic->arena_idx = cont->arena_idx;
  pic->ip = cont->ip;
  pic->ptable = cont->ptable;
  pic->cc = cont->prev;
}

void
pic_wind(pic_state *pic, pic_checkpoint *here, pic_checkpoint *there)
{
  if (here == there)
    return;

  if (here->depth < there->depth) {
    pic_wind(pic, here, there->prev);
    pic_call(pic, there->in, 0);
  }
  else {
    pic_call(pic, there->out, 0);
    pic_wind(pic, here->prev, there);
  }
}

static pic_value
pic_dynamic_wind(pic_state *pic, struct pic_proc *in, struct pic_proc *thunk, struct pic_proc *out)
{
  pic_checkpoint *here;
  pic_value val;

  if (in != NULL) {
    pic_call(pic, in, 0);       /* enter */
  }

  here = pic->cp;
  pic->cp = (pic_checkpoint *)pic_obj_alloc(pic, sizeof(pic_checkpoint), PIC_TYPE_CP);
  pic->cp->prev = here;
  pic->cp->depth = here->depth + 1;
  pic->cp->in = in;
  pic->cp->out = out;

  val = pic_call(pic, thunk, 0);

  pic->cp = here;

  if (out != NULL) {
    pic_call(pic, out, 0);      /* exit */
  }

  return val;
}

#define CV_ID 0
#define CV_ESCAPE 1

static pic_value
cont_call(pic_state *pic)
{
  int argc;
  pic_value *argv;
  int id;
  struct pic_cont *cc, *cont;

  pic_get_args(pic, "*", &argc, &argv);

  id = pic_int(pic, pic_closure_ref(pic, CV_ID));

  /* check if continuation is alive */
  for (cc = pic->cc; cc != NULL; cc = cc->prev) {
    if (cc->id == id) {
      break;
    }
  }
  if (cc == NULL) {
    pic_errorf(pic, "calling dead escape continuation");
  }

  cont = pic_data_ptr(pic_closure_ref(pic, CV_ESCAPE))->data;
  cont->results = pic_make_vec(pic, argc, argv);

  pic_load_point(pic, cont);

  PIC_LONGJMP(pic, cont->jmp, 1);

  PIC_UNREACHABLE();
}

struct pic_proc *
pic_make_cont(pic_state *pic, struct pic_cont *cont)
{
  static const pic_data_type cont_type = { "cont", NULL, NULL };
  struct pic_proc *c;

  /* save the escape continuation in proc */
  c = pic_lambda(pic, cont_call, 2, pic_int_value(pic, cont->id), pic_obj_value(pic_data_value(pic, cont, &cont_type)));

  return c;
}

static pic_value
pic_callcc(pic_state *pic, struct pic_proc *proc)
{
  struct pic_cont cont;

  pic_save_point(pic, &cont);

  if (PIC_SETJMP(pic, cont.jmp)) {
    return pic_valuesk(pic, cont.results->len, cont.results->data);
  }
  else {
    pic_value val;

    val = pic_call(pic, proc, 1, pic_obj_value(pic_make_cont(pic, &cont)));

    pic->cc = pic->cc->prev;

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
  pic_callinfo *ci;
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
  struct pic_proc *cb;

  pic_get_args(pic, "l", &cb);

  return pic_callcc(pic, cb);
}

static pic_value
pic_cont_dynamic_wind(pic_state *pic)
{
  struct pic_proc *in, *thunk, *out;

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
  struct pic_proc *producer, *consumer;
  int argc;
  pic_vec *args;

  pic_get_args(pic, "ll", &producer, &consumer);

  pic_call(pic, producer, 0);

  argc = pic_receive(pic, 0, NULL);
  args = pic_make_vec(pic, argc, NULL);

  pic_receive(pic, argc, args->data);

  return pic_applyk(pic, consumer, argc, args->data);
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
