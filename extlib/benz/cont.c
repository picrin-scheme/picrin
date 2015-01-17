/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/proc.h"
#include "picrin/cont.h"
#include "picrin/pair.h"
#include "picrin/data.h"
#include "picrin/error.h"

void
pic_wind(pic_state *pic, struct pic_winder *here, struct pic_winder *there)
{
  if (here == there)
    return;

  if (here->depth < there->depth) {
    pic_wind(pic, here, there->prev);
    pic_apply0(pic, there->in);
  }
  else {
    pic_apply0(pic, there->out);
    pic_wind(pic, here->prev, there);
  }
}

pic_value
pic_dynamic_wind(pic_state *pic, struct pic_proc *in, struct pic_proc *thunk, struct pic_proc *out)
{
  struct pic_winder *here;
  pic_value val;

  if (in != NULL) {
    pic_apply0(pic, in);        /* enter */
  }

  here = pic->wind;
  pic->wind = pic_alloc(pic, sizeof(struct pic_winder));
  pic->wind->prev = here;
  pic->wind->depth = here->depth + 1;
  pic->wind->in = in;
  pic->wind->out = out;

  val = pic_apply0(pic, thunk);

  pic->wind = here;

  if (out != NULL) {
    pic_apply0(pic, out);       /* exit */
  }

  return val;
}

void
pic_save_point(pic_state *pic, struct pic_escape *escape)
{
  escape->valid = true;

  /* save runtime context */
  escape->wind = pic->wind;
  escape->sp_offset = pic->sp - pic->stbase;
  escape->ci_offset = pic->ci - pic->cibase;
  escape->xp_offset = pic->xp - pic->xpbase;
  escape->arena_idx = pic->arena_idx;
  escape->ip = pic->ip;

  escape->results = pic_undef_value();
}

void
pic_load_point(pic_state *pic, struct pic_escape *escape)
{
  if (! escape->valid) {
    pic_errorf(pic, "calling dead escape continuation");
  }

  pic_wind(pic, pic->wind, escape->wind);

  /* load runtime context */
  pic->wind = escape->wind;
  pic->sp = pic->stbase + escape->sp_offset;
  pic->ci = pic->cibase + escape->ci_offset;
  pic->xp = pic->xpbase + escape->xp_offset;
  pic->arena_idx = escape->arena_idx;
  pic->ip = escape->ip;

  escape->valid = false;
}

static pic_value
escape_call(pic_state *pic)
{
  size_t argc;
  pic_value *argv;
  struct pic_data *e;

  pic_get_args(pic, "*", &argc, &argv);

  e = pic_data_ptr(pic_attr_ref(pic, pic_obj_value(pic_get_proc(pic)), "@@escape"));

  pic_load_point(pic, e->data);

  longjmp(((struct pic_escape *)e->data)->jmp, 1);
}

struct pic_proc *
pic_make_econt(pic_state *pic, struct pic_escape *escape)
{
  static const pic_data_type escape_type = { "escape", pic_free, NULL };
  struct pic_proc *cont;
  struct pic_data *e;

  cont = pic_make_proc(pic, escape_call, "<escape-procedure>");

  e = pic_data_alloc(pic, &escape_type, escape);

  /* save the escape continuation in proc */
  pic_attr_set(pic, pic_obj_value(cont), "@@escape", pic_obj_value(e));

  return cont;
}

pic_value
pic_escape(pic_state *pic, struct pic_proc *proc)
{
  struct pic_escape *escape = pic_alloc(pic, sizeof(struct pic_escape));

  pic_save_point(pic, escape);

  if (setjmp(escape->jmp)) {
    return pic_values_by_list(pic, escape->results);
  }
  else {
    pic_value val;

    val = pic_apply1(pic, proc, pic_obj_value(pic_make_econt(pic, escape)));

    escape->valid = false;

    return val;
  }
}

pic_value
pic_values0(pic_state *pic)
{
  return pic_values_by_list(pic, pic_nil_value());
}

pic_value
pic_values1(pic_state *pic, pic_value arg1)
{
  return pic_values_by_list(pic, pic_list1(pic, arg1));
}

pic_value
pic_values2(pic_state *pic, pic_value arg1, pic_value arg2)
{
  return pic_values_by_list(pic, pic_list2(pic, arg1, arg2));
}

pic_value
pic_values3(pic_state *pic, pic_value arg1, pic_value arg2, pic_value arg3)
{
  return pic_values_by_list(pic, pic_list3(pic, arg1, arg2, arg3));
}

pic_value
pic_values4(pic_state *pic, pic_value arg1, pic_value arg2, pic_value arg3, pic_value arg4)
{
  return pic_values_by_list(pic, pic_list4(pic, arg1, arg2, arg3, arg4));
}

pic_value
pic_values5(pic_state *pic, pic_value arg1, pic_value arg2, pic_value arg3, pic_value arg4, pic_value arg5)
{
  return pic_values_by_list(pic, pic_list5(pic, arg1, arg2, arg3, arg4, arg5));
}

pic_value
pic_values_by_array(pic_state *pic, size_t argc, pic_value *argv)
{
  size_t i;

  for (i = 0; i < argc; ++i) {
    pic->sp[i] = argv[i];
  }
  pic->ci->retc = (int)argc;

  return argc == 0 ? pic_none_value() : pic->sp[0];
}

pic_value
pic_values_by_list(pic_state *pic, pic_value list)
{
  pic_value v;
  int i;

  i = 0;
  pic_for_each (v, list) {
    pic->sp[i++] = v;
  }
  pic->ci->retc = i;

  return pic_nil_p(list) ? pic_none_value() : pic->sp[0];
}

size_t
pic_receive(pic_state *pic, size_t n, pic_value *argv)
{
  pic_callinfo *ci;
  size_t i, retc;

  /* take info from discarded frame */
  ci = pic->ci + 1;
  retc = (size_t)ci->retc;

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

  return pic_escape(pic, cb);
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
  size_t argc;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  return pic_values_by_array(pic, argc, argv);
}

static pic_value
pic_cont_call_with_values(pic_state *pic)
{
  struct pic_proc *producer, *consumer;
  size_t argc;
  pic_value args[256];

  pic_get_args(pic, "ll", &producer, &consumer);

  pic_apply(pic, producer, pic_nil_value());

  argc = pic_receive(pic, 256, args);

  return pic_apply_trampoline(pic, consumer, pic_list_by_array(pic, argc, args));
}

void
pic_init_cont(pic_state *pic)
{
  pic_defun(pic, "call-with-current-continuation", pic_cont_callcc);
  pic_defun(pic, "call/cc", pic_cont_callcc);
  pic_defun(pic, "dynamic-wind", pic_cont_dynamic_wind);
  pic_defun(pic, "values", pic_cont_values);
  pic_defun(pic, "call-with-values", pic_cont_call_with_values);
}
