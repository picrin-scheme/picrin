/**
 * See Copyright Notice in picrin.h
 */

#include <setjmp.h>
#include <string.h>
#include <stdarg.h>

#include "picrin.h"
#include "picrin/proc.h"
#include "picrin/cont.h"
#include "picrin/pair.h"
#include "picrin/error.h"

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
  pic->ci->retc = argc;

  return argc == 0 ? pic_none_value() : pic->sp[0];
}

pic_value
pic_values_by_list(pic_state *pic, pic_value list)
{
  pic_value v;
  size_t i;

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
  retc = ci->retc;

  for (i = 0; i < retc && i < n; ++i) {
    argv[i] = ci->fp[i];
  }

  return retc;
}

static void save_cont(pic_state *, struct pic_cont **);
static void restore_cont(pic_state *, struct pic_cont *);

static ptrdiff_t
native_stack_length(pic_state *pic, char **pos)
{
  char t;

  *pos = (pic->native_stack_start > &t)
    ? &t
    : pic->native_stack_start;

  return (pic->native_stack_start > &t)
    ? pic->native_stack_start - &t
    : &t - pic->native_stack_start;
}

static void
save_cont(pic_state *pic, struct pic_cont **c)
{
  void pic_vm_tear_off(pic_state *);
  struct pic_cont *cont;
  char *pos;

  pic_vm_tear_off(pic);         /* tear off */

  cont = *c = (struct pic_cont *)pic_obj_alloc(pic, sizeof(struct pic_cont), PIC_TT_CONT);

  cont->wind = pic->wind;

  cont->stk_len = native_stack_length(pic, &pos);
  cont->stk_pos = pos;
  assert(cont->stk_len > 0);
  cont->stk_ptr = pic_alloc(pic, cont->stk_len);
  memcpy(cont->stk_ptr, cont->stk_pos, cont->stk_len);

  cont->sp_offset = pic->sp - pic->stbase;
  cont->st_len = pic->stend - pic->stbase;
  cont->st_ptr = pic_alloc(pic, sizeof(pic_value) * cont->st_len);
  memcpy(cont->st_ptr, pic->stbase, sizeof(pic_value) * cont->st_len);

  cont->ci_offset = pic->ci - pic->cibase;
  cont->ci_len = pic->ciend - pic->cibase;
  cont->ci_ptr = pic_alloc(pic, sizeof(pic_callinfo) * cont->ci_len);
  memcpy(cont->ci_ptr, pic->cibase, sizeof(pic_callinfo) * cont->ci_len);

  cont->xp_offset = pic->xp - pic->xpbase;
  cont->xp_len = pic->xpend - pic->xpbase;
  cont->xp_ptr = pic_alloc(pic, sizeof(struct pic_proc *) * cont->xp_len);
  memcpy(cont->xp_ptr, pic->xpbase, sizeof(struct pic_proc *) * cont->xp_len);

  cont->ip = pic->ip;

  cont->arena_idx = pic->arena_idx;
  cont->arena_size = pic->arena_size;
  cont->arena = (struct pic_object **)pic_alloc(pic, sizeof(struct pic_object *) * pic->arena_size);
  memcpy(cont->arena, pic->arena, sizeof(struct pic_object *) * pic->arena_size);

  cont->results = pic_undef_value();
}

static void
native_stack_extend(pic_state *pic, struct pic_cont *cont)
{
  volatile pic_value v[1024];

  ((void)v);
  restore_cont(pic, cont);
}

noreturn static void
restore_cont(pic_state *pic, struct pic_cont *cont)
{
  char v;
  struct pic_cont *tmp = cont;

  if (&v < pic->native_stack_start) {
    if (&v > cont->stk_pos) native_stack_extend(pic, cont);
  }
  else {
    if (&v > cont->stk_pos + cont->stk_len) native_stack_extend(pic, cont);
  }

  pic->wind = cont->wind;

  pic->stbase = pic_realloc(pic, pic->stbase, sizeof(pic_value) * cont->st_len);
  memcpy(pic->stbase, cont->st_ptr, sizeof(pic_value) * cont->st_len);
  pic->sp = pic->stbase + cont->sp_offset;
  pic->stend = pic->stbase + cont->st_len;

  pic->cibase = pic_realloc(pic, pic->cibase, sizeof(pic_callinfo) * cont->ci_len);
  memcpy(pic->cibase, cont->ci_ptr, sizeof(pic_callinfo) * cont->ci_len);
  pic->ci = pic->cibase + cont->ci_offset;
  pic->ciend = pic->cibase + cont->ci_len;

  pic->xpbase = pic_realloc(pic, pic->xpbase, sizeof(struct pic_proc *) * cont->xp_len);
  memcpy(pic->xpbase, cont->xp_ptr, sizeof(struct pic_proc *) * cont->xp_len);
  pic->xp = pic->xpbase + cont->xp_offset;
  pic->xpend = pic->xpbase + cont->xp_len;

  pic->ip = cont->ip;

  pic->arena = (struct pic_object **)pic_realloc(pic, pic->arena, sizeof(struct pic_object *) * cont->arena_size);
  memcpy(pic->arena, cont->arena, sizeof(struct pic_object *) * cont->arena_size);
  pic->arena_size = cont->arena_size;
  pic->arena_idx = cont->arena_idx;

  memcpy(cont->stk_pos, cont->stk_ptr, cont->stk_len);

  longjmp(tmp->jmp, 1);
}

static void
do_wind(pic_state *pic, struct pic_winder *here, struct pic_winder *there)
{
  if (here == there)
    return;

  if (here->depth < there->depth) {
    do_wind(pic, here, there->prev);
    pic_apply0(pic, there->in);
  }
  else {
    pic_apply0(pic, there->out);
    do_wind(pic, here->prev, there);
  }
}

static pic_value
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

noreturn static pic_value
cont_call(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc;
  pic_value *argv;
  struct pic_cont *cont;

  proc = pic_get_proc(pic);
  pic_get_args(pic, "*", &argc, &argv);

  cont = (struct pic_cont *)pic_ptr(pic_attr_ref(pic, proc, "@@cont"));
  cont->results = pic_list_by_array(pic, argc, argv);

  /* execute guard handlers */
  do_wind(pic, pic->wind, cont->wind);

  restore_cont(pic, cont);
}

pic_value
pic_callcc(pic_state *pic, struct pic_proc *proc)
{
  struct pic_cont *cont;

  save_cont(pic, &cont);
  if (setjmp(cont->jmp)) {
    return pic_values_by_list(pic, cont->results);
  }
  else {
    struct pic_proc *c;

    c = pic_make_proc(pic, cont_call, "<continuation-procedure>");

    /* save the continuation object in proc */
    pic_attr_set(pic, c, "@@cont", pic_obj_value(cont));

    return pic_apply1(pic, proc, pic_obj_value(c));
  }
}

static pic_value
pic_callcc_trampoline(pic_state *pic, struct pic_proc *proc)
{
  struct pic_cont *cont;

  save_cont(pic, &cont);
  if (setjmp(cont->jmp)) {
    return pic_values_by_list(pic, cont->results);
  }
  else {
    struct pic_proc *c;

    c = pic_make_proc(pic, cont_call, "<continuation-procedure>");

    /* save the continuation object in proc */
    pic_attr_set(pic, c, "@@cont", pic_obj_value(cont));

    return pic_apply_trampoline(pic, proc, pic_list1(pic, pic_obj_value(c)));
  }
}

static pic_value
pic_cont_callcc(pic_state *pic)
{
  struct pic_proc *cb;

  pic_get_args(pic, "l", &cb);

  return pic_callcc_trampoline(pic, cb);
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
