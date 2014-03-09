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

pic_value
pic_values(pic_state *pic, size_t c, ...)
{
  va_list ap;
  size_t i;

  va_start(ap, c);

  for (i = 0; i < c; ++i) {
    pic->sp[i] = va_arg(ap, pic_value);
  }
  pic->ci->retc = c;

  va_end(ap);

  return c == 0 ? pic_none_value() : pic->sp[0];
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

static size_t
native_stack_length(pic_state *pic, char **pos)
{
  char t;

  *pos = (pic->native_stack_start > &t)
    ? &t
    : pic->native_stack_start;

  return (pic->native_stack_start > &t)
    ? (size_t)(pic->native_stack_start - &t)
    : (size_t)(&t - pic->native_stack_start + 1);
}

static void
save_cont(pic_state *pic, struct pic_cont **c)
{
  struct pic_cont *cont;
  char *pos;

  cont = *c = (struct pic_cont *)pic_obj_alloc(pic, sizeof(struct pic_cont), PIC_TT_CONT);

  cont->blk = pic->blk;
  PIC_BLK_INCREF(pic, cont->blk);

  cont->stk_len = native_stack_length(pic, &pos);
  cont->stk_pos = pos;
  cont->stk_ptr = pic_alloc(pic, sizeof(pic_value) * cont->stk_len);
  memcpy(cont->stk_ptr, cont->stk_pos, sizeof(pic_value) * cont->stk_len);

  cont->sp_offset = pic->sp - pic->stbase;
  cont->st_len = pic->stend - pic->stbase;
  cont->st_ptr = (pic_value *)pic_alloc(pic, sizeof(pic_value) * cont->st_len);
  memcpy(cont->st_ptr, pic->stbase, sizeof(pic_value) * cont->st_len);

  cont->ci_offset = pic->ci - pic->cibase;
  cont->ci_len = pic->ciend - pic->cibase;
  cont->ci_ptr = (pic_callinfo *)pic_alloc(pic, sizeof(pic_callinfo) * cont->ci_len);
  memcpy(cont->ci_ptr, pic->cibase, sizeof(pic_callinfo) * cont->ci_len);

  cont->ip = pic->ip;

  cont->ridx = pic->ridx;
  cont->rlen = pic->rlen;
  cont->rescue = (struct pic_proc **)pic_alloc(pic, sizeof(struct pic_proc *) * cont->rlen);
  memcpy(cont->rescue, pic->rescue, sizeof(struct pic_proc *) * cont->rlen);

  cont->arena_idx = pic->arena_idx;
  memcpy(cont->arena, pic->arena, sizeof(struct pic_object *) * PIC_ARENA_SIZE);

  cont->results = pic_undef_value();
}

static void
native_stack_extend(pic_state *pic, struct pic_cont *cont)
{
  volatile pic_value v[1024];

  ((void)v);
  restore_cont(pic, cont);
}

NORETURN static void
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

  PIC_BLK_DECREF(pic, pic->blk);
  PIC_BLK_INCREF(pic, cont->blk);
  pic->blk = cont->blk;

  pic->stbase = (pic_value *)pic_realloc(pic, pic->stbase, sizeof(pic_value) * cont->st_len);
  memcpy(pic->stbase, cont->st_ptr, sizeof(pic_value) * cont->st_len);
  pic->sp = pic->stbase + cont->sp_offset;
  pic->stend = pic->stbase + cont->st_len;

  pic->cibase = (pic_callinfo *)pic_realloc(pic, pic->cibase, sizeof(pic_callinfo) * cont->ci_len);
  memcpy(pic->cibase, cont->ci_ptr, sizeof(pic_callinfo) * cont->ci_len);
  pic->ci = pic->cibase + cont->ci_offset;
  pic->ciend = pic->cibase + cont->ci_len;

  pic->ip = cont->ip;

  pic->rescue = (struct pic_proc **)pic_realloc(pic, pic->rescue, sizeof(struct pic_proc *) * cont->rlen);
  memcpy(pic->rescue, cont->rescue, sizeof(struct pic_object *) * cont->rlen);
  pic->ridx = cont->ridx;
  pic->rlen = cont->rlen;

  memcpy(pic->arena, cont->arena, sizeof(struct pic_object *) * PIC_ARENA_SIZE);
  pic->arena_idx = cont->arena_idx;

  memcpy(cont->stk_pos, cont->stk_ptr, sizeof(pic_value) * cont->stk_len);

  longjmp(tmp->jmp, 1);
}

static void
walk_to_block(pic_state *pic, pic_block *here, pic_block *there)
{
  if (here == there)
    return;

  if (here->depth < there->depth) {
    walk_to_block(pic, here, there->prev);
    pic_apply_argv(pic, there->in, 0);
  }
  else {
    pic_apply_argv(pic, there->out, 0);
    walk_to_block(pic, here->prev, there);
  }
}

NORETURN static pic_value
cont_call(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc;
  pic_value *argv;
  struct pic_cont *cont;

  proc = pic_get_proc(pic);
  pic_get_args(pic, "*", &argc, &argv);

  cont = (struct pic_cont *)pic_ptr(proc->env->values[0]);
  cont->results = pic_list_by_array(pic, argc, argv);

  /* execute guard handlers */
  walk_to_block(pic, pic->blk, cont->blk);

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

    c = pic_proc_new(pic, cont_call);

    /* save the continuation object in proc */
    pic_proc_cv_init(pic, c, 1);
    pic_proc_cv_set(pic, c, 0, pic_obj_value(cont));

    return pic_apply_argv(pic, proc, 1, pic_obj_value(c));
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

    c = pic_proc_new(pic, cont_call);

    /* save the continuation object in proc */
    pic_proc_cv_init(pic, c, 1);
    pic_proc_cv_set(pic, c, 0, pic_obj_value(cont));

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
  pic_value v;

  pic_get_args(pic, "lll", &in, &thunk, &out);

  /* enter */
  pic_apply_argv(pic, in, 0);
  {
    pic_block *here;

    here = pic->blk;
    pic->blk = (pic_block *)pic_alloc(pic, sizeof(pic_block));
    pic->blk->prev = here;
    pic->blk->depth = here->depth + 1;
    pic->blk->in = in;
    pic->blk->out = out;
    pic->blk->refcnt = 1;
    PIC_BLK_INCREF(pic, here);

    v = pic_apply_argv(pic, thunk, 0);

    PIC_BLK_DECREF(pic, pic->blk);
    pic->blk = here;
  }
  /* exit */
  pic_apply_argv(pic, out, 0);

  return v;
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
