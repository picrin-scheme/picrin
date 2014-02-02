/**
 * See Copyright Notice in picrin.h
 */

#include <setjmp.h>
#include <string.h>

#include "picrin.h"
#include "picrin/proc.h"
#include "picrin/cont.h"

static void save_cont(pic_state *, struct pic_cont **);
static void restore_cont(pic_state *, struct pic_cont *);

static size_t
native_stack_length(pic_state *pic, pic_value **pos)
{
  pic_value t;

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
  pic_value *pos;

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

  cont->ridx = pic->ridx;
  cont->rlen = pic->rlen;
  cont->rescue = (struct pic_proc **)pic_alloc(pic, sizeof(struct pic_proc *) * cont->rlen);
  memcpy(cont->rescue, pic->rescue, sizeof(struct pic_proc *) * cont->rlen);

  cont->arena_idx = pic->arena_idx;
  memcpy(cont->arena, pic->arena, sizeof(struct pic_object *) * PIC_ARENA_SIZE);

  cont->result = pic_undef_value();
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
  pic_value v;
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
walk_to_block(pic_state *pic, struct pic_block *here, struct pic_block *there)
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
  pic_value v;
  struct pic_cont *cont;

  proc = pic_get_proc(pic);
  pic_get_args(pic, "o", &v);

  cont = (struct pic_cont *)pic_ptr(proc->env->values[0]);
  cont->result = v;

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
    return cont->result;
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

pic_value
pic_values(pic_state *pic, size_t c, ...)
{
  va_list ap;
  size_t i;
  pic_value head = pic_none_value();

  va_start(ap, c);

  for (i = 0; i < c; ++i) {
    pic->ci->fp[i] = va_arg(ap, pic_value);
    if (i == 0) {
      head = pic->ci->fp[0];
    }
  }
  pic->ci->fp[i] = pic_undef_value();

  va_end(ap);

  return head;
}

pic_value
pic_values_from_array(pic_state *pic, size_t argc, pic_value *argv)
{
  size_t i;
  pic_value head = pic_none_value();

  for (i = 0; i < argc; ++i) {
    pic->ci->fp[i] = argv[i];
    if (i == 0) {
      head = pic->ci->fp[0];
    }
  }
  pic->ci->fp[i] = pic_undef_value();

  return head;
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
  pic_value v;

  pic_get_args(pic, "lll", &in, &thunk, &out);

  /* enter */
  pic_apply_argv(pic, in, 0);
  {
    struct pic_block *here;

    here = pic->blk;
    pic->blk = (struct pic_block *)pic_alloc(pic, sizeof(struct pic_block));
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

void
pic_init_cont(pic_state *pic)
{
  pic_defun(pic, "call-with-current-continuation", pic_cont_callcc);
  pic_defun(pic, "call/cc", pic_cont_callcc);
  pic_defun(pic, "dynamic-wind", pic_cont_dynamic_wind);
}
