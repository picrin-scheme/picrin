#include <setjmp.h>
#include <string.h>

#include "picrin.h"
#include "picrin/proc.h"
#include "picrin/cont.h"

static struct pic_cont *save_cont(pic_state *pic);
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

static struct pic_cont *
save_cont(pic_state *pic)
{
  struct pic_cont *cont;
  pic_value *pos;

  cont = (struct pic_cont *)pic_obj_alloc(pic, sizeof(struct pic_cont), PIC_TT_CONT);

  cont->blk = pic->blk;
  PIC_BLK_INCREF(pic, cont->blk);

  cont->stk_len = native_stack_length(pic, &pos);
  cont->stk_pos = pos;
  cont->stk_ptr = pic_alloc(pic, sizeof(pic_value) * cont->stk_len);
  memcpy(cont->stk_ptr, cont->stk_pos, sizeof(pic_value) * cont->stk_len);

  cont->sp = pic->sp;
  cont->st_len = pic->stend - pic->stbase;
  cont->st_ptr = (pic_value *)pic_alloc(pic, sizeof(pic_value) * cont->st_len);
  memcpy(cont->st_ptr, pic->stbase, sizeof(pic_value) * cont->st_len);

  cont->ci = pic->ci;
  cont->ci_len = pic->ciend - pic->cibase;
  cont->ci_ptr = (pic_callinfo *)pic_alloc(pic, sizeof(pic_callinfo) * cont->ci_len);
  memcpy(cont->ci_ptr, pic->cibase, sizeof(pic_callinfo) * cont->ci_len);

  cont->arena_idx = pic->arena_idx;
  memcpy(cont->arena, pic->arena, sizeof(struct pic_object *) * PIC_ARENA_SIZE);

  cont->result = pic_undef_value();

  return cont;
}

static void
native_stack_extend(pic_state *pic, struct pic_cont *cont)
{
  volatile pic_value v[1024];

  ((void)v);
  restore_cont(pic, cont);
}

static void
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

  pic->sp = cont->sp;
  memcpy(pic->stbase, cont->st_ptr, sizeof(pic_value) * cont->st_len);

  pic->ci = cont->ci;
  memcpy(pic->cibase, cont->ci_ptr, sizeof(pic_callinfo) * cont->ci_len);

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

static pic_value
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

  /* the function never returns */
  return pic_undef_value();
}

static pic_value
pic_cont_callcc(pic_state *pic)
{
  struct pic_cont *cont;
  pic_value v;
  struct pic_proc *cb;

  pic_get_args(pic, "o", &v);

  if (! pic_proc_p(v)) {
    pic_error(pic, "expected procedure");
  }
  cb = pic_proc_ptr(v);

  cont = save_cont(pic);
  if (setjmp(cont->jmp)) {
    return cont->result;
  }
  else {
    struct pic_proc *c;

    c = pic_proc_new_cfunc(pic, cont_call);
    /* save the continuation object in proc */
    c->env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TT_ENV);
    c->env->up = NULL;
    c->env->valuec = 1;
    c->env->values = (pic_value *)pic_calloc(pic, 1, sizeof(pic_value));
    c->env->values[0] = pic_obj_value(cont);

    return pic_apply_argv(pic, cb, 1, pic_obj_value(c));
  }
}

static pic_value
pic_cont_dynamic_wind(pic_state *pic)
{
  pic_value a,b,c,v;
  struct pic_proc *in, *thunk, *out;

  pic_get_args(pic, "ooo", &a, &b, &c);

  if (! pic_proc_p(a)) {
    pic_error(pic, "procedure expected");
  }
  in = pic_proc_ptr(a);
  if (! pic_proc_p(b)) {
    pic_error(pic, "procedure expected");
  }
  thunk = pic_proc_ptr(b);
  if (! pic_proc_p(c)) {
    pic_error(pic, "procedure expected");
  }
  out = pic_proc_ptr(c);

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
