#include <setjmp.h>
#include <string.h>

#include "picrin.h"
#include "picrin/proc.h"
#include "picrin/cont.h"

static void restore_cont(pic_state *, struct pic_cont *);

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

  pic->sp = cont->sp;
  pic->stbase = cont->stbase;
  pic->stend = cont->stend;
  pic->ci = cont->ci;
  pic->cibase = cont->cibase;
  pic->ciend = cont->ciend;
  memcpy(pic->arena, cont->arena, sizeof(struct pic_object *) * PIC_ARENA_SIZE);
  pic->arena_idx = cont->arena_idx;

  memcpy(cont->stk_pos, cont->stk_ptr, sizeof(pic_value) * cont->stk_len);

  longjmp(tmp->jmp, 1);
}

static pic_value
pic_cont_call(pic_state *pic)
{
  struct pic_proc *proc;
  pic_value v;
  struct pic_cont *cont;

  proc = pic_get_proc(pic);
  pic_get_args(pic, "o", &v);

  cont = (struct pic_cont *)pic_ptr(proc->env->values[0]);
  cont->result = v;

  restore_cont(pic, cont);

  /* the function never returns */
  return pic_undef_value();
}

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

  cont->stk_len = native_stack_length(pic, &pos);
  cont->stk_pos = pos;
  cont->stk_ptr = pic_alloc(pic, sizeof(pic_value) * cont->stk_len);
  memcpy(cont->stk_ptr, cont->stk_pos, sizeof(pic_value) * cont->stk_len);

  cont->stbase = (pic_value *)pic_alloc(pic, sizeof(pic_value) * (pic->stend - pic->stbase));
  cont->stend = cont->stbase + (pic->stend - pic->stbase);
  cont->sp = cont->stbase + (pic->sp - pic->stbase);
  memcpy(cont->stbase, pic->stbase, sizeof(pic_value) * (pic->stend - pic->stbase));

  cont->cibase = (pic_callinfo *)pic_alloc(pic, sizeof(pic_callinfo) * (pic->ciend - pic->cibase));
  cont->ciend = cont->cibase + (pic->ciend - pic->cibase);
  cont->ci = cont->cibase + (pic->ci - pic->cibase);
  memcpy(cont->cibase, pic->cibase, sizeof(pic_callinfo) * (pic->ciend - pic->cibase));

  cont->arena = (struct pic_object **)pic_alloc(pic, sizeof(struct pic_object *) * PIC_ARENA_SIZE);
  cont->arena_idx = pic->arena_idx;
  memcpy(cont->arena, pic->arena, sizeof(struct pic_object *) * PIC_ARENA_SIZE);

  cont->result = pic_undef_value();

  return cont;
}

static pic_value
pic_callcc(pic_state *pic)
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

    c = pic_proc_new_cfunc(pic, pic_cont_call);
    /* save the continuation object in proc */
    c->env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TT_ENV);
    c->env->up = NULL;
    c->env->valuec = 1;
    c->env->values = (pic_value *)pic_calloc(pic, 1, sizeof(pic_value));
    c->env->values[0] = pic_obj_value(cont);

    return pic_apply_argv(pic, cb, 1, pic_obj_value(c));
  }
}

void
pic_init_cont(pic_state *pic)
{
  pic_defun(pic, "call-with-current-continuation", pic_callcc);
  pic_defun(pic, "call/cc", pic_callcc);
}
