#include "picrin.h"
#include "picrin/proc.h"
#include "picrin/irep.h"

struct pic_proc *
pic_proc_new(pic_state *pic, struct pic_irep *irep, struct pic_env *env)
{
  struct pic_proc *proc;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc *), PIC_TT_PROC);
  proc->cfunc_p = false;
  proc->u.irep = irep;
  proc->env = env;
  return proc;
}

struct pic_proc *
pic_proc_new_cfunc(pic_state *pic, pic_func_t cfunc)
{
  struct pic_proc *proc;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc *), PIC_TT_PROC);
  proc->cfunc_p = true;
  proc->u.cfunc = cfunc;
  proc->env = NULL;
  return proc;
}

static pic_value
pic_proc_proc_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_proc_p(v));
}

static pic_value
pic_proc_apply(pic_state *pic)
{
  pic_value proc, args;

  pic_get_args(pic, "oo", &proc, &args);

  if (! pic_proc_p(proc)) {
    pic_error(pic, "apply: expected procedure");
  }

  return pic_apply(pic, pic_proc_ptr(proc), args);
}

void
pic_init_proc(pic_state *pic)
{
  pic_defun(pic, "procedure?", pic_proc_proc_p);
  pic_defun(pic, "apply", pic_proc_apply);
}
