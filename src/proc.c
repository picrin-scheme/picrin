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
  proc->aux = pic_undef_value();
  return proc;
}

struct pic_proc *
pic_proc_new_cfunc(pic_state *pic, pic_func_t cfunc, pic_value aux)
{
  struct pic_proc *proc;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc *), PIC_TT_PROC);
  proc->cfunc_p = true;
  proc->u.cfunc = cfunc;
  proc->env = NULL;
  proc->aux = aux;
  return proc;
}
