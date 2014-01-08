#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/irep.h"

struct pic_proc *
pic_proc_new(pic_state *pic, pic_func_t cfunc)
{
  struct pic_proc *proc;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->cfunc_p = true;
  proc->u.cfunc = cfunc;
  proc->env = NULL;
  return proc;
}

struct pic_proc *
pic_proc_new_irep(pic_state *pic, struct pic_irep *irep, struct pic_env *env)
{
  struct pic_proc *proc;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->cfunc_p = false;
  proc->u.irep = irep;
  proc->env = env;
  return proc;
}

void
pic_proc_cv_reserve(pic_state *pic, struct pic_proc *proc, size_t cv_size)
{
  struct pic_env *env;

  if (proc->env != NULL) {
    pic_error(pic, "env slot already in use");
  }
  env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TT_ENV);
  env->valuec = cv_size;
  env->values = (pic_value *)pic_calloc(pic, cv_size, sizeof(pic_value));
  env->up = NULL;

  proc->env = env;
}

int
pic_proc_cv_size(pic_state *pic, struct pic_proc *proc)
{
  return proc->env ? proc->env->valuec : 0;
}

pic_value
pic_proc_cv_ref(pic_state *pic, struct pic_proc *proc, size_t i)
{
  if (proc->env == NULL) {
    pic_error(pic, "no closed env");
  }
  return proc->env->values[i];
}

void
pic_proc_cv_set(pic_state *pic, struct pic_proc *proc, size_t i, pic_value v)
{
  if (proc->env == NULL) {
    pic_error(pic, "no closed env");
  }
  proc->env->values[i] = v;
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
  struct pic_proc *proc;
  pic_value *args, v;
  size_t argc;
  int i;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  if (argc == 0) {
    pic_error(pic, "apply: wrong number of arguments");
  }
  v = args[argc - 1];
  for (i = argc - 2; i >= 0; --i) {
    v = pic_cons(pic, args[i], v);
  }

  return pic_apply(pic, proc, v);
}

void
pic_init_proc(pic_state *pic)
{
  pic_defun(pic, "procedure?", pic_proc_proc_p);
  pic_defun(pic, "apply", pic_proc_apply);
}
