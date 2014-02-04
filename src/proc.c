/**
 * See Copyright Notice in picrin.h
 */

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
pic_proc_cv_init(pic_state *pic, struct pic_proc *proc, size_t cv_size)
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
  UNUSED(pic);
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
  pic_value *args, arg_list;
  size_t argc;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  if (argc == 0) {
    pic_error(pic, "apply: wrong number of arguments");
  }
  arg_list = args[--argc];
  while (argc--) {
    arg_list = pic_cons(pic, args[argc], arg_list);
  }
  return pic_apply(pic, proc, arg_list);
}

static pic_value
pic_proc_map(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc;
  pic_value *args;
  int i;
  pic_value cars, ret;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  ret = pic_nil_value();
  do {
    cars = pic_nil_value();
    for (i = argc - 1; i >= 0; --i) {
      if (! pic_pair_p(args[i])) {
        break;
      }
      cars = pic_cons(pic, pic_car(pic, args[i]), cars);
      args[i] = pic_cdr(pic, args[i]);
    }
    if (i >= 0)
      break;
    ret = pic_cons(pic, pic_apply(pic, proc, cars), ret);
  } while (1);

  return pic_reverse(pic, ret);
}

static pic_value
pic_proc_for_each(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc;
  pic_value *args;
  int i;
  pic_value cars;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  do {
    cars = pic_nil_value();
    for (i = argc - 1; i >= 0; --i) {
      if (! pic_pair_p(args[i])) {
        break;
      }
      cars = pic_cons(pic, pic_car(pic, args[i]), cars);
      args[i] = pic_cdr(pic, args[i]);
    }
    if (i >= 0)
      break;
    pic_apply(pic, proc, cars);
  } while (1);

  return pic_none_value();
}

void
pic_init_proc(pic_state *pic)
{
  pic_defun(pic, "procedure?", pic_proc_proc_p);
  pic_defun(pic, "apply", pic_proc_apply);
  pic_defun(pic, "map", pic_proc_map);
  pic_defun(pic, "for-each", pic_proc_for_each);
}
