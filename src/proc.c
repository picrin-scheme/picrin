/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/irep.h"
#include "picrin/dict.h"

struct pic_proc *
pic_proc_new(pic_state *pic, pic_func_t func, const char *name)
{
  struct pic_proc *proc;

  assert(name != NULL);

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->kind = PIC_PROC_KIND_FUNC;
  proc->u.func.f = func;
  proc->u.func.name = pic_intern_cstr(pic, name);
  proc->env = NULL;
  proc->attr = NULL;
  return proc;
}

struct pic_proc *
pic_proc_new_irep(pic_state *pic, struct pic_irep *irep, struct pic_env *env)
{
  struct pic_proc *proc;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->kind = PIC_PROC_KIND_IREP;
  proc->u.irep = irep;
  proc->env = env;
  proc->attr = NULL;
  return proc;
}

pic_sym
pic_proc_name(struct pic_proc *proc)
{
  switch (proc->kind) {
  case PIC_PROC_KIND_FUNC:
    return proc->u.func.name;
  case PIC_PROC_KIND_IREP:
    return proc->u.irep->name;
  }
  UNREACHABLE();
}

struct pic_dict *
pic_proc_attr(pic_state *pic, struct pic_proc *proc)
{
  if (proc->attr == NULL) {
    proc->attr = pic_dict_new(pic);
  }
  return proc->attr;
}

void
pic_proc_cv_init(pic_state *pic, struct pic_proc *proc, size_t cv_size)
{
  struct pic_env *env;

  if (proc->env != NULL) {
    pic_error(pic, "env slot already in use");
  }
  env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TT_ENV);
  env->regc = cv_size;
  env->regs = (pic_value *)pic_calloc(pic, cv_size, sizeof(pic_value));
  env->up = NULL;

  proc->env = env;
}

int
pic_proc_cv_size(pic_state *pic, struct pic_proc *proc)
{
  UNUSED(pic);
  return proc->env ? proc->env->regc : 0;
}

pic_value
pic_proc_cv_ref(pic_state *pic, struct pic_proc *proc, size_t i)
{
  if (proc->env == NULL) {
    pic_error(pic, "no closed env");
  }
  return proc->env->regs[i];
}

void
pic_proc_cv_set(pic_state *pic, struct pic_proc *proc, size_t i, pic_value v)
{
  if (proc->env == NULL) {
    pic_error(pic, "no closed env");
  }
  proc->env->regs[i] = v;
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
  pic_value *args;
  size_t argc;
  pic_value arg_list;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  if (argc == 0) {
    pic_error(pic, "apply: wrong number of arguments");
  }

  arg_list = args[--argc];
  while (argc--) {
    arg_list = pic_cons(pic, args[argc], arg_list);
  }

  return pic_apply_trampoline(pic, proc, arg_list);
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

static pic_value
pic_proc_attribute(pic_state *pic)
{
  struct pic_proc *proc;

  pic_get_args(pic, "l", &proc);

  return pic_obj_value(pic_proc_attr(pic, proc));
}

void
pic_init_proc(pic_state *pic)
{
  pic_defun(pic, "procedure?", pic_proc_proc_p);
  pic_defun(pic, "apply", pic_proc_apply);
  pic_defun(pic, "map", pic_proc_map);
  pic_defun(pic, "for-each", pic_proc_for_each);

  pic_deflibrary ("(picrin attribute)") {
    pic_defun(pic, "attribute", pic_proc_attribute);
  }
}
