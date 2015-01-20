/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/irep.h"

struct pic_proc *
pic_make_proc(pic_state *pic, pic_func_t func, const char *name)
{
  struct pic_proc *proc;

  assert(name != NULL);

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->kind = PIC_PROC_KIND_FUNC;
  proc->u.func.f = func;
  proc->u.func.name = pic_intern_cstr(pic, name);
  proc->env = NULL;
  return proc;
}

struct pic_proc *
pic_make_proc_irep(pic_state *pic, struct pic_irep *irep, struct pic_env *env)
{
  struct pic_proc *proc;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->kind = PIC_PROC_KIND_IREP;
  proc->u.irep = irep;
  proc->env = env;
  return proc;
}

pic_sym *
pic_proc_name(struct pic_proc *proc)
{
  switch (proc->kind) {
  case PIC_PROC_KIND_FUNC:
    return proc->u.func.name;
  case PIC_PROC_KIND_IREP:
    return proc->u.irep->name;
  }
  PIC_UNREACHABLE();
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
    pic_errorf(pic, "apply: wrong number of arguments");
  }

  arg_list = args[--argc];
  while (argc--) {
    arg_list = pic_cons(pic, args[argc], arg_list);
  }

  return pic_apply_trampoline(pic, proc, arg_list);
}

void
pic_init_proc(pic_state *pic)
{
  pic_defun(pic, "procedure?", pic_proc_proc_p);
  pic_defun(pic, "apply", pic_proc_apply);
}
