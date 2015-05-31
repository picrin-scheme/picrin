/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

struct pic_proc *
pic_make_proc(pic_state *pic, pic_func_t func, const char *name)
{
  struct pic_proc *proc;
  pic_sym *sym;

  assert(name != NULL);

  sym = pic_intern_cstr(pic, name);

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->tag = PIC_PROC_TAG_FUNC;
  proc->u.f.func = func;
  proc->u.f.name = sym;
  return proc;
}

struct pic_proc *
pic_make_proc_irep(pic_state *pic, struct pic_irep *irep, struct pic_context *cxt)
{
  struct pic_proc *proc;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->tag = PIC_PROC_TAG_IREP;
  proc->u.i.irep = irep;
  proc->u.i.cxt = cxt;
  return proc;
}

pic_sym *
pic_proc_name(struct pic_proc *proc)
{
  switch (proc->tag) {
  case PIC_PROC_TAG_FUNC:
    return proc->u.f.name;
  case PIC_PROC_TAG_IREP:
    return proc->u.i.irep->name;
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
