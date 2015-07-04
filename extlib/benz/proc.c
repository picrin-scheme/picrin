/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

struct pic_proc *
pic_make_proc(pic_state *pic, pic_func_t func)
{
  struct pic_proc *proc;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->tag = PIC_PROC_TAG_FUNC;
  proc->u.f.func = func;
  proc->u.f.env = NULL;
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

struct pic_dict *
pic_proc_env(pic_state *pic, struct pic_proc *proc)
{
  assert(pic_proc_func_p(proc));

  if (! proc->u.f.env) {
    proc->u.f.env = pic_make_dict(pic);
  }
  return proc->u.f.env;
}

bool
pic_proc_env_has(pic_state *pic, struct pic_proc *proc, const char *key)
{
  return pic_dict_has(pic, pic_proc_env(pic, proc), pic_intern_cstr(pic, key));
}

pic_value
pic_proc_env_ref(pic_state *pic, struct pic_proc *proc, const char *key)
{
  return pic_dict_ref(pic, pic_proc_env(pic, proc), pic_intern_cstr(pic, key));
}

void
pic_proc_env_set(pic_state *pic, struct pic_proc *proc, const char *key, pic_value val)
{
  pic_dict_set(pic, pic_proc_env(pic, proc), pic_intern_cstr(pic, key), val);
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

  return pic_apply_trampoline_list(pic, proc, arg_list);
}

void
pic_init_proc(pic_state *pic)
{
  pic_defun(pic, "procedure?", pic_proc_proc_p);
  pic_defun(pic, "apply", pic_proc_apply);
}
