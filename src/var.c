/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/proc.h"
#include "picrin/var.h"

struct pic_var *
pic_var_new(pic_state *pic, pic_value init, struct pic_proc *conv /* = NULL */)
{
  struct pic_var *var;

  var = (struct pic_var *)pic_obj_alloc(pic, sizeof(struct pic_var), PIC_TT_VAR);
  var->value = pic_undef_value();
  var->conv = conv;

  pic_var_set(pic, var, init);

  return var;
}

pic_value
pic_var_ref(pic_state *pic, struct pic_var *var)
{
  UNUSED(pic);
  return var->value;
}

void
pic_var_set(pic_state *pic, struct pic_var *var, pic_value value)
{
  if (var->conv) {
    value = pic_apply1(pic, var->conv, value);
  }
  pic_var_set_force(pic, var, value);
}

void
pic_var_set_force(pic_state *pic, struct pic_var *var, pic_value value)
{
  UNUSED(pic);
  var->value = value;
}

static struct pic_var *
get_var_from_proc(pic_state *pic, struct pic_proc *proc)
{
  pic_value v;

  if (! pic_proc_p(v)) {
    goto typeerror;
  }
  if (! pic_proc_func_p(pic_proc_ptr(v))) {
    goto typeerror;
  }
  if (pic_proc_cv_size(pic, proc) != 1) {
    goto typeerror;
  }
  v = pic_proc_cv_ref(pic, proc, 0);
  if (! pic_var_p(v)) {
    goto typeerror;
  }
  return pic_var_ptr(v);

 typeerror:
  pic_error(pic, "expected parameter");
  UNREACHABLE();
}

static pic_value
var_call(pic_state *pic)
{
  struct pic_proc *proc;
  struct pic_var *var;
  pic_value v;
  int c;

  proc = pic_get_proc(pic);

  c = pic_get_args(pic, "|o", &v);
  if (c == 0) {
    var = pic_var_ptr(proc->env->regs[0]);
    return pic_var_ref(pic, var);
  }
  else if (c == 1) {
    var = pic_var_ptr(proc->env->regs[0]);

    pic_var_set(pic, var, v);
    return pic_none_value();
  }
  else {
    pic_abort(pic, "logic flaw");
  }
  UNREACHABLE();
}

struct pic_proc *
pic_wrap_var(pic_state *pic, struct pic_var *var)
{
  struct pic_proc *proc;

  proc = pic_proc_new(pic, var_call, "<var-procedure>");
  pic_proc_cv_init(pic, proc, 1);
  pic_proc_cv_set(pic, proc, 0, pic_obj_value(var));
  return proc;
}

struct pic_var *
pic_unwrap_var(pic_state *pic, struct pic_proc *proc)
{
  return get_var_from_proc(pic, proc);
}

static pic_value
pic_var_make_parameter(pic_state *pic)
{
  struct pic_proc *conv = NULL;
  struct pic_var *var;
  pic_value init;

  pic_get_args(pic, "o|l", &init, &conv);

  var = pic_var_new(pic, init, conv);
  return pic_obj_value(pic_wrap_var(pic, var));
}

static pic_value
pic_var_parameter_ref(pic_state *pic)
{
  struct pic_proc *proc;
  struct pic_var *var;

  pic_get_args(pic, "l", &proc);

  var = get_var_from_proc(pic, proc);
  return pic_var_ref(pic, var);
}

static pic_value
pic_var_parameter_set(pic_state *pic)
{
  struct pic_proc *proc;
  struct pic_var *var;
  pic_value v;

  pic_get_args(pic, "lo", &proc, &v);

  var = get_var_from_proc(pic, proc);
  /* no convert */
  pic_var_set_force(pic, var, v);
  return pic_none_value();
}

static pic_value
pic_var_parameter_converter(pic_state *pic)
{
  struct pic_proc *proc;
  struct pic_var *var;

  pic_get_args(pic, "l", &proc);

  var = get_var_from_proc(pic, proc);
  if (var->conv) {
    return pic_obj_value(var->conv);
  }
  else {
    return pic_false_value();
  }
}

void
pic_init_var(pic_state *pic)
{
  pic_deflibrary ("(picrin parameter)") {
    pic_defun(pic, "make-parameter", pic_var_make_parameter);
    pic_defun(pic, "parameter-ref", pic_var_parameter_ref);
    pic_defun(pic, "parameter-set!", pic_var_parameter_set); /* no convert */
    pic_defun(pic, "parameter-converter", pic_var_parameter_converter);
  }
}
