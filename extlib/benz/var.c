/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

static pic_value
var_conv(pic_state *pic, struct pic_proc *var, pic_value val)
{
  if (pic_proc_env_has(pic, var, "conv") != 0) {
    return pic_apply1(pic, pic_proc_ptr(pic_proc_env_ref(pic, var, "conv")), val);
  }
  return val;
}

static pic_value
var_get(pic_state *pic, struct pic_proc *var)
{
  pic_value elem, it;
  struct pic_reg *reg;

  pic_for_each (elem, pic->ptable, it) {
    reg = pic_reg_ptr(elem);
    if (pic_reg_has(pic, reg, var)) {
      return pic_reg_ref(pic, reg, var);
    }
  }
  pic_panic(pic, "logic flaw");
}

static pic_value
var_set(pic_state *pic, struct pic_proc *var, pic_value val)
{
  struct pic_reg *reg;

  reg = pic_reg_ptr(pic_car(pic, pic->ptable));

  pic_reg_set(pic, reg, var, val);

  return pic_undef_value();
}

static pic_value
var_call(pic_state *pic)
{
  struct pic_proc *self = pic_get_proc(pic);
  pic_value val;
  int n;

  n = pic_get_args(pic, "|o", &val);

  if (n == 0) {
    return var_get(pic, self);
  } else {
    return var_set(pic, self, var_conv(pic, self, val));
  }
}

struct pic_proc *
pic_make_var(pic_state *pic, pic_value init, struct pic_proc *conv)
{
  struct pic_proc *var;

  var = pic_make_proc(pic, var_call);

  if (conv != NULL) {
    pic_proc_env_set(pic, var, "conv", pic_obj_value(conv));
  }

  pic_apply1(pic, var, init);

  return var;
}

static pic_value
pic_var_make_parameter(pic_state *pic)
{
  struct pic_proc *conv = NULL;
  pic_value init;

  pic_get_args(pic, "o|l", &init, &conv);

  return pic_obj_value(pic_make_var(pic, init, conv));
}

static pic_value
pic_var_with_parameter(pic_state *pic)
{
  struct pic_proc *body;
  pic_value val;

  pic_get_args(pic, "l", &body);

  pic->ptable = pic_cons(pic, pic_obj_value(pic_make_reg(pic)), pic->ptable);

  val = pic_apply0(pic, body);

  pic->ptable = pic_cdr(pic, pic->ptable);

  return val;
}

void
pic_init_var(pic_state *pic)
{
  pic_defun(pic, "make-parameter", pic_var_make_parameter);
  pic_defun(pic, "with-parameter", pic_var_with_parameter);
}
