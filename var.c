/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/var.h"
#include "picrin/pair.h"

struct pic_var *
pic_var_new(pic_state *pic, pic_value init, struct pic_proc *conv)
{
  struct pic_var *var;

  var = (struct pic_var *)pic_obj_alloc(pic, sizeof(struct pic_var), PIC_TT_VAR);
  var->stack = pic_nil_value();
  var->conv = conv;

  pic_var_push(pic, var, init);

  return var;
}

pic_value
pic_var_ref(pic_state *pic, struct pic_var *var)
{
  return pic_car(pic, var->stack);
}

void
pic_var_set(pic_state *pic, struct pic_var *var, pic_value value)
{
  if (var->conv != NULL) {
    value = pic_apply1(pic, var->conv, value);
  }
  pic_set_car(pic, var->stack, value);
}

void
pic_var_push(pic_state *pic, struct pic_var *var, pic_value value)
{
  if (var->conv != NULL) {
    value = pic_apply1(pic, var->conv, value);
  }
  var->stack = pic_cons(pic, value, var->stack);
}

void
pic_var_pop(pic_state *pic, struct pic_var *var)
{
  var->stack = pic_cdr(pic, var->stack);
}

static pic_value
pic_var_make_parameter(pic_state *pic)
{
  struct pic_proc *conv = NULL;
  pic_value init;

  pic_get_args(pic, "o|l", &init, &conv);

  return pic_obj_value(pic_var_new(pic, init, conv));
}

static pic_value
pic_var_parameter_ref(pic_state *pic)
{
  struct pic_var *var;
  pic_value v;

  pic_get_args(pic, "o", &v);

  pic_assert_type(pic, v, var);

  var = pic_var_ptr(v);

  return pic_var_ref(pic, var);
}

static pic_value
pic_var_parameter_set(pic_state *pic)
{
  struct pic_var *var;
  pic_value v, val;

  pic_get_args(pic, "oo", &v, &val);

  pic_assert_type(pic, v, var);

  var = pic_var_ptr(v);
  pic_var_set(pic, var, val);
  return pic_none_value();
}

static pic_value
pic_var_parameter_push(pic_state *pic)
{
  struct pic_var *var;
  pic_value v, val;

  pic_get_args(pic, "oo", &v, &val);

  pic_assert_type(pic, v, var);

  var = pic_var_ptr(v);
  pic_var_push(pic, var, val);
  return pic_none_value();
}

static pic_value
pic_var_parameter_pop(pic_state *pic)
{
  struct pic_var *var;
  pic_value v;

  pic_get_args(pic, "o", &v);

  pic_assert_type(pic, v, var);

  var = pic_var_ptr(v);
  pic_var_pop(pic, var);
  return pic_none_value();
}

void
pic_init_var(pic_state *pic)
{
  pic_defun(pic, "make-parameter", pic_var_make_parameter);
  pic_defun(pic, "parameter-ref", pic_var_parameter_ref);
  pic_defun(pic, "parameter-set!", pic_var_parameter_set);
  pic_defun(pic, "parameter-push!", pic_var_parameter_push);
  pic_defun(pic, "parameter-pop!", pic_var_parameter_pop);
}
