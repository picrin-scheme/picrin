/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/dict.h"

static pic_value
var_lookup(pic_state *pic, pic_value var)
{
  pic_value val, env;
  struct pic_dict *binding;

  val = pic_ref(pic, pic->PICRIN_BASE, "current-dynamic-environment");
  if (pic_eq_p(val, var)) {
    return pic_false_value();
  }

  env = pic_apply0(pic, pic_proc_ptr(val));
  while (! pic_nil_p(env)) {
    pic_assert_type(pic, pic_car(pic, env), dict);

    binding = pic_dict_ptr(pic_car(pic, env));
    if (pic_dict_has(pic, binding, var)) {
      return pic_dict_ref(pic, binding, var);
    }
    env = pic_cdr(pic, env);
  }

  return pic_false_value();
}

static pic_value
var_call(pic_state *pic)
{
  struct pic_proc *self = pic_get_proc(pic);
  pic_value val, tmp, box, conv;
  size_t n;

  n = pic_get_args(pic, "|oo", &val, &tmp);

  box = var_lookup(pic, pic_obj_value(self));
  if (! pic_test(box)) {
    box = pic_attr_ref(pic, pic_obj_value(self), "@@box");
  }

  switch (n) {
  case 0:
    return pic_car(pic, box);

  case 1:
    conv = pic_attr_ref(pic, pic_obj_value(self), "@@converter");
    if (pic_test(conv)) {
      pic_assert_type(pic, conv, proc);

      val = pic_apply1(pic, pic_proc_ptr(conv), val);
    }
    pic_set_car(pic, box, val);

    return pic_none_value();

  case 2:
    assert(pic_false_p(tmp));

    conv = pic_attr_ref(pic, pic_obj_value(self), "@@converter");
    if (pic_test(conv)) {
      pic_assert_type(pic, conv, proc);

      return pic_apply1(pic, pic_proc_ptr(conv), val);
    } else {
      return val;
    }
  }
  UNREACHABLE();
}

struct pic_proc *
pic_make_var(pic_state *pic, pic_value init, struct pic_proc *conv)
{
  struct pic_proc *var;

  var = pic_make_proc(pic, var_call, "<var-call>");
  pic_attr_set(pic, pic_obj_value(var), "@@box", pic_list1(pic, init));
  pic_attr_set(pic, pic_obj_value(var), "@@converter", conv ? pic_obj_value(conv) : pic_false_value());

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

void
pic_init_var(pic_state *pic)
{
  pic_define_noexport(pic, "current-dynamic-environment", pic_false_value());

  pic_defun(pic, "make-parameter", pic_var_make_parameter);

  pic_set(pic, pic->PICRIN_BASE, "current-dynamic-environment", pic_obj_value(pic_make_var(pic, pic_nil_value(), NULL)));
}
