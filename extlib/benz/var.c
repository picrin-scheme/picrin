/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

static pic_value
var_get(pic_state *pic, struct pic_proc *var)
{
  pic_value elem, it;
  struct pic_weak *weak;

  pic_for_each (elem, pic->ptable, it) {
    weak = pic_weak_ptr(elem);
    if (pic_weak_has(pic, weak, var)) {
      return pic_weak_ref(pic, weak, var);
    }
  }
  pic_panic(pic, "logic flaw");
}

static pic_value
var_set(pic_state *pic, struct pic_proc *var, pic_value val)
{
  struct pic_weak *weak;

  weak = pic_weak_ptr(pic_car(pic, pic->ptable));

  pic_weak_set(pic, weak, var, val);

  return pic_undef_value(pic);
}

static pic_value
var_call(pic_state *pic)
{
  struct pic_proc *self;
  pic_value val;
  int n;

  n = pic_get_args(pic, "&|o", &self, &val);

  if (n == 0) {
    return var_get(pic, self);
  } else {
    pic_value conv;

    conv = pic_closure_ref(pic, 0);
    if (! pic_false_p(pic, conv)) {
      val = pic_call(pic, pic_proc_ptr(conv), 1, val);
    }
    return var_set(pic, self, val);
  }
}

struct pic_proc *
pic_make_var(pic_state *pic, pic_value init, struct pic_proc *conv)
{
  struct pic_proc *var;
  pic_value c = pic_false_value(pic);

  if (conv != NULL) {
    c = pic_obj_value(conv);
  }
  var = pic_lambda(pic, var_call, 1, c);

  pic_call(pic, var, 1, init);

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

  pic->ptable = pic_cons(pic, pic_obj_value(pic_make_weak(pic)), pic->ptable);

  val = pic_call(pic, body, 0);

  pic->ptable = pic_cdr(pic, pic->ptable);

  return val;
}

void
pic_init_var(pic_state *pic)
{
  pic_defun(pic, "make-parameter", pic_var_make_parameter);
  pic_defun(pic, "with-parameter", pic_var_with_parameter);
}
