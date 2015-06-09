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
  pic_sym *id;
  struct pic_dict *dict;

  id = pic_sym_ptr(pic_proc_env_ref(pic, var, "id"));

  pic_for_each (elem, pic->ptable, it) {
    dict = pic_dict_ptr(elem);
    if (pic_dict_has(pic, dict, id)) {
      return pic_dict_ref(pic, dict, id);
    }
  }
  pic_panic(pic, "logic flaw");
}

static pic_value
var_set(pic_state *pic, struct pic_proc *var, pic_value val)
{
  pic_sym *id;
  struct pic_dict *dict;

  id = pic_sym_ptr(pic_proc_env_ref(pic, var, "id"));

  dict = pic_dict_ptr(pic_car(pic, pic->ptable));

  pic_dict_set(pic, dict, id, val);

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
  pic_value converter = conv ? pic_obj_value(conv) : pic_false_value();
  pic_sym *id;

  var = pic_make_proc(pic, var_call, "<var-call>");

  if (conv != NULL) {
    pic_proc_env_set(pic, var, "conv", converter);
  }
  id = pic_intern(pic, pic_format(pic, "%d", pic->pnum++));
  pic_proc_env_set(pic, var, "id", pic_obj_value(id));

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

  pic->ptable = pic_cons(pic, pic_obj_value(pic_make_dict(pic)), pic->ptable);

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
