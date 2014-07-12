/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/proc.h"
#include "picrin/var.h"

static pic_value
var_ref(pic_state *pic, struct pic_var *var)
{
  UNUSED(pic);
  return var->value;
}

static void
var_set(pic_state *pic, struct pic_var *var, pic_value value)
{
  UNUSED(pic);
  var->value = value;
}

struct pic_var *
pic_var_new(pic_state *pic, pic_value init)
{
  struct pic_var *var;

  var = (struct pic_var *)pic_obj_alloc(pic, sizeof(struct pic_var), PIC_TT_VAR);
  var->value = init;

  return var;
}

pic_value
pic_var_ref(pic_state *pic, const char *name)
{
  pic_value v;
  struct pic_var *var;

  v = pic_ref(pic, name);

  pic_assert_type(pic, v, var);

  var = pic_var_ptr(v);

  return var_ref(pic, var);
}

void
pic_var_set(pic_state *pic, const char *name, pic_value value)
{
  pic_value v;
  struct pic_var *var;

  v = pic_ref(pic, name);

  pic_assert_type(pic, v, var);

  var = pic_var_ptr(v);

  var_set(pic, var, value);
}

static pic_value
pic_var_make_var(pic_state *pic)
{
  pic_value init;

  pic_get_args(pic, "o", &init);

  return pic_obj_value(pic_var_new(pic, init));
}

static pic_value
pic_var_var_ref(pic_state *pic)
{
  struct pic_var *var;
  pic_value v;

  pic_get_args(pic, "o", &v);

  pic_assert_type(pic, v, var);

  var = pic_var_ptr(v);

  return var_ref(pic, var);
}

static pic_value
pic_var_var_set(pic_state *pic)
{
  struct pic_var *var;
  pic_value v, val;

  pic_get_args(pic, "oo", &v, &val);

  pic_assert_type(pic, v, var);

  var = pic_var_ptr(v);
  var_set(pic, var, val);

  return pic_none_value();
}

void
pic_init_var(pic_state *pic)
{
  pic_deflibrary ("(picrin var)") {
    pic_defun(pic, "make-var", pic_var_make_var);
    pic_defun(pic, "var-ref", pic_var_var_ref);
    pic_defun(pic, "var-set!", pic_var_var_set);
  }
}
