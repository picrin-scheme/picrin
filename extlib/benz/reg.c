/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

struct pic_reg *
pic_make_reg(pic_state *pic)
{
  struct pic_reg *reg;

  reg = (struct pic_reg *)pic_obj_alloc(pic, sizeof(struct pic_reg), PIC_TT_REG);
  reg->prev = NULL;
  xh_init_ptr(&reg->hash, sizeof(pic_value));

  return reg;
}

pic_value
pic_reg_ref(pic_state *pic, struct pic_reg *reg, void *key)
{
  xh_entry *e;

  e = xh_get_ptr(&reg->hash, key);
  if (! e) {
    pic_errorf(pic, "element not found for a key: ~s", pic_obj_value(key));
  }
  return xh_val(e, pic_value);
}

void
pic_reg_set(pic_state PIC_UNUSED(*pic), struct pic_reg *reg, void *key, pic_value val)
{
  xh_put_ptr(&reg->hash, key, &val);
}

bool
pic_reg_has(pic_state PIC_UNUSED(*pic), struct pic_reg *reg, void *key)
{
  return xh_get_ptr(&reg->hash, key) != NULL;
}

void
pic_reg_del(pic_state *pic, struct pic_reg *reg, void *key)
{
  if (xh_get_ptr(&reg->hash, key) == NULL) {
    pic_errorf(pic, "no slot named ~s found in registry", pic_obj_value(key));
  }

  xh_del_ptr(&reg->hash, key);
}


static pic_value
reg_get(pic_state *pic, struct pic_reg *reg, void *key)
{
  if (! pic_reg_has(pic, reg, key)) {
    return pic_undef_value();
  }
  return pic_reg_ref(pic, reg, key);
}

static pic_value
reg_set(pic_state *pic, struct pic_reg *reg, void *key, pic_value val)
{
  if (pic_undef_p(val)) {
    if (pic_reg_has(pic, reg, key)) {
      pic_reg_del(pic, reg, key);
    }
  } else {
    pic_reg_set(pic, reg, key, val);
  }

  return pic_undef_value();
}

static pic_value
reg_call(pic_state *pic)
{
  struct pic_proc *self = pic_get_proc(pic);
  struct pic_reg *reg;
  pic_value key, val;
  int n;

  n = pic_get_args(pic, "o|o", &key, &val);

  if (! pic_obj_p(key)) {
    pic_errorf(pic, "attempted to set a non-object key '~s' in a registory", key);
  }

  reg = pic_reg_ptr(pic_proc_env_ref(pic, self, "reg"));

  if (n == 1) {
    return reg_get(pic, reg, pic_obj_ptr(key));
  } else {
    return reg_set(pic, reg, pic_obj_ptr(key), val);
  }
}

static pic_value
pic_reg_make_registry(pic_state *pic)
{
  struct pic_reg *reg;
  struct pic_proc *proc;

  pic_get_args(pic, "");

  reg = pic_make_reg(pic);

  proc = pic_make_proc(pic, reg_call, "<reg-call>");

  pic_proc_env_set(pic, proc, "reg", pic_obj_value(reg));

  return pic_obj_value(proc);
}

void
pic_init_reg(pic_state *pic)
{
  pic_defun(pic, "make-registry", pic_reg_make_registry);
}
