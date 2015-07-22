/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

KHASH_DEFINE(reg, void *, pic_value, kh_ptr_hash_func, kh_ptr_hash_equal)

struct pic_reg *
pic_make_reg(pic_state *pic)
{
  struct pic_reg *reg;

  reg = (struct pic_reg *)pic_obj_alloc(pic, sizeof(struct pic_reg), PIC_TT_REG);
  reg->prev = NULL;
  kh_init(reg, &reg->hash);

  return reg;
}

pic_value
pic_reg_ref(pic_state *pic, struct pic_reg *reg, void *key)
{
  khash_t(reg) *h = &reg->hash;
  khiter_t it;

  it = kh_get(reg, h, key);
  if (it == kh_end(h)) {
    pic_errorf(pic, "element not found for a key: ~s", pic_obj_value(key));
  }
  return kh_val(h, it);
}

void
pic_reg_set(pic_state PIC_UNUSED(*pic), struct pic_reg *reg, void *key, pic_value val)
{
  khash_t(reg) *h = &reg->hash;
  int ret;
  khiter_t it;

  it = kh_put(reg, h, key, &ret);
  kh_val(h, it) = val;
}

bool
pic_reg_has(pic_state PIC_UNUSED(*pic), struct pic_reg *reg, void *key)
{
  return kh_get(reg, &reg->hash, key) != kh_end(&reg->hash);
}

void
pic_reg_del(pic_state *pic, struct pic_reg *reg, void *key)
{
  khash_t(reg) *h = &reg->hash;
  khiter_t it;

  it = kh_get(reg, h, key);
  if (it == kh_end(h)) {
    pic_errorf(pic, "no slot named ~s found in register", pic_obj_value(key));
  }
  kh_del(reg, h, it);
}


static pic_value
reg_get(pic_state *pic, struct pic_reg *reg, void *key)
{
  if (! pic_reg_has(pic, reg, key)) {
    return pic_false_value();
  }
  return pic_cons(pic, pic_obj_value(key), pic_reg_ref(pic, reg, key));
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
    pic_errorf(pic, "attempted to set a non-object key '~s' in a register", key);
  }

  reg = pic_reg_ptr(pic_proc_env_ref(pic, self, "reg"));

  if (n == 1) {
    return reg_get(pic, reg, pic_obj_ptr(key));
  } else {
    return reg_set(pic, reg, pic_obj_ptr(key), val);
  }
}

static pic_value
pic_reg_make_register(pic_state *pic)
{
  struct pic_reg *reg;
  struct pic_proc *proc;

  pic_get_args(pic, "");

  reg = pic_make_reg(pic);

  proc = pic_make_proc(pic, reg_call);

  pic_proc_env_set(pic, proc, "reg", pic_obj_value(reg));

  return pic_obj_value(proc);
}

void
pic_init_reg(pic_state *pic)
{
  pic_defun(pic, "make-register", pic_reg_make_register);
}
