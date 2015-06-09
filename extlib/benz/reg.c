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
