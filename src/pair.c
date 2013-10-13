#include "picrin.h"

pic_value
pic_cons(pic_state *pic, pic_value car, pic_value cdr)
{
  struct pic_pair *pair;

  pair = (struct pic_pair *)pic_obj_alloc(pic, sizeof(struct pic_pair), PIC_TT_PAIR);
  pair->car = car;
  pair->cdr = cdr;

  return pic_obj_value(pair);
}

pic_value
pic_car(pic_state *pic, pic_value obj)
{
  struct pic_pair *pair;

  pair = (struct pic_pair *)obj.u.data;

  return pair->car;
}

pic_value
pic_cdr(pic_state *pic, pic_value obj)
{
  struct pic_pair *pair;

  pair = (struct pic_pair *)obj.u.data;

  return pair->cdr;
}
