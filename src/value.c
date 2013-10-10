#include "picrin.h"

enum pic_tt
pic_type(pic_value v)
{
  switch (v->type) {
  case PIC_VTYPE_NIL:
    return PIC_TT_NIL;
  case PIC_VTYPE_HEAP:
    return v->u->data->tt;
  }
}

pic_value
pic_nil_value()
{
  pic_value v;

  v.vtype = PIC_VTYPE_NIL;
  v.u.data = NULL;
  return v;
}

pic_value
pic_obj_value(struct pic_object *obj)
{
  pic_value v;

  v.vtype = PIC_VTYPE_HEAP;
  v.u.data = obj;
  return v;
}
