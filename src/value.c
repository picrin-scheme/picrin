#include "picrin.h"

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
