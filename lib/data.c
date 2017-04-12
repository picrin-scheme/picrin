/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"

bool
pic_data_p(pic_state *pic, pic_value obj, const pic_data_type *type)
{
  if (pic_type(pic, obj) != PIC_TYPE_DATA) {
    return false;
  }
  return type == NULL || data_ptr(pic, obj)->type == type;
}

void *
pic_data(pic_state *PIC_UNUSED(pic), pic_value data)
{
  return data_ptr(pic, data)->data;
}

pic_value
pic_data_value(pic_state *pic, void *userdata, const pic_data_type *type)
{
  struct data *data;

  data = (struct data *)pic_obj_alloc(pic, PIC_TYPE_DATA);
  data->type = type;
  data->data = userdata;

  return obj_value(pic, data);
}
