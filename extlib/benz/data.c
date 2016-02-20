#include "picrin.h"
#include "picrin/private/object.h"

bool
pic_data_p(pic_state *pic, pic_value obj, const pic_data_type *type)
{
  if (pic_type(pic, obj) != PIC_TYPE_DATA) {
    return false;
  }
  return type == NULL || pic_data_ptr(pic, obj)->type == type;
}

void *
pic_data(pic_state PIC_UNUSED(*pic), pic_value data)
{
  return pic_data_ptr(pic, data)->data;
}

pic_value
pic_data_value(pic_state *pic, void *userdata, const pic_data_type *type)
{
  struct pic_data *data;

  data = (struct pic_data *)pic_obj_alloc(pic, sizeof(struct pic_data), PIC_TYPE_DATA);
  data->type = type;
  data->data = userdata;

  return pic_obj_value(data);
}
