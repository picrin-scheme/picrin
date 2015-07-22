#include "picrin.h"

struct pic_data *
pic_data_alloc(pic_state *pic, const pic_data_type *type, void *userdata)
{
  struct pic_data *data;
  struct pic_dict *storage = pic_make_dict(pic);

  data = (struct pic_data *)pic_obj_alloc(pic, sizeof(struct pic_data), PIC_TT_DATA);
  data->type = type;
  data->data = userdata;
  data->storage = storage;

  return data;
}
