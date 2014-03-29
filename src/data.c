#include "picrin.h"
#include "picrin/data.h"

struct pic_data *
pic_data_alloc(pic_state *pic, const pic_data_type *type, void *userdata)
{
  struct pic_data *data;

  data = (struct pic_data *)pic_obj_alloc(pic, sizeof(struct pic_data), PIC_TT_DATA);
  data->type = type;
  data->data = userdata;
  xh_init_str(&data->storage, sizeof(pic_value));

  return data;
}
