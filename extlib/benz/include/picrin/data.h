/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_DATA_H
#define PICRIN_DATA_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_data {
  PIC_OBJECT_HEADER
  const pic_data_type *type;
  void *data;
};

#define pic_data_ptr(o) ((struct pic_data *)pic_obj_ptr(o))

PIC_INLINE bool pic_data_type_p(pic_state *pic, const pic_value obj, const pic_data_type *type) {
  return pic_data_p(pic, obj) && pic_data_ptr(obj)->type == type;
}

#if defined(__cplusplus)
}
#endif

#endif
