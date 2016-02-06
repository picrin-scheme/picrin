/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_BOX_H
#define PICRIN_BOX_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_box {
  PIC_OBJECT_HEADER
  pic_value value;
};

#define pic_box_p(v) (pic_type(v) == PIC_TT_BOX)
#define pic_box_ptr(v) ((struct pic_box *)pic_ptr(v))

PIC_INLINE struct pic_box *
pic_box(pic_state *pic, pic_value value)
{
  struct pic_box *box;

  box = (struct pic_box *)pic_obj_alloc(pic, sizeof(struct pic_box), PIC_TT_BOX);
  box->value = value;
  return box;
}

#if defined(__cplusplus)
}
#endif

#endif
