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
#define pic_box_ptr(o) ((struct pic_box *)pic_ptr(o))

PIC_INLINE pic_value
pic_box(pic_state *pic, pic_value value)
{
  struct pic_box *box;

  box = (struct pic_box *)pic_obj_alloc(pic, sizeof(struct pic_box), PIC_TT_BOX);
  box->value = value;

  return pic_obj_value(box);
}

PIC_INLINE pic_value
pic_unbox(pic_state *pic, pic_value box)
{
  if (! pic_box_p(box)) {
    pic_errorf(pic, "box required");
  }
  return pic_box_ptr(box)->value;
}

PIC_INLINE void
pic_set_box(pic_state *pic, pic_value box, pic_value value)
{
  if (! pic_box_p(box)) {
    pic_errorf(pic, "box required");
  }
  pic_box_ptr(box)->value = value;
}

#if defined(__cplusplus)
}
#endif

#endif
