#include "picrin.h"
#include "picrin/box.h"

pic_value
pic_box(pic_state *pic, pic_value value)
{
  struct pic_box *box;

  box = (struct pic_box *)pic_obj_alloc(pic, sizeof(struct pic_box), PIC_TT_BOX);
  box->value = value;
  return pic_obj_value(box);
}

pic_value
pic_unbox(pic_state *pic, pic_value box)
{
  if (! pic_box_p(box)) {
    pic_errorf(pic, "expected box, but got ~s", box);
  }
  return pic_box_ptr(box)->value;
}

void
pic_set_box(pic_state *pic, pic_value box, pic_value value)
{
  if (! pic_box_p(box)) {
    pic_errorf(pic, "expected box, but got ~s", box);
  }
  pic_box_ptr(box)->value = value;
}
