/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_BOX_H__
#define PICRIN_BOX_H__

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_box {
  PIC_OBJECT_HEADER
  pic_value value;
};

#define pic_box_p(v) (pic_type(v) == PIC_TT_BOX)
#define pic_box_ptr(v) ((struct pic_box *)pic_ptr(v))

pic_value pic_box(pic_state *, pic_value);
pic_value pic_unbox(pic_state *, pic_value);
void pic_set_box(pic_state *, pic_value, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
