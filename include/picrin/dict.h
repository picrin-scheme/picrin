/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_DICT_H__
#define PICRIN_DICT_H__

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_dict {
  PIC_OBJECT_HEADER
  xhash hash;
};

#define pic_dict_p(v) (pic_type(v) == PIC_TT_DICT)
#define pic_dict_ptr(v) ((struct pic_dict *)pic_ptr(v))

#if defined(__cplusplus)
}
#endif

#endif
