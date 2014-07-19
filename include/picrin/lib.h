/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_LIB_H__
#define PICRIN_LIB_H__

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_lib {
  PIC_OBJECT_HEADER
  pic_value name;
  struct pic_senv *senv;
  struct pic_dict *exports;
};

#define pic_lib_ptr(o) ((struct pic_lib *)pic_ptr(o))

#if defined(__cplusplus)
}
#endif

#endif
