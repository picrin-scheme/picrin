/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_VECTOR_H
#define PICRIN_VECTOR_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_vector {
  PIC_OBJECT_HEADER
  pic_value *data;
  int len;
};

#define pic_vec_ptr(o) ((struct pic_vector *)pic_obj_ptr(o))

#if defined(__cplusplus)
}
#endif

#endif
