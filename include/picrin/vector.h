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
  size_t len;
};

#define pic_vec_p(v) (pic_type(v) == PIC_TT_VECTOR)
#define pic_vec_ptr(o) ((struct pic_vector *)pic_ptr(o))

struct pic_vector *pic_make_vec(pic_state *, size_t);
struct pic_vector *pic_make_vec_from_list(pic_state *, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
