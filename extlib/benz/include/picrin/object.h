/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_OBJECT_H
#define PICRIN_OBJECT_H

#if defined(__cplusplus)
extern "C" {
#endif


/* vector */

struct pic_vector {
  PIC_OBJECT_HEADER
  pic_value *data;
  int len;
};

#define pic_vec_ptr(o) ((struct pic_vector *)pic_obj_ptr(o))


/* weak */

KHASH_DECLARE(weak, void *, pic_value)

struct pic_weak {
  PIC_OBJECT_HEADER
  khash_t(weak) hash;
  struct pic_weak *prev;         /* for GC */
};

#define pic_weak_ptr(v) ((struct pic_weak *)pic_obj_ptr(v))


#if defined(__cplusplus)
}
#endif

#endif
