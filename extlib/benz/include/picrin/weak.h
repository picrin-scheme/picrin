/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_WEAK_H
#define PICRIN_WEAK_H

#if defined(__cplusplus)
extern "C" {
#endif

KHASH_DECLARE(weak, void *, pic_value)

struct pic_weak {
  PIC_OBJECT_HEADER
  khash_t(weak) hash;
  struct pic_weak *prev;         /* for GC */
};

#define pic_weak_ptr(v) ((struct pic_weak *)pic_ptr(v))

#if defined(__cplusplus)
}
#endif

#endif
