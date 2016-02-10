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

#define pic_weak_p(v) (pic_type(v) == PIC_TT_WEAK)
#define pic_weak_ptr(v) ((struct pic_weak *)pic_ptr(v))

struct pic_weak *pic_make_weak(pic_state *);

pic_value pic_weak_ref(pic_state *, struct pic_weak *, void *);
void pic_weak_set(pic_state *, struct pic_weak *, void *, pic_value);
void pic_weak_del(pic_state *, struct pic_weak *, void *);
bool pic_weak_has(pic_state *, struct pic_weak *, void *);

#if defined(__cplusplus)
}
#endif

#endif
