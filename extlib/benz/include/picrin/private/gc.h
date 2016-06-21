/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_GC_H
#define PICRIN_GC_H

#if defined(__cplusplus)
extern "C" {
#endif

#if PIC_BITMAP_GC
# define OBJECT_HEADER                           \
  unsigned char tt;
#else
# define OBJECT_HEADER                           \
  unsigned char tt;                              \
  char gc_mark;
#endif

struct heap *pic_heap_open(pic_state *);
void pic_heap_close(pic_state *, struct heap *);

#if defined(__cplusplus)
}
#endif

#endif
