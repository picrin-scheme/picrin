/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_GC_H__
#define PICRIN_GC_H__

#if defined(__cplusplus)
extern "C" {
#endif

#define PIC_GC_UNMARK 0
#define PIC_GC_MARK 1

struct pic_heap;

struct pic_heap *pic_heap_open();
void pic_heap_close(struct pic_heap *);

#if defined(__cplusplus)
}
#endif

#endif
