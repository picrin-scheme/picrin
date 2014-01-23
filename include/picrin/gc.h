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

union header {
  struct {
    union header *ptr;
    size_t size;
    unsigned int mark : 1;
  } s;
  long alignment[4];
};

struct heap_page {
  union header *basep, *endp;
  struct heap_page *next;
};

struct pic_heap {
  union header base, *freep;
  struct heap_page *pages;
};

void init_heap(struct pic_heap *);
void finalize_heap(struct pic_heap *);

#if defined(__cplusplus)
}
#endif

#endif
