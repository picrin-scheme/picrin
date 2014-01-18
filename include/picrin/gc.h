/**
 * See Copyright Notice in picrin.h
 */

#ifndef GC_H__
#define GC_H__

#if defined(__cplusplus)
extern "C" {
#endif

enum pic_gc_mark {
  PIC_GC_UNMARK = 0,
  PIC_GC_MARK
};

union header {
  struct {
    union header *ptr;
    size_t size;
    enum pic_gc_mark mark : 1;
  } s;
  long alignment[2];
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
