#ifndef GC_H__
#define GC_H__

enum pic_gc_mark {
  PIC_GC_MARK,
  PIC_GC_UNMARK
};

union header {
  struct {
    union header *ptr;
    size_t size;
    enum pic_gc_mark mark;
  } s;
  long alignment;   /* force alignment for headers to be allocated at the size of long */
};

struct heap_page {
  union header *base, *freep;
  size_t heap_size;
};

void init_heap_page(struct heap_page *);

#endif
