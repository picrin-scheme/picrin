#ifndef GC_H__
#define GC_H__

union header {
  struct {
    union header *ptr;
    size_t size;
  } s;
  long alignment;   /* force alignment for headers to be allocated at the size of long */
};

struct heap_page {
  union header *base, *freep;
  size_t heap_size;
};

void init_heap_page(struct heap_page *);

#endif
