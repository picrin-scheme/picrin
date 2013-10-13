#ifndef GC_H__
#define GC_H__

struct header {
  struct header *ptr;
  size_t size;
};

struct heap_page {
  struct header *base, *freep;
  size_t heap_size;
};

void init_heap_page(struct heap_page *);

#endif
