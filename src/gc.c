#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"

void
init_heap_page(struct heap_page *heap)
{
  struct header *base, *freep;
  void *p;

  p = (struct header *)malloc(PIC_HEAP_SIZE);

  heap->base = base = (struct header *)
    (((unsigned long)p + sizeof(struct header) -1) & ~(sizeof(struct header) - 1));
  base->ptr = base + 1;
  base->size = 0;

  heap->freep = freep = base->ptr;
  freep->ptr = base;
  freep->size = ((char *)p + PIC_HEAP_SIZE - (char *)freep) / sizeof(struct header);
}

void *
pic_alloc(pic_state *pic, size_t size)
{
  struct header *freep, *p, *prevp;
  size_t nunits;

  nunits = (size + sizeof(struct header) - 1) / sizeof(struct header) + 1;

  freep = pic->heap->freep;
  prevp = freep;
  for (p = prevp->ptr; ; prevp = p, p = p->ptr) {
    if (p->size >= nunits)
      break;
    if (p == freep)
      return 0;
  }
  if (p->size == nunits) {
    prevp->ptr = p->ptr;
  }
  else {
    p->size -= nunits;
    p += p->size;
    p->size = nunits;
  }
  pic->heap->freep = prevp;

  return (void *)(p + 1);
}

void
pic_free(pic_state *pic, void *ptr)
{
  struct header *bp, *p;

  bp = (struct header *)ptr - 1;

  for (p = pic->heap->freep; !(p < bp && bp < p->ptr); p = p->ptr) {
    if (p >= p->ptr && (bp > p || bp < p->ptr))
	break;
  }
  if (bp + bp->size == p->ptr) {
    bp->size += p->ptr->size;
    bp->ptr = p->ptr->ptr;
  }
  else {
    bp->ptr = p->ptr;
  }
  if (p + p->size == bp) {
    p->size += bp->size;
    p->ptr = bp->ptr;
  } else {
    p->ptr  =  bp;
  }
  pic->heap->freep = p;
}

struct pic_object *
pic_gc_alloc(pic_state *pic, size_t size, enum pic_tt tt)
{
  struct pic_object *obj;

  obj = (struct pic_object *)malloc(size);
  obj->tt = tt;

  return obj;
}

