#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"

void
init_heap_page(struct heap_page *heap)
{
  union header *base, *freep;
  void *p;

  p = (union header *)malloc(PIC_HEAP_SIZE);

  heap->base = base = (union header *)
    (((unsigned long)p + sizeof(union header) -1) & ~(sizeof(union header) - 1));
  base->s.ptr = base + 1;
  base->s.size = 0;

  heap->freep = freep = base->s.ptr;
  freep->s.ptr = base;
  freep->s.size = ((char *)p + PIC_HEAP_SIZE - (char *)freep) / sizeof(union header);
}

void *
pic_alloc(pic_state *pic, size_t size)
{
  union header *freep, *p, *prevp;
  size_t nunits;

  nunits = (size + sizeof(union header) - 1) / sizeof(union header) + 1;

  freep = pic->heap->freep;
  prevp = freep;
  for (p = prevp->s.ptr; ; prevp = p, p = p->s.ptr) {
    if (p->s.size >= nunits)
      break;
    if (p == freep) {
      return 0;
    }
  }
  if (p->s.size == nunits) {
    prevp->s.ptr = p->s.ptr;
  }
  else {
    p->s.size -= nunits;
    p += p->s.size;
    p->s.size = nunits;
  }
  pic->heap->freep = prevp;

  return (void *)(p + 1);
}

void
pic_free(pic_state *pic, void *ptr)
{
  union header *bp, *p;

  bp = (union header *)ptr - 1;

  for (p = pic->heap->freep; !(p < bp && bp < p->s.ptr); p = p->s.ptr) {
    if (p >= p->s.ptr && (bp > p || bp < p->s.ptr))
	break;
  }
  if (bp + bp->s.size == p->s.ptr) {
    bp->s.size += p->s.ptr->s.size;
    bp->s.ptr = p->s.ptr->s.ptr;
  }
  else {
    bp->s.ptr = p->s.ptr;
  }
  if (p + p->s.size == bp) {
    p->s.size += bp->s.size;
    p->s.ptr = bp->s.ptr;
  } else {
    p->s.ptr  =  bp;
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

