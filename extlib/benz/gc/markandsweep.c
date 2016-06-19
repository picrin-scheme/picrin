/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/private/object.h"
#include "picrin/private/state.h"

enum {
  WHITE = 0,
  BLACK = 1
};

#define PAGE_UNITS ((PIC_HEAP_PAGE_SIZE - offsetof(struct heap_page, basep)) / sizeof(union header))

struct heap_page {
  struct heap_page *next;
  union header basep[1];
};

static void *
heap_alloc(pic_state *pic, size_t size)
{
  union header *p, *prevp;
  size_t nunits;

  assert(size > 0);

  nunits = (size + sizeof(union header) - 1) / sizeof(union header) + 1;

  prevp = pic->heap->freep;
  for (p = prevp->s.ptr; ; prevp = p, p = p->s.ptr) {
    if (p->s.size >= nunits)
      break;
    if (p == pic->heap->freep) {
      return NULL;
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

static void
heap_free(pic_state *pic, void *ap)
{
  union header *bp, *p;

  assert(ap != NULL);

  bp = (union header *)ap - 1;

  for (p = pic->heap->freep; ! (bp > p && bp < p->s.ptr); p = p->s.ptr) {
    if (p >= p->s.ptr && (bp > p || bp < p->s.ptr)) {
      break;
    }
  }
  if (bp + bp->s.size == p->s.ptr && p->s.ptr->s.size > 0) { /* don't melt base header */
    bp->s.size += p->s.ptr->s.size;
    bp->s.ptr = p->s.ptr->s.ptr;
  } else {
    bp->s.ptr = p->s.ptr;
  }
  if (p + p->s.size == bp && bp->s.size > 0) { /* don't melt base header */
    p->s.size += bp->s.size;
    p->s.ptr = bp->s.ptr;
  } else {
    p->s.ptr = bp;
  }
  pic->heap->freep = p;
}

static void
heap_morecore(pic_state *pic)
{
  union header *bp, *np;
  struct heap_page *page;

  assert(PAGE_UNITS >= 2);

  page = pic_malloc(pic, PIC_HEAP_PAGE_SIZE);
  page->next = pic->heap->pages;

  bp = page->basep;
  bp->s.size = 0;      /* bp is never used for allocation */
  heap_free(pic, bp + 1);

  np = page->basep + 1;
  np->s.size = PAGE_UNITS - 1;
  heap_free(pic, np + 1);

  pic->heap->pages = page;
}

static bool
is_marked(pic_state *PIC_UNUSED(pic), struct object *obj)
{
  return obj->u.basic.gc_mark == BLACK;
}

static void
mark(pic_state *PIC_UNUSED(pic), struct object *obj)
{
  obj->u.basic.gc_mark = BLACK;
}

static size_t
gc_sweep_page(pic_state *pic, struct heap_page *page)
{
  union header *bp, *p, *head = NULL, *tail = NULL;
  struct object *obj;
  size_t alive = 0;

  for (bp = page->basep; ; bp = bp->s.ptr) {
    p = bp + (bp->s.size ? bp->s.size : 1); /* first bp's size is 0, so force advnce */
    for (; p != bp->s.ptr; p += p->s.size) {
      if (p < page->basep || page->basep + PAGE_UNITS <= p) {
        goto escape;
      }
      obj = (struct object *)(p + 1);
      if (obj->u.basic.gc_mark == BLACK) {
        obj->u.basic.gc_mark = WHITE;
        alive += p->s.size;
      } else {
        if (head == NULL) {
          head = p;
        }
        if (tail != NULL) {
          tail->s.ptr = p;
        }
        tail = p;
        tail->s.ptr = NULL; /* We can safely reuse ptr field of dead object */
      }
    }
  }
 escape:

  /* free! */
  while (head != NULL) {
    p = head;
    head = head->s.ptr;
    gc_finalize_object(pic, (struct object *)(p + 1));
    heap_free(pic, p + 1);
  }

  return alive;
}

static void
gc_init(pic_state *PIC_UNUSED(pic))
{
  return;
}
