/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/private/object.h"
#include "picrin/private/state.h"

#define UNIT_SIZE (sizeof(uint32_t) * CHAR_BIT)
#define BITMAP_SIZE (PIC_HEAP_PAGE_SIZE / sizeof(union header) / UNIT_SIZE)
#define PAGE_UNITS ((PIC_HEAP_PAGE_SIZE - sizeof(struct heap_page)) / sizeof(union header))

struct heap_page {
  struct heap_page *next;
  size_t current;
  uint32_t bitmap[BITMAP_SIZE];
  uint32_t shadow[BITMAP_SIZE];
  uint32_t index[BITMAP_SIZE / UNIT_SIZE];
};

struct heap *
pic_heap_open(pic_state *pic)
{
  struct heap *heap;

  heap = pic_malloc(pic, sizeof(struct heap));

  heap->pages = NULL;
  heap->weaks = NULL;

  return heap;
}

void
pic_heap_close(pic_state *pic, struct heap *heap)
{
  struct heap_page *page;

  while (heap->pages) {
    page = heap->pages;
    heap->pages = heap->pages->next;
    pic_free(pic, page);
  }
  pic_free(pic, heap);
}

/* bitmap */

static union header *
index2header(struct heap_page *page, size_t index)
{
  return ((union header *)(page + 1)) + index;
}

static struct heap_page *
obj2page(pic_state *PIC_UNUSED(pic), union header *h)
{
  unsigned long int mask = ~(PIC_HEAP_PAGE_SIZE - 1);

  return (struct heap_page *)(((unsigned long int) h) & mask);
}

static int
numofbits(unsigned long bits)
{
    bits = bits - (bits >> 1 & 0x55555555);
    bits = (bits & 0x33333333) + (bits >> 2 & 0x33333333);
    bits = bits + ((bits >> 4) & 0x0f0f0f0f);
    bits = bits * 0x01010101;

    return bits >> 24;
}

static void
mark_at(struct heap_page *page, size_t index, size_t size)
{
  size_t mark_size;

  while (0 < size) {
    if (size  <= UNIT_SIZE - (index % UNIT_SIZE))
      mark_size = size;
    else
      mark_size = UNIT_SIZE - (index % UNIT_SIZE);
    page->bitmap[index / UNIT_SIZE] |= ~(-1 << mark_size) << (index % UNIT_SIZE);
    size  -= mark_size;
    index += mark_size;
  }
}

static int
is_marked_at(uint32_t *bitmap, size_t index, size_t size)
{
  size_t test_size;

  while (0 < size) {
    if (size  <= UNIT_SIZE - (index % UNIT_SIZE))
      test_size = size;
    else
      test_size = UNIT_SIZE - (index % UNIT_SIZE);

    if ((bitmap[index / UNIT_SIZE] >> (index % UNIT_SIZE)) & ~(-1 << test_size))
      return 1;
    size  -= test_size;
    index += test_size;
  }

  return 0;

}

static void *
heap_alloc_heap_page(struct heap_page *page, size_t nunits)
{
  size_t index;
  union header *p;

  for (index = page->current; index < PAGE_UNITS - (nunits + 1); ++index) {
    if (index % UNIT_SIZE == 0 && is_marked_at(page->index, index / UNIT_SIZE, 1)) {
      index += UNIT_SIZE;
    }

    if (! is_marked_at(page->bitmap, index, nunits+1)) {
      mark_at(page, index, nunits+1);
      p = index2header(page, index);
      p->s.size = nunits;
      page->current = index + nunits + 1;
      return (void *)(p+1);
    }
  }

  return NULL;
}

static void *
heap_alloc(pic_state *pic, size_t size)
{
  struct heap_page *p;
  void *ret;
  size_t nunits;

  assert(size > 0);

  nunits = (size + sizeof(union header) - 1) / sizeof(union header);

  p = pic->heap->pages;

  while (p) {
    ret = heap_alloc_heap_page(p, nunits);
    if (ret != NULL) {
      return ret;
    }
    p = p->next;
  }

  return NULL;
}

static void
heap_morecore(pic_state *pic)
{
  struct heap_page *page;

  assert(2 <= PAGE_UNITS);

  if (PIC_MEMALIGN(pic, (void **)&page, PIC_HEAP_PAGE_SIZE, PIC_HEAP_PAGE_SIZE) != 0)
    pic_panic(pic, "memory exhausted");

  memset(page->bitmap, 0, sizeof(page->bitmap));
  memset(page->index, 0, sizeof(page->index));
  page->current = 0;

  page->next = pic->heap->pages;

  pic->heap->pages = page;
}

static bool
is_marked(pic_state *pic, struct object *obj)
{
  union header *h = ((union header *)obj) - 1;
  struct heap_page *page;
  size_t i;

  page = obj2page(pic, h);

  i = h - ((union header *)(page + 1));

  return is_marked_at(page->bitmap, i, h->s.size + 1);
}

static void
mark(pic_state *pic, struct object *obj)
{
  union header *h = ((union header *)obj) - 1;
  struct heap_page *page;
  size_t i;

  page = obj2page(pic, h);

  i = h - ((union header *)(page + 1));

  mark_at(page, i, h->s.size + 1);
}

static size_t
gc_sweep_page(pic_state *pic, struct heap_page *page)
{
  size_t index, i, inuse = 0;
  union header *h;

  for (i = 0; i < BITMAP_SIZE; ++i) {
    page->shadow[i] &= ~page->bitmap[i];
    inuse += numofbits(page->bitmap[i]);
  }

  for (index = 0; index < PAGE_UNITS; ++index) {
    if (page->shadow[index / UNIT_SIZE] & (1 << (index % UNIT_SIZE))) {
      h = index2header(page, index);
      index += h->s.size;
      gc_finalize_object(pic, (struct object *) (h + 1));
    }
  }
  return inuse;
}

static void
gc_sweep_phase(pic_state *pic)
{
  struct heap_page *page;
  int it;
  khash_t(weak) *h;
  khash_t(oblist) *s = &pic->oblist;
  symbol *sym;
  struct object *obj;
  size_t total = 0, inuse = 0;

  /* weak maps */
  while (pic->heap->weaks != NULL) {
    h = &pic->heap->weaks->hash;
    for (it = kh_begin(h); it != kh_end(h); ++it) {
      if (! kh_exist(h, it))
        continue;
      obj = kh_key(h, it);
      if (! is_marked(pic, obj)) {
        kh_del(weak, h, it);
      }
    }
    pic->heap->weaks = pic->heap->weaks->prev;
  }

  /* symbol table */
  for (it = kh_begin(s); it != kh_end(s); ++it) {
    if (! kh_exist(s, it))
      continue;
    sym = kh_val(s, it);
    if (sym && ! is_marked(pic, (struct object *)sym)) {
      kh_del(oblist, s, it);
    }
  }

  page = pic->heap->pages;
  while (page) {
    inuse += gc_sweep_page(pic, page);
    total += PAGE_UNITS;
    page = page->next;
  }

  if (PIC_PAGE_REQUEST_THRESHOLD(total) <= inuse) {
    heap_morecore(pic);
  }
}

void
gc_init(pic_state *pic)
{
  struct heap_page *page;

  page = pic->heap->pages;
  while (page) {
    /* clear mark bits */
    memcpy(page->shadow, page->bitmap, sizeof(page->bitmap));
    memset(page->bitmap, 0, sizeof(page->bitmap));
    memset(page->index, 0, sizeof(page->index));
    page->current = 0;
    page = page->next;
  }
}

struct object *
pic_obj_alloc_unsafe(pic_state *pic, size_t size, int type)
{
  struct object *obj;

#if GC_STRESS
  pic_gc(pic);
#endif

  obj = (struct object *)heap_alloc(pic, size);
  if (obj == NULL) {
    pic_gc(pic);
    obj = (struct object *)heap_alloc(pic, size);
    if (obj == NULL) {
      heap_morecore(pic);
      obj = (struct object *)heap_alloc(pic, size);
      if (obj == NULL)
	pic_panic(pic, "GC memory exhausted");
    }
  }
  obj->u.basic.tt = type;

  return obj;
}
