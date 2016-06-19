/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/private/object.h"
#include "picrin/private/state.h"

#define UNIT_SIZE (sizeof(uint32_t) * CHAR_BIT)
#define BITMAP_SIZE (PIC_HEAP_PAGE_SIZE / sizeof(union header) / UNIT_SIZE)

struct heap_page {
  struct heap_page *next;
  size_t freep;
  uint32_t bitmap[BITMAP_SIZE];
  uint32_t shadow[BITMAP_SIZE];
  union header basep[1];
};

/* bitmap */

static union header *
index2header(struct heap_page *page, size_t index)
{
  return page->basep + index;
}

static struct heap_page *
obj2page(pic_state *PIC_UNUSED(pic), union header *h)
{
  return (struct heap_page *)(((unsigned long)h) & ~(PIC_HEAP_PAGE_SIZE - 1));
}

static int
popcount32(uint32_t bits)
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

    if ((bitmap[index / UNIT_SIZE] >> (index % UNIT_SIZE)) & ~((~0) << test_size))
      return 1;
    size  -= test_size;
    index += test_size;
  }

  return 0;
}

static void *
heap_alloc(pic_state *pic, size_t size)
{
  struct heap_page *page;
  size_t nunits;

  assert(size > 0);

  nunits = (size + sizeof(union header) - 1) / sizeof(union header) + 1;

  page = pic->heap->pages;
  while (page) {
    size_t index;
    union header *h;

    for (index = page->freep; index < PAGE_UNITS - nunits; ++index) {
      if (! is_marked_at(page->bitmap, index, nunits)) {
        mark_at(page, index, nunits);
        h = index2header(page, index);
        h->s.size = nunits;
        page->freep = index + nunits;
        return (void *)(h + 1);
      }
    }
    page = page->next;
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
  page->freep = 0;

  page->next = pic->heap->pages;

  pic->heap->pages = page;
}

static bool
is_marked(pic_state *pic, struct object *obj)
{
  union header *h = ((union header *)obj) - 1;
  struct heap_page *page;
  size_t index;

  page = obj2page(pic, h);
  index = h - page->basep;

  return is_marked_at(page->bitmap, index, h->s.size);
}

static void
mark(pic_state *pic, struct object *obj)
{
  union header *h = ((union header *)obj) - 1;
  struct heap_page *page;
  size_t index;

  page = obj2page(pic, h);
  index = h - page->basep;

  mark_at(page, index, h->s.size);
}

static size_t
gc_sweep_page(pic_state *pic, struct heap_page *page)
{
  size_t index, i, inuse = 0;
  union header *h;

  for (i = 0; i < BITMAP_SIZE; ++i) {
    page->shadow[i] &= ~page->bitmap[i];
    inuse += popcount32(page->bitmap[i]);
  }

  for (index = 0; index < PAGE_UNITS; ++index) {
    if (page->shadow[index / UNIT_SIZE] & (1 << (index % UNIT_SIZE))) {
      h = index2header(page, index);
      index += h->s.size - 1;
      gc_finalize_object(pic, (struct object *) (h + 1));
    }
  }
  return inuse;
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
    page->freep = 0;
    page = page->next;
  }
}
