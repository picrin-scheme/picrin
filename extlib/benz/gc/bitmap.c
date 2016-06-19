/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/private/object.h"
#include "picrin/private/khash.h"
#include "picrin/private/state.h"

union header {
  struct {
    union header *ptr;
    size_t size;
  } s;
};

#define UNIT_SIZE (sizeof(uint32_t) * CHAR_BIT)
#define BITMAP_SIZE                                                     \
  (PIC_HEAP_PAGE_SIZE / sizeof(union header) / UNIT_SIZE)
#define HEADER_SIZE ((PIC_HEAP_PAGE_SIZE - sizeof(struct heap_page)) / sizeof(union header))


struct heap_page {
  struct heap_page *next;
  size_t current;
  uint32_t bitmap[BITMAP_SIZE];
  uint32_t shadow[BITMAP_SIZE];
  uint32_t index[BITMAP_SIZE / UNIT_SIZE];
};

struct object {
  union {
    struct basic basic;
    struct identifier id;
    struct string str;
    struct blob blob;
    struct pair pair;
    struct vector vec;
    struct dict dict;
    struct weak weak;
    struct data data;
    struct record rec;
    struct env env;
    struct proc proc;
    struct context cxt;
    struct port port;
    struct error err;
    struct checkpoint cp;
  } u;
};

struct heap {
  struct heap_page *pages;
  struct weak *weaks;       /* weak map chain */
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

#if PIC_USE_LIBC
void *
pic_default_allocf(void *PIC_UNUSED(userdata), void *ptr, size_t size)
{
  if (size != 0) {
    return realloc(ptr, size);
  }
  free(ptr);
  return NULL;
}
#endif

void *
pic_malloc(pic_state *pic, size_t size)
{
  void *ptr;

  ptr = pic->allocf(pic->userdata, NULL, size);
  if (ptr == NULL && size > 0) {
    pic_panic(pic, "memory exhausted");
  }
  return ptr;
}

void *
pic_realloc(pic_state *pic, void *ptr, size_t size)
{
  ptr = pic->allocf(pic->userdata, ptr, size);
  if (ptr == NULL && size > 0) {
    pic_panic(pic, "memory exhausted");
  }
  return ptr;
}

void *
pic_calloc(pic_state *pic, size_t count, size_t size)
{
  void *ptr;

  size *= count;
  ptr = pic->allocf(pic->userdata, NULL, size);
  if (ptr == NULL && size > 0) {
    pic_panic(pic, "memory exhausted");
  }
  memset(ptr, 0, size);
  return ptr;
}

void
pic_free(pic_state *pic, void *ptr)
{
  pic->allocf(pic->userdata, ptr, 0);
}

static void
gc_protect(pic_state *pic, struct object *obj)
{
  if (pic->arena_idx >= pic->arena_size) {
    pic->arena_size = pic->arena_size * 2 + 1;
    pic->arena = pic_realloc(pic, pic->arena, sizeof(struct object *) * pic->arena_size);
  }
  pic->arena[pic->arena_idx++] = obj;
}

pic_value
pic_protect(pic_state *pic, pic_value v)
{
  if (! pic_obj_p(pic, v))
    return v;

  gc_protect(pic, pic_obj_ptr(v));

  return v;
}

size_t
pic_enter(pic_state *pic)
{
  return pic->arena_idx;
}

void
pic_leave(pic_state *pic, size_t state)
{
  pic->arena_idx = state;
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
  size_t mask = ~(-1 << size);
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

  for (index = page->current; index < HEADER_SIZE - (nunits + 1); ++index) {
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

  assert(2 <= HEADER_SIZE);

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

/* MARK */

static void gc_mark_object(pic_state *, struct object *);

static void
gc_mark(pic_state *pic, pic_value v)
{
  if (! pic_obj_p(pic, v))
    return;

  gc_mark_object(pic, pic_obj_ptr(v));
}

static void
gc_mark_object(pic_state *pic, struct object *obj)
{
 loop:

  if (is_marked(pic, obj))
    return;

  mark(pic, obj);

#define LOOP(o) obj = (struct object *)(o); goto loop

  switch (obj->u.basic.tt) {
  case PIC_TYPE_PAIR: {
    gc_mark(pic, obj->u.pair.car);
    if (pic_obj_p(pic, obj->u.pair.cdr)) {
      LOOP(pic_obj_ptr(obj->u.pair.cdr));
    }
    break;
  }
  case PIC_TYPE_CXT: {
    int i;

    for (i = 0; i < obj->u.cxt.regc; ++i) {
      gc_mark(pic, obj->u.cxt.regs[i]);
    }
    if (obj->u.cxt.up) {
      LOOP(obj->u.cxt.up);
    }
    break;
  }
  case PIC_TYPE_FUNC: {
    int i;
    for (i = 0; i < obj->u.proc.u.f.localc; ++i) {
      gc_mark(pic, obj->u.proc.locals[i]);
    }
    break;
  }
  case PIC_TYPE_IREP: {
    if (obj->u.proc.u.i.cxt) {
      LOOP(obj->u.proc.u.i.cxt);
    }
    break;
  }
  case PIC_TYPE_PORT: {
    break;
  }
  case PIC_TYPE_ERROR: {
    gc_mark_object(pic, (struct object *)obj->u.err.type);
    gc_mark_object(pic, (struct object *)obj->u.err.msg);
    gc_mark(pic, obj->u.err.irrs);
    LOOP(obj->u.err.stack);
    break;
  }
  case PIC_TYPE_STRING: {
    break;
  }
  case PIC_TYPE_VECTOR: {
    int i;
    for (i = 0; i < obj->u.vec.len; ++i) {
      gc_mark(pic, obj->u.vec.data[i]);
    }
    break;
  }
  case PIC_TYPE_BLOB: {
    break;
  }
  case PIC_TYPE_ID: {
    gc_mark_object(pic, (struct object *)obj->u.id.u.id);
    LOOP(obj->u.id.env);
    break;
  }
  case PIC_TYPE_ENV: {
    khash_t(env) *h = &obj->u.env.map;
    int it;

    for (it = kh_begin(h); it != kh_end(h); ++it) {
      if (kh_exist(h, it)) {
        gc_mark_object(pic, (struct object *)kh_key(h, it));
        gc_mark_object(pic, (struct object *)kh_val(h, it));
      }
    }
    if (obj->u.env.up) {
      LOOP(obj->u.env.up);
    }
    break;
  }
  case PIC_TYPE_DATA: {
    if (obj->u.data.type->mark) {
      obj->u.data.type->mark(pic, obj->u.data.data, gc_mark);
    }
    break;
  }
  case PIC_TYPE_DICT: {
    pic_value key, val;
    int it = 0;

    while (pic_dict_next(pic, pic_obj_value(&obj->u.dict), &it, &key, &val)) {
      gc_mark(pic, key);
      gc_mark(pic, val);
    }
    break;
  }
  case PIC_TYPE_RECORD: {
    gc_mark(pic, obj->u.rec.type);
    if (pic_obj_p(pic, obj->u.rec.datum)) {
      LOOP(pic_obj_ptr(obj->u.rec.datum));
    }
    break;
  }
  case PIC_TYPE_SYMBOL: {
    LOOP(obj->u.id.u.str);
    break;
  }
  case PIC_TYPE_WEAK: {
    struct weak *weak = (struct weak *)obj;

    weak->prev = pic->heap->weaks;
    pic->heap->weaks = weak;
    break;
  }
  case PIC_TYPE_CP: {
    if (obj->u.cp.prev) {
      gc_mark_object(pic, (struct object *)obj->u.cp.prev);
    }
    if (obj->u.cp.in) {
      gc_mark_object(pic, (struct object *)obj->u.cp.in);
    }
    if (obj->u.cp.out) {
      LOOP((struct object *)obj->u.cp.out);
    }
    break;
  }
  default:
    PIC_UNREACHABLE();
  }
}

static void
gc_mark_phase(pic_state *pic)
{
  pic_value *stack;
  struct callinfo *ci;
  struct list_head *list;
  int it;
  size_t j;

  assert(pic->heap->weaks == NULL);

  /* checkpoint */
  if (pic->cp) {
    gc_mark_object(pic, (struct object *)pic->cp);
  }

  /* stack */
  for (stack = pic->stbase; stack != pic->sp; ++stack) {
    gc_mark(pic, *stack);
  }

  /* callinfo */
  for (ci = pic->ci; ci != pic->cibase; --ci) {
    if (ci->cxt) {
      gc_mark_object(pic, (struct object *)ci->cxt);
    }
  }

  /* arena */
  for (j = 0; j < pic->arena_idx; ++j) {
    gc_mark_object(pic, (struct object *)pic->arena[j]);
  }

  /* ireps */
  for (list = pic->ireps.next; list != &pic->ireps; list = list->next) {
    struct irep *irep = (struct irep *)list;
    for (j = 0; j < irep->npool; ++j) {
      gc_mark_object(pic, irep->pool[j]);
    }
  }

  /* global variables */
  gc_mark(pic, pic->globals);

  /* macro objects */
  gc_mark(pic, pic->macros);

  /* error object */
  gc_mark(pic, pic->err);

  /* features */
  gc_mark(pic, pic->features);

  /* library table */
  for (it = kh_begin(&pic->ltable); it != kh_end(&pic->ltable); ++it) {
    if (! kh_exist(&pic->ltable, it)) {
      continue;
    }
    gc_mark_object(pic, (struct object *)kh_val(&pic->ltable, it).name);
    gc_mark_object(pic, (struct object *)kh_val(&pic->ltable, it).env);
    gc_mark_object(pic, (struct object *)kh_val(&pic->ltable, it).exports);
  }

  /* weak maps */
  do {
    struct object *key;
    pic_value val;
    int it;
    khash_t(weak) *h;
    struct weak *weak;

    j = 0;
    weak = pic->heap->weaks;

    while (weak != NULL) {
      h = &weak->hash;
      for (it = kh_begin(h); it != kh_end(h); ++it) {
        if (! kh_exist(h, it))
          continue;
        key = kh_key(h, it);
        val = kh_val(h, it);
        if (is_marked(pic, key)) {
          if (pic_obj_p(pic, val) && ! is_marked(pic, pic_obj_ptr(val))) {
            gc_mark(pic, val);
            ++j;
          }
        }
      }
      weak = weak->prev;
    }
  } while (j > 0);
}

/* SWEEP */

static void
gc_finalize_object(pic_state *pic, struct object *obj)
{
  switch (obj->u.basic.tt) {
  case PIC_TYPE_VECTOR: {
    pic_free(pic, obj->u.vec.data);
    break;
  }
  case PIC_TYPE_BLOB: {
    pic_free(pic, obj->u.blob.data);
    break;
  }
  case PIC_TYPE_STRING: {
    pic_rope_decref(pic, obj->u.str.rope);
    break;
  }
  case PIC_TYPE_ENV: {
    kh_destroy(env, &obj->u.env.map);
    break;
  }
  case PIC_TYPE_DATA: {
    if (obj->u.data.type->dtor) {
      obj->u.data.type->dtor(pic, obj->u.data.data);
    }
    break;
  }
  case PIC_TYPE_DICT: {
    kh_destroy(dict, &obj->u.dict.hash);
    break;
  }
  case PIC_TYPE_SYMBOL: {
    /* TODO: remove this symbol's entry from pic->syms immediately */
    break;
  }
  case PIC_TYPE_WEAK: {
    kh_destroy(weak, &obj->u.weak.hash);
    break;
  }
  case PIC_TYPE_IREP: {
    pic_irep_decref(pic, obj->u.proc.u.i.irep);
    break;
  }

  case PIC_TYPE_PAIR:
  case PIC_TYPE_CXT:
  case PIC_TYPE_PORT:
  case PIC_TYPE_ERROR:
  case PIC_TYPE_ID:
  case PIC_TYPE_RECORD:
  case PIC_TYPE_CP:
  case PIC_TYPE_FUNC:
    break;

  default:
    PIC_UNREACHABLE();
  }
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
    if (! is_marked(pic, (struct object *)sym)) {
      kh_del(oblist, s, it);
    }
  }

  page = pic->heap->pages;
  while(page) {
    size_t index, i;
    union header *h;

    total += HEADER_SIZE;
    for (i = 0; i < BITMAP_SIZE; ++i) {
      page->shadow[i] &= ~page->bitmap[i];
      inuse += numofbits(page->bitmap[i]);
    }

    for (index = 0; index < HEADER_SIZE; ++index) {
      if (page->shadow[index / UNIT_SIZE] & (1 << (index % UNIT_SIZE))) {
        h = index2header(page, index);
        index += h->s.size;
        gc_finalize_object(pic, (struct object *) (h + 1));
      }
    }
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

void
pic_gc(pic_state *pic)
{
  if (! pic->gc_enable) {
    return;
  }

  gc_init(pic);

  gc_mark_phase(pic);
  gc_sweep_phase(pic);
}

void *
pic_alloca(pic_state *pic, size_t n)
{
  static const pic_data_type t = { "pic_alloca", pic_free, 0 };

  /* TODO: optimize */
  return pic_data(pic, pic_data_value(pic, pic_malloc(pic, n), &t));
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

struct object *
pic_obj_alloc(pic_state *pic, size_t size, int type)
{
  struct object *obj;

  obj = pic_obj_alloc_unsafe(pic, size, type);

  gc_protect(pic, obj);
  return obj;
}
