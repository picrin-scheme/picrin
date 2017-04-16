/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"
#include "state.h"

#define PAGE_UNITS ((PIC_HEAP_PAGE_SIZE - offsetof(struct heap_page, u)) / sizeof(union header))

#define alignof(type) offsetof(struct { char c; type m; }, m)
#define roundup(n,unit) (((n) + (unit) - 1) / (unit) * (unit))

struct object {
  union {
    struct basic basic;
    struct symbol sym;
    struct string str;
    struct blob blob;
    struct pair pair;
    struct vector vec;
    struct dict dict;
    struct weak weak;
    struct data data;
    struct record rec;
    struct proc proc;
    struct frame frame;
    struct port port;
    struct error err;
    struct irep irep;
  } u;
};

struct free_region {
  union header *ptr;
  size_t size;
};

union header {
  struct free_region s;
  char alignment[roundup(sizeof(struct free_region), alignof(struct object))];
};

struct heap_page {
  struct heap_page *next;
  union {
    union header basep[1];
    struct object alignment;
  } u;
};

struct heap {
  union header base, *freep;
  struct heap_page *pages;
  struct weak *weaks;           /* weak map chain */
};

#define unitsof(type) ((type2size(type) + sizeof(union header) - 1) / sizeof(union header))

struct heap *
pic_heap_open(pic_state *pic)
{
  struct heap *heap;

  heap = pic_malloc(pic, sizeof(struct heap));

  heap->base.s.ptr = &heap->base;
  heap->base.s.size = 0; /* not 1, since it must never be used for allocation */
  heap->freep = &heap->base;

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
  if (pic->ai >= pic->arena_size) {
    pic->arena_size = pic->arena_size * 2 + 1;
    pic->arena = pic_realloc(pic, pic->arena, sizeof(struct object *) * pic->arena_size);
  }
  pic->arena[pic->ai++] = obj;
}

pic_value
pic_protect(pic_state *pic, pic_value v)
{
  if (! obj_p(pic, v))
    return v;

  gc_protect(pic, obj_ptr(pic, v));

  return v;
}

size_t
pic_enter(pic_state *pic)
{
  return pic->ai;
}

void
pic_leave(pic_state *pic, size_t state)
{
  pic->ai = state;
}

void *
pic_alloca(pic_state *pic, size_t n)
{
  static const pic_data_type t = { "pic_alloca", pic_free };

  return pic_data(pic, pic_data_value(pic, pic_malloc(pic, n), &t));
}

/* MARK */

PIC_STATIC_INLINE bool
is_marked(struct object *obj)
{
  return obj->u.basic.tt & GC_MARK;
}

PIC_STATIC_INLINE void
mark(struct object *obj)
{
  obj->u.basic.tt |= GC_MARK;
}
static void gc_mark_object(pic_state *, struct object *);

static void
gc_mark(pic_state *pic, pic_value v)
{
  if (! obj_p(pic, v))
    return;

  gc_mark_object(pic, obj_ptr(pic, v));
}

static void
gc_mark_object(pic_state *pic, struct object *obj)
{
 loop:

  if (is_marked(obj))
    return;

  mark(obj);

#define LOOP(o) obj = (struct object *)(o); goto loop

  switch (obj_type(pic, obj)) {
  case PIC_TYPE_PAIR: {
    gc_mark(pic, obj->u.pair.car);
    if (obj_p(pic, obj->u.pair.cdr)) {
      LOOP(obj_ptr(pic, obj->u.pair.cdr));
    }
    break;
  }
  case PIC_TYPE_FRAME: {
    int i;

    for (i = 0; i < obj->u.frame.regc; ++i) {
      gc_mark(pic, obj->u.frame.regs[i]);
    }
    if (obj->u.frame.up) {
      LOOP(obj->u.frame.up);
    }
    break;
  }
  case PIC_TYPE_PROC_FUNC: {
    if (obj->u.proc.env) {
      LOOP(obj->u.proc.env);
    }
    break;
  }
  case PIC_TYPE_PROC_IREP: {
    if (obj->u.proc.env) {
      gc_mark_object(pic, (struct object *)obj->u.proc.env);
    }
    LOOP(obj->u.proc.u.irep);
    break;
  }
  case PIC_TYPE_IREP: {
    size_t i;
    for (i = 0; i < obj->u.irep.objc; ++i) {
      gc_mark(pic, obj->u.irep.obj[i]);
    }
    for (i = 0; i < obj->u.irep.irepc; ++i) {
      gc_mark_object(pic, (struct object *)obj->u.irep.irep[i]);
    }
    break;
  }
  case PIC_TYPE_PORT: {
    break;
  }
  case PIC_TYPE_ERROR: {
    gc_mark_object(pic, (struct object *)obj->u.err.type);
    gc_mark(pic, obj->u.err.irrs);
    LOOP(obj->u.err.msg);
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
  case PIC_TYPE_DATA: {
    break;
  }
  case PIC_TYPE_DICT: {
    pic_value key, val;
    int it = 0;

    while (pic_dict_next(pic, obj_value(pic, &obj->u.dict), &it, &key, &val)) {
      gc_mark(pic, key);
      gc_mark(pic, val);
    }
    break;
  }
  case PIC_TYPE_RECORD: {
    gc_mark(pic, obj->u.rec.datum);
    LOOP(obj->u.rec.type);
    break;
  }
  case PIC_TYPE_SYMBOL: {
    LOOP(obj->u.sym.str);
    break;
  }
  case PIC_TYPE_WEAK: {
    struct weak *weak = (struct weak *)obj;

    weak->prev = pic->heap->weaks;
    pic->heap->weaks = weak;
    break;
  }
  default:
    PIC_UNREACHABLE();
  }
}

static void
gc_mark_phase(pic_state *pic)
{
  struct context *cxt;
  size_t j;

  assert(pic->heap->weaks == NULL);

  /* context */
  for (cxt = pic->cxt; cxt != NULL; cxt = cxt->prev) {
    if (cxt->fp) gc_mark_object(pic, (struct object *)cxt->fp);
    if (cxt->sp) gc_mark_object(pic, (struct object *)cxt->sp);
    if (cxt->irep) gc_mark_object(pic, (struct object *)cxt->irep);
  }

  /* arena */
  for (j = 0; j < pic->ai; ++j) {
    gc_mark_object(pic, (struct object *)pic->arena[j]);
  }

  /* global variables */
  gc_mark(pic, pic->globals);

  /* dynamic environment */
  gc_mark(pic, pic->dyn_env);

  /* top continuation */
  gc_mark(pic, pic->halt);

  /* features */
  gc_mark(pic, pic->features);

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
        if (is_marked(key)) {
          if (obj_p(pic, val) && ! is_marked(obj_ptr(pic, val))) {
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
  switch (obj_type(pic, obj)) {
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
    struct irep *irep = &obj->u.irep;
    if ((irep->flags & IREP_CODE_STATIC) == 0) {
      pic_free(pic, (code_t *) irep->code);
    }
    pic_free(pic, irep->obj);
    pic_free(pic, irep->irep);
    break;
  }
  case PIC_TYPE_PORT: {
    pic_fclose(pic, obj_value(pic, obj)); /* FIXME */
    break;
  }
  case PIC_TYPE_FRAME: {
    pic_free(pic, obj->u.frame.regs);
    break;
  }

  case PIC_TYPE_PAIR:
  case PIC_TYPE_ERROR:
  case PIC_TYPE_RECORD:
  case PIC_TYPE_PROC_FUNC:
  case PIC_TYPE_PROC_IREP:
    break;

  default:
    PIC_UNREACHABLE();
  }
}

static size_t
type2size(int type)
{
  switch (type) {
  case PIC_TYPE_VECTOR: return sizeof(struct vector);
  case PIC_TYPE_BLOB: return sizeof(struct blob);
  case PIC_TYPE_STRING: return sizeof(struct string);
  case PIC_TYPE_DATA: return sizeof(struct data);
  case PIC_TYPE_DICT: return sizeof(struct dict);
  case PIC_TYPE_SYMBOL: return sizeof(struct symbol);
  case PIC_TYPE_WEAK: return sizeof(struct weak);
  case PIC_TYPE_IREP: return sizeof(struct irep);
  case PIC_TYPE_PORT: return sizeof(struct port);
  case PIC_TYPE_PAIR: return sizeof(struct pair);
  case PIC_TYPE_FRAME: return sizeof(struct frame);
  case PIC_TYPE_ERROR: return sizeof(struct error);
  case PIC_TYPE_RECORD: return sizeof(struct record);
  case PIC_TYPE_PROC_FUNC: return sizeof(struct proc);
  case PIC_TYPE_PROC_IREP: return sizeof(struct proc);
  default: PIC_UNREACHABLE();
  }
}

static struct object *
obj_alloc(pic_state *pic, int type)
{
  union header *p, *prevp;
  struct object *obj;
  size_t nunits;

  nunits = unitsof(type);

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

  obj = (struct object *) p;
  obj->u.basic.tt = type;
  return obj;
}

static void
free_chunk(pic_state *pic, union header *bp)
{
  union header *p;

  assert(bp != NULL);

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

  bp = page->u.basep;
  bp->s.size = 0;      /* bp is never used for allocation */
  free_chunk(pic, bp);

  np = page->u.basep + 1;
  np->s.size = PAGE_UNITS - 1;
  free_chunk(pic, np);

  pic->heap->pages = page;
}

static size_t
gc_sweep_page(pic_state *pic, struct heap_page *page)
{
  union header *bp, *p, *head = NULL, *tail = NULL;
  struct object *obj;
  size_t alive = 0, nunits;

  for (bp = page->u.basep; ; bp = bp->s.ptr) {
    p = bp + (bp->s.size ? bp->s.size : 1); /* first bp's size is 0, so force advnce */
    while (p != bp->s.ptr) {
      if (p < page->u.basep || page->u.basep + PAGE_UNITS <= p) {
        goto escape;
      }
      obj = (struct object *) p;
      nunits = unitsof(obj_type(pic, obj));
      if (obj->u.basic.tt & GC_MARK) {
        obj->u.basic.tt &= ~GC_MARK;
        alive += nunits;
      } else {
        gc_finalize_object(pic, obj);
        if (head == NULL) {
          head = p;
        }
        if (tail != NULL) {
          tail->s.ptr = p;
        }
        tail = p;
        tail->s.size = nunits;
        tail->s.ptr = NULL; /* We can safely reuse ptr field of dead object */
      }
      p += nunits;
    }
  }
 escape:

  /* free! */
  while (head != NULL) {
    p = head;
    head = head->s.ptr;
    free_chunk(pic, p);
  }

  return alive;
}

static void
gc_sweep_phase(pic_state *pic)
{
  struct heap_page *page;
  int it;
  khash_t(weak) *h;
  khash_t(oblist) *s = &pic->oblist;
  struct symbol *sym;
  struct object *obj;
  size_t total = 0, inuse = 0;

  /* weak maps */
  while (pic->heap->weaks != NULL) {
    h = &pic->heap->weaks->hash;
    for (it = kh_begin(h); it != kh_end(h); ++it) {
      if (! kh_exist(h, it))
        continue;
      obj = kh_key(h, it);
      if (! is_marked(obj)) {
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
    if (sym && ! is_marked((struct object *)sym)) {
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
pic_gc(pic_state *pic)
{
  if (! pic->gc_enable) {
    return;
  }

  gc_mark_phase(pic);
  gc_sweep_phase(pic);
}

struct object *
pic_obj_alloc_unsafe(pic_state *pic, int type)
{
  struct object *obj;

#if GC_STRESS
  pic_gc(pic);
#endif

  obj = obj_alloc(pic, type);
  if (obj == NULL) {
    pic_gc(pic);
    obj = obj_alloc(pic, type);
    if (obj == NULL) {
      heap_morecore(pic);
      obj = obj_alloc(pic, type);
      if (obj == NULL)
	pic_panic(pic, "GC memory exhausted");
    }
  }

  return obj;
}

struct object *
pic_obj_alloc(pic_state *pic, int type)
{
  struct object *obj;

  obj = pic_obj_alloc_unsafe(pic, type);

  gc_protect(pic, obj);
  return obj;
}
