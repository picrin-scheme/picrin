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

union header {
  struct {
    union header *ptr;
    size_t size;
  } s;
};

struct heap_page {
  union header *basep, *endp;
  struct heap_page *next;
};

struct pic_object {
  union {
    struct pic_basic basic;
    struct pic_identifier id;
    struct pic_string str;
    struct pic_blob blob;
    struct pic_pair pair;
    struct pic_vector vec;
    struct pic_dict dict;
    struct pic_weak weak;
    struct pic_data data;
    struct pic_record rec;
    struct pic_env env;
    struct pic_proc proc;
    struct pic_context cxt;
    struct pic_port port;
    struct pic_error err;
    struct pic_checkpoint cp;
  } u;
};

struct pic_heap {
  union header base, *freep;
  struct heap_page *pages;
  struct pic_weak *weaks;       /* weak map chain */
};

struct pic_heap *
pic_heap_open(pic_state *pic)
{
  struct pic_heap *heap;

  heap = pic_malloc(pic, sizeof(struct pic_heap));

  heap->base.s.ptr = &heap->base;
  heap->base.s.size = 0; /* not 1, since it must never be used for allocation */

  heap->freep = &heap->base;
  heap->pages = NULL;

  heap->weaks = NULL;

  return heap;
}

void
pic_heap_close(pic_state *pic, struct pic_heap *heap)
{
  struct heap_page *page;

  while (heap->pages) {
    page = heap->pages;
    heap->pages = heap->pages->next;
    pic_free(pic, page->basep);
    pic_free(pic, page);
  }
  pic_free(pic, heap);
}

#if PIC_ENABLE_LIBC
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
gc_protect(pic_state *pic, struct pic_object *obj)
{
  if (pic->arena_idx >= pic->arena_size) {
    pic->arena_size = pic->arena_size * 2 + 1;
    pic->arena = pic_realloc(pic, pic->arena, sizeof(struct pic_object *) * pic->arena_size);
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
  size_t nunits;

  nunits = PIC_HEAP_PAGE_SIZE / sizeof(union header);

  assert(nunits >= 2);

  bp = pic_malloc(pic, PIC_HEAP_PAGE_SIZE);
  bp->s.size = 0;               /* bp is never used for allocation */
  heap_free(pic, bp + 1);

  np = bp + 1;
  np->s.size = nunits - 1;
  heap_free(pic, np + 1);

  page = pic_malloc(pic, sizeof(struct heap_page));
  page->basep = bp;
  page->endp = bp + nunits;
  page->next = pic->heap->pages;

  pic->heap->pages = page;
}

/* MARK */

static void gc_mark_object(pic_state *, struct pic_object *);

static void
gc_mark(pic_state *pic, pic_value v)
{
  if (! pic_obj_p(pic, v))
    return;

  gc_mark_object(pic, pic_obj_ptr(v));
}

static void
gc_mark_object(pic_state *pic, struct pic_object *obj)
{
 loop:

  if (obj->u.basic.gc_mark == BLACK)
    return;

  obj->u.basic.gc_mark = BLACK;

#define LOOP(o) obj = (struct pic_object *)(o); goto loop

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
  case PIC_TYPE_PROC: {
    if (pic_proc_irep_p(&obj->u.proc)) {
      if (obj->u.proc.u.i.cxt) {
        LOOP(obj->u.proc.u.i.cxt);
      }
    } else {
      int i;
      for (i = 0; i < obj->u.proc.u.f.localc; ++i) {
        gc_mark(pic, obj->u.proc.locals[i]);
      }
    }
    break;
  }
  case PIC_TYPE_PORT: {
    break;
  }
  case PIC_TYPE_ERROR: {
    gc_mark_object(pic, (struct pic_object *)obj->u.err.type);
    gc_mark_object(pic, (struct pic_object *)obj->u.err.msg);
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
    gc_mark_object(pic, (struct pic_object *)obj->u.id.u.id);
    LOOP(obj->u.id.env);
    break;
  }
  case PIC_TYPE_ENV: {
    khash_t(env) *h = &obj->u.env.map;
    int it;

    for (it = kh_begin(h); it != kh_end(h); ++it) {
      if (kh_exist(h, it)) {
        gc_mark_object(pic, (struct pic_object *)kh_key(h, it));
        gc_mark_object(pic, (struct pic_object *)kh_val(h, it));
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
    struct pic_weak *weak = (struct pic_weak *)obj;

    weak->prev = pic->heap->weaks;
    pic->heap->weaks = weak;
    break;
  }
  case PIC_TYPE_CP: {
    if (obj->u.cp.prev) {
      gc_mark_object(pic, (struct pic_object *)obj->u.cp.prev);
    }
    if (obj->u.cp.in) {
      gc_mark_object(pic, (struct pic_object *)obj->u.cp.in);
    }
    if (obj->u.cp.out) {
      LOOP((struct pic_object *)obj->u.cp.out);
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
  pic_callinfo *ci;
  struct pic_proc **xhandler;
  struct pic_list *list;
  int it;
  size_t j;

  assert(pic->heap->weaks == NULL);

  /* checkpoint */
  if (pic->cp) {
    gc_mark_object(pic, (struct pic_object *)pic->cp);
  }

  /* stack */
  for (stack = pic->stbase; stack != pic->sp; ++stack) {
    gc_mark(pic, *stack);
  }

  /* callinfo */
  for (ci = pic->ci; ci != pic->cibase; --ci) {
    if (ci->cxt) {
      gc_mark_object(pic, (struct pic_object *)ci->cxt);
    }
  }

  /* exception handlers */
  for (xhandler = pic->xpbase; xhandler != pic->xp; ++xhandler) {
    gc_mark_object(pic, (struct pic_object *)*xhandler);
  }

  /* arena */
  for (j = 0; j < pic->arena_idx; ++j) {
    gc_mark_object(pic, (struct pic_object *)pic->arena[j]);
  }

  /* ireps */
  for (list = pic->ireps.next; list != &pic->ireps; list = list->next) {
    struct pic_irep *irep = (struct pic_irep *)list;
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

  /* parameter table */
  gc_mark(pic, pic->ptable);

  /* library table */
  for (it = kh_begin(&pic->ltable); it != kh_end(&pic->ltable); ++it) {
    if (! kh_exist(&pic->ltable, it)) {
      continue;
    }
    gc_mark_object(pic, (struct pic_object *)kh_val(&pic->ltable, it).name);
    gc_mark_object(pic, (struct pic_object *)kh_val(&pic->ltable, it).env);
    gc_mark_object(pic, (struct pic_object *)kh_val(&pic->ltable, it).exports);
  }

  /* weak maps */
  do {
    struct pic_object *key;
    pic_value val;
    int it;
    khash_t(weak) *h;
    struct pic_weak *weak;

    j = 0;
    weak = pic->heap->weaks;

    while (weak != NULL) {
      h = &weak->hash;
      for (it = kh_begin(h); it != kh_end(h); ++it) {
        if (! kh_exist(h, it))
          continue;
        key = kh_key(h, it);
        val = kh_val(h, it);
        if (key->u.basic.gc_mark == BLACK) {
          if (pic_obj_p(pic, val) && pic_obj_ptr(val)->u.basic.gc_mark == WHITE) {
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
gc_finalize_object(pic_state *pic, struct pic_object *obj)
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
  case PIC_TYPE_PROC: {
    if (pic_proc_irep_p(&obj->u.proc)) {
      pic_irep_decref(pic, obj->u.proc.u.i.irep);
    }
    break;
  }

  case PIC_TYPE_PAIR:
  case PIC_TYPE_CXT:
  case PIC_TYPE_PORT:
  case PIC_TYPE_ERROR:
  case PIC_TYPE_ID:
  case PIC_TYPE_RECORD:
  case PIC_TYPE_CP:
    break;

  default:
    PIC_UNREACHABLE();
  }
}

static size_t
gc_sweep_page(pic_state *pic, struct heap_page *page)
{
  union header *bp, *p, *head = NULL, *tail = NULL;
  struct pic_object *obj;
  size_t alive = 0;

  for (bp = page->basep; ; bp = bp->s.ptr) {
    p = bp + (bp->s.size ? bp->s.size : 1); /* first bp's size is 0, so force advnce */
    for (; p != bp->s.ptr; p += p->s.size) {
      if (p < page->basep || page->endp <= p) {
        goto escape;
      }
      obj = (struct pic_object *)(p + 1);
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
    gc_finalize_object(pic, (struct pic_object *)(p + 1));
    heap_free(pic, p + 1);
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
  pic_sym *sym;
  struct pic_object *obj;
  size_t total = 0, inuse = 0;

  /* weak maps */
  while (pic->heap->weaks != NULL) {
    h = &pic->heap->weaks->hash;
    for (it = kh_begin(h); it != kh_end(h); ++it) {
      if (! kh_exist(h, it))
        continue;
      obj = kh_key(h, it);
      if (obj->u.basic.gc_mark == WHITE) {
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
    if (sym && sym->gc_mark == WHITE) {
      kh_del(oblist, s, it);
    }
  }

  page = pic->heap->pages;
  while (page) {
    inuse += gc_sweep_page(pic, page);
    total += page->endp - page->basep;
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

void *
pic_alloca(pic_state *pic, size_t n)
{
  static const pic_data_type t = { "pic_alloca", pic_free, 0 };

  /* TODO: optimize */
  return pic_data(pic, pic_data_value(pic, pic_malloc(pic, n), &t));
}

struct pic_object *
pic_obj_alloc_unsafe(pic_state *pic, size_t size, int type)
{
  struct pic_object *obj;

#if GC_STRESS
  pic_gc(pic);
#endif

  obj = (struct pic_object *)heap_alloc(pic, size);
  if (obj == NULL) {
    pic_gc(pic);
    obj = (struct pic_object *)heap_alloc(pic, size);
    if (obj == NULL) {
      heap_morecore(pic);
      obj = (struct pic_object *)heap_alloc(pic, size);
      if (obj == NULL)
	pic_panic(pic, "GC memory exhausted");
    }
  }
  obj->u.basic.gc_mark = WHITE;
  obj->u.basic.tt = type;

  return obj;
}

struct pic_object *
pic_obj_alloc(pic_state *pic, size_t size, int type)
{
  struct pic_object *obj;

  obj = pic_obj_alloc_unsafe(pic, size, type);

  gc_protect(pic, obj);
  return obj;
}
