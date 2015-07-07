/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

union header {
  struct {
    union header *ptr;
    size_t size;
    char mark;
  } s;
  long alignment[2];
};

struct heap_page {
  union header *basep, *endp;
  struct heap_page *next;
};

struct pic_heap {
  union header base, *freep;
  struct heap_page *pages;
};


static void
heap_init(struct pic_heap *heap)
{
  heap->base.s.ptr = &heap->base;
  heap->base.s.size = 0; /* not 1, since it must never be used for allocation */
  heap->base.s.mark = PIC_GC_UNMARK;

  heap->freep = &heap->base;
  heap->pages = NULL;

#if GC_DEBUG
  printf("freep = %p\n", (void *)heap->freep);
#endif
}

struct pic_heap *
pic_heap_open(pic_state *pic)
{
  struct pic_heap *heap;

  heap = pic_calloc(pic, 1, sizeof(struct pic_heap));
  heap_init(heap);
  return heap;
}

void
pic_heap_close(pic_state *pic, struct pic_heap *heap)
{
  struct heap_page *page;

  while (heap->pages) {
    page = heap->pages;
    heap->pages = heap->pages->next;
    pic_free(pic, page);
  }
}

static void gc_free(pic_state *, union header *);

static void
add_heap_page(pic_state *pic)
{
  union header *up, *np;
  struct heap_page *page;
  size_t nu;

#if GC_DEBUG
  puts("adding heap page!");
#endif

  nu = (PIC_HEAP_PAGE_SIZE + sizeof(union header) - 1) / sizeof(union header) + 1;

  up = pic_calloc(pic, 1 + nu + 1, sizeof(union header));
  up->s.size = nu + 1;
  up->s.mark = PIC_GC_UNMARK;
  gc_free(pic, up);

  np = up + 1;
  np->s.size = nu;
  np->s.ptr = up->s.ptr;
  up->s.size = 1;
  up->s.ptr = np;

  page = pic_malloc(pic, sizeof(struct heap_page));
  page->basep = up;
  page->endp = up + nu + 1;
  page->next = pic->heap->pages;

  pic->heap->pages = page;
}

#if PIC_ENABLE_LIBC
void *
pic_default_allocf(void *ptr, size_t size)
{
  if (size == 0) {
    if (ptr) {
      free(ptr);
    }
    return NULL;
  }
  if (ptr) {
    return realloc(ptr, size);
  } else {
    return malloc(size);
  }
}
#endif

void *
pic_malloc(pic_state *pic, size_t size)
{
  void *ptr;

  ptr = pic->allocf(NULL, size);
  if (ptr == NULL && size > 0) {
    pic_panic(pic, "memory exhausted");
  }
  return ptr;
}

void *
pic_realloc(pic_state *pic, void *ptr, size_t size)
{
  ptr = pic->allocf(ptr, size);
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
  ptr = pic->allocf(NULL, size);
  if (ptr == NULL && size > 0) {
    pic_panic(pic, "memory exhausted");
  }
  memset(ptr, 0, size);
  return ptr;
}

void
pic_free(pic_state *pic, void *ptr)
{
  pic->allocf(ptr, 0);
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
pic_gc_protect(pic_state *pic, pic_value v)
{
  struct pic_object *obj;

  if (pic_vtype(v) != PIC_VTYPE_HEAP) {
    return v;
  }
  obj = pic_obj_ptr(v);

  gc_protect(pic, obj);

  return v;
}

size_t
pic_gc_arena_preserve(pic_state *pic)
{
  return pic->arena_idx;
}

void
pic_gc_arena_restore(pic_state *pic, size_t state)
{
  pic->arena_idx = state;
}

static void *
gc_alloc(pic_state *pic, size_t size)
{
  union header *freep, *p, *prevp;
  size_t nunits;

#if GC_DEBUG
  assert(size > 0);
#endif

  nunits = (size + sizeof(union header) - 1) / sizeof(union header) + 1;

  prevp = freep = pic->heap->freep;
  for (p = prevp->s.ptr; ; prevp = p, p = p->s.ptr) {
    if (p->s.size >= nunits)
      break;
    if (p == freep) {
      return NULL;
    }
  }

#if GC_DEBUG
  {
    unsigned char *c;
    size_t s, i, j;
    if (p->s.size == nunits) {
      c = (unsigned char *)(p + p->s.size - nunits + 1);
      s = nunits - 1;
    } else {
      c = (unsigned char *)(p + p->s.size - nunits);
      s = nunits;
    }

    for (i = 0; i < s; ++i) {
      for (j = 0; j < sizeof(union header); ++j) {
	assert(c[i * sizeof(union header) + j] == 0xAA);
      }
    }
  }
#endif

  if (p->s.size == nunits) {
    prevp->s.ptr = p->s.ptr;
  }
  else {
    p->s.size -= nunits;
    p += p->s.size;
    p->s.size = nunits;
  }
  pic->heap->freep = prevp;

  p->s.mark = PIC_GC_UNMARK;

#if GC_DEBUG
  memset(p+1, 0, sizeof(union header) * (nunits - 1));
  p->s.ptr = (union header *)0xcafebabe;
#endif

  return (void *)(p + 1);
}

static void
gc_free(pic_state *pic, union header *bp)
{
  union header *freep, *p;

#if GC_DEBUG
  assert(bp != NULL);
  assert(bp->s.size > 1);
#endif

#if GC_DEBUG
  memset(bp + 1, 0xAA, (bp->s.size - 1) * sizeof(union header));
#endif

  freep = pic->heap->freep;
  for (p = freep; ! (bp > p && bp < p->s.ptr); p = p->s.ptr) {
    if (p >= p->s.ptr && (bp > p || bp < p->s.ptr)) {
      break;
    }
  }
  if (bp + bp->s.size == p->s.ptr) {
    bp->s.size += p->s.ptr->s.size;
    bp->s.ptr = p->s.ptr->s.ptr;

#if GC_DEBUG
    memset(p->s.ptr, 0xAA, sizeof(union header));
#endif
  }
  else {
    bp->s.ptr = p->s.ptr;
  }
  if (p + p->s.size == bp && p->s.size > 1) {
    p->s.size += bp->s.size;
    p->s.ptr = bp->s.ptr;

#if GC_DEBUG
    memset(bp, 0xAA, sizeof(union header));
#endif
  }
  else {
    p->s.ptr = bp;
  }
  pic->heap->freep = p;
}

static void gc_mark(pic_state *, pic_value);
static void gc_mark_object(pic_state *pic, struct pic_object *obj);

static bool
gc_is_marked(union header *p)
{
  return p->s.mark == PIC_GC_MARK;
}

static bool
gc_obj_is_marked(struct pic_object *obj)
{
  union header *p;

  p = ((union header *)obj) - 1;

  return gc_is_marked(p);
}

static bool
gc_value_need_mark(pic_value value)
{
  return pic_obj_p(value) && (! gc_obj_is_marked(pic_obj_ptr(value)));
}

static void
gc_unmark(union header *p)
{
  p->s.mark = PIC_GC_UNMARK;
}

static void
gc_mark_object(pic_state *pic, struct pic_object *obj)
{
  union header *p;

  p = ((union header *)obj) - 1;

  if (gc_is_marked(p))
    return;
  p->s.mark = PIC_GC_MARK;

  switch (obj->tt) {
  case PIC_TT_PAIR: {
    gc_mark(pic, ((struct pic_pair *)obj)->car);
    gc_mark(pic, ((struct pic_pair *)obj)->cdr);
    break;
  }
  case PIC_TT_CXT: {
    struct pic_context *cxt = (struct pic_context *)obj;
    int i;

    for (i = 0; i < cxt->regc; ++i) {
      gc_mark(pic, cxt->regs[i]);
    }
    if (cxt->up) {
      gc_mark_object(pic, (struct pic_object *)cxt->up);
    }
    break;
  }
  case PIC_TT_PROC: {
    struct pic_proc *proc = (struct pic_proc *)obj;
    if (pic_proc_irep_p(proc)) {
      gc_mark_object(pic, (struct pic_object *)proc->u.i.irep);
      if (proc->u.i.cxt) {
        gc_mark_object(pic, (struct pic_object *)proc->u.i.cxt);
      }
    } else {
      if (proc->u.f.env) {
        gc_mark_object(pic, (struct pic_object *)proc->u.f.env);
      }
    }
    break;
  }
  case PIC_TT_PORT: {
    break;
  }
  case PIC_TT_ERROR: {
    struct pic_error *err = (struct pic_error *)obj;
    gc_mark_object(pic, (struct pic_object *)err->type);
    gc_mark_object(pic, (struct pic_object *)err->msg);
    gc_mark(pic, err->irrs);
    gc_mark_object(pic, (struct pic_object *)err->stack);
    break;
  }
  case PIC_TT_STRING: {
    break;
  }
  case PIC_TT_VECTOR: {
    size_t i;
    for (i = 0; i < ((struct pic_vector *)obj)->len; ++i) {
      gc_mark(pic, ((struct pic_vector *)obj)->data[i]);
    }
    break;
  }
  case PIC_TT_BLOB: {
    break;
  }
  case PIC_TT_ID: {
    struct pic_id *id = (struct pic_id *)obj;
    gc_mark(pic, id->var);
    gc_mark_object(pic, (struct pic_object *)id->env);
    break;
  }
  case PIC_TT_ENV: {
    struct pic_env *env = (struct pic_env *)obj;
    khash_t(env) *h = &env->map;
    khiter_t it;

    if (env->up) {
      gc_mark_object(pic, (struct pic_object *)env->up);
    }
    for (it = kh_begin(h); it != kh_end(h); ++it) {
      if (kh_exist(h, it)) {
        gc_mark_object(pic, kh_key(h, it));
        gc_mark_object(pic, (struct pic_object *)kh_val(h, it));
      }
    }
    break;
  }
  case PIC_TT_LIB: {
    struct pic_lib *lib = (struct pic_lib *)obj;
    gc_mark(pic, lib->name);
    gc_mark_object(pic, (struct pic_object *)lib->env);
    gc_mark_object(pic, (struct pic_object *)lib->exports);
    break;
  }
  case PIC_TT_IREP: {
    struct pic_irep *irep = (struct pic_irep *)obj;
    size_t i;

    for (i = 0; i < irep->ilen; ++i) {
      gc_mark_object(pic, (struct pic_object *)irep->irep[i]);
    }
    for (i = 0; i < irep->plen; ++i) {
      gc_mark(pic, irep->pool[i]);
    }
    break;
  }
  case PIC_TT_DATA: {
    struct pic_data *data = (struct pic_data *)obj;

    gc_mark_object(pic, (struct pic_object *)data->storage);
    if (data->type->mark) {
      data->type->mark(pic, data->data, gc_mark);
    }
    break;
  }
  case PIC_TT_DICT: {
    struct pic_dict *dict = (struct pic_dict *)obj;
    pic_sym *sym;
    khiter_t it;

    pic_dict_for_each (sym, dict, it) {
      gc_mark_object(pic, (struct pic_object *)sym);
      gc_mark(pic, pic_dict_ref(pic, dict, sym));
    }
    break;
  }
  case PIC_TT_RECORD: {
    struct pic_record *rec = (struct pic_record *)obj;

    gc_mark_object(pic, (struct pic_object *)rec->data);
    break;
  }
  case PIC_TT_SYMBOL: {
    break;
  }
  case PIC_TT_REG: {
    struct pic_reg *reg = (struct pic_reg *)obj;

    reg->prev = pic->regs;
    pic->regs = reg;
    break;
  }
  case PIC_TT_CP: {
    struct pic_checkpoint *cp = (struct pic_checkpoint *)obj;

    if (cp->prev) {
      gc_mark_object(pic, (struct pic_object *)cp->prev);
    }
    if (cp->in) {
      gc_mark_object(pic, (struct pic_object *)cp->in);
    }
    if (cp->out) {
      gc_mark_object(pic, (struct pic_object *)cp->out);
    }
    break;
  }
  case PIC_TT_BOX: {
    struct pic_box *box = (struct pic_box *)obj;

    gc_mark(pic, box->value);
    break;
  }
  case PIC_TT_NIL:
  case PIC_TT_BOOL:
#if PIC_ENABLE_FLOAT
  case PIC_TT_FLOAT:
#endif
  case PIC_TT_INT:
  case PIC_TT_CHAR:
  case PIC_TT_EOF:
  case PIC_TT_UNDEF:
  case PIC_TT_INVALID:
    pic_panic(pic, "logic flaw");
  }
}

static void
gc_mark(pic_state *pic, pic_value v)
{
  struct pic_object *obj;

  if (pic_vtype(v) != PIC_VTYPE_HEAP)
    return;
  obj = pic_obj_ptr(v);

  gc_mark_object(pic, obj);
}

#define M(x) gc_mark_object(pic, (struct pic_object *)pic->x)

static void
gc_mark_global_symbols(pic_state *pic)
{
  M(sQUOTE); M(sQUASIQUOTE); M(sUNQUOTE); M(sUNQUOTE_SPLICING);
  M(sSYNTAX_QUOTE); M(sSYNTAX_QUASIQUOTE); M(sSYNTAX_UNQUOTE); M(sSYNTAX_UNQUOTE_SPLICING);
  M(sDEFINE_LIBRARY); M(sIMPORT); M(sEXPORT); M(sCOND_EXPAND);
  M(sCALL); M(sGREF); M(sLREF); M(sCREF);

  M(uDEFINE); M(uLAMBDA); M(uIF); M(uBEGIN); M(uQUOTE); M(uSETBANG); M(uDEFINE_MACRO);
  M(uDEFINE_LIBRARY); M(uIMPORT); M(uEXPORT); M(uCOND_EXPAND);
  M(uCONS); M(uCAR); M(uCDR); M(uNILP); M(uSYMBOLP); M(uPAIRP);
  M(uADD); M(uSUB); M(uMUL); M(uDIV); M(uEQ); M(uLT); M(uLE); M(uGT); M(uGE); M(uNOT);
}

#define P(x) gc_mark(pic, pic->x)

static void
gc_mark_system_procedures(pic_state *pic)
{
  P(pCONS);
  P(pCAR);
  P(pCDR);
  P(pNILP);
  P(pSYMBOLP);
  P(pPAIRP);
  P(pNOT);
  P(pADD);
  P(pSUB);
  P(pMUL);
  P(pDIV);
  P(pEQ);
  P(pLT);
  P(pLE);
  P(pGT);
  P(pGE);
}

static void
gc_mark_phase(pic_state *pic)
{
  pic_value *stack;
  pic_callinfo *ci;
  struct pic_proc **xhandler;
  size_t j;

  assert(pic->regs == NULL);

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
    gc_mark_object(pic, pic->arena[j]);
  }

  /* mark reserved symbols */
  gc_mark_global_symbols(pic);

  /* mark system procedures */
  gc_mark_system_procedures(pic);

  /* global variables */
  if (pic->globals) {
    gc_mark_object(pic, (struct pic_object *)pic->globals);
  }

  /* macro objects */
  if (pic->macros) {
    gc_mark_object(pic, (struct pic_object *)pic->macros);
  }

  /* attribute table */
  if (pic->attrs) {
    gc_mark_object(pic, (struct pic_object *)pic->attrs);
  }

  /* error object */
  gc_mark(pic, pic->err);

  /* features */
  gc_mark(pic, pic->features);

  /* library table */
  gc_mark(pic, pic->libs);

  /* parameter table */
  gc_mark(pic, pic->ptable);

  /* registries */
  do {
    struct pic_object *key;
    pic_value val;
    khiter_t it;
    khash_t(reg) *h;
    struct pic_reg *reg;

    j = 0;
    reg = pic->regs;

    while (reg != NULL) {
      h = &reg->hash;
      for (it = kh_begin(h); it != kh_end(h); ++it) {
        if (! kh_exist(h, it))
          continue;
        key = kh_key(h, it);
        val = kh_val(h, it);
        if (gc_obj_is_marked(key) && gc_value_need_mark(val)) {
          gc_mark(pic, val);
          ++j;
        }
      }
      reg = reg->prev;
    }
  } while (j > 0);
}

static void
gc_finalize_object(pic_state *pic, struct pic_object *obj)
{
#if GC_DEBUG
  printf("* finalizing object: %s", pic_type_repr(pic_type(pic_obj_value(obj))));
  //  pic_debug(pic, pic_obj_value(obj));
  puts("");
#endif

  switch (obj->tt) {
  case PIC_TT_PAIR: {
    break;
  }
  case PIC_TT_CXT: {
    break;
  }
  case PIC_TT_PROC: {
    break;
  }
  case PIC_TT_VECTOR: {
    pic_free(pic, ((struct pic_vector *)obj)->data);
    break;
  }
  case PIC_TT_BLOB: {
    pic_free(pic, ((struct pic_blob *)obj)->data);
    break;
  }
  case PIC_TT_STRING: {
    pic_rope_decref(pic, ((struct pic_string *)obj)->rope);
    break;
  }
  case PIC_TT_PORT: {
    break;
  }
  case PIC_TT_ERROR: {
    break;
  }
  case PIC_TT_ID: {
    break;
  }
  case PIC_TT_ENV: {
    struct pic_env *env = (struct pic_env *)obj;
    kh_destroy(env, &env->map);
    break;
  }
  case PIC_TT_LIB: {
    break;
  }
  case PIC_TT_IREP: {
    struct pic_irep *irep = (struct pic_irep *)obj;
    pic_free(pic, irep->code);
    pic_free(pic, irep->irep);
    pic_free(pic, irep->pool);
    break;
  }
  case PIC_TT_DATA: {
    struct pic_data *data = (struct pic_data *)obj;
    if (data->type->dtor) {
      data->type->dtor(pic, data->data);
    }
    break;
  }
  case PIC_TT_DICT: {
    struct pic_dict *dict = (struct pic_dict *)obj;
    kh_destroy(dict, &dict->hash);
    break;
  }
  case PIC_TT_RECORD: {
    break;
  }
  case PIC_TT_SYMBOL: {
    pic_sym *sym = (pic_sym *)obj;
    pic_free(pic, (void *)sym->cstr);
    break;
  }
  case PIC_TT_REG: {
    struct pic_reg *reg = (struct pic_reg *)obj;
    kh_destroy(reg, &reg->hash);
    break;
  }
  case PIC_TT_CP: {
    break;
  }
  case PIC_TT_BOX: {
    break;
  }
  case PIC_TT_NIL:
  case PIC_TT_BOOL:
#if PIC_ENABLE_FLOAT
  case PIC_TT_FLOAT:
#endif
  case PIC_TT_INT:
  case PIC_TT_CHAR:
  case PIC_TT_EOF:
  case PIC_TT_UNDEF:
  case PIC_TT_INVALID:
    pic_panic(pic, "logic flaw");
  }
}

static void
gc_sweep_symbols(pic_state *pic)
{
  khash_t(s) *h = &pic->syms;
  khiter_t it;
  pic_sym *sym;

  for (it = kh_begin(h); it != kh_end(h); ++it) {
    if (! kh_exist(h, it))
      continue;
    sym = kh_val(h, it);
    if (! gc_obj_is_marked((struct pic_object *)sym)) {
      kh_del(s, h, it);
    }
  }
}

static void
gc_sweep_page(pic_state *pic, struct heap_page *page)
{
#if GC_DEBUG
  static union header * const NIL = (union header *)0xdeadbeef;
#else
  static union header * const NIL = NULL;
#endif
  union header *bp, *p, *s = NIL, *t = NIL;

#if GC_DEBUG
  int c = 0;
#endif

  for (bp = page->basep; ; bp = bp->s.ptr) {
    for (p = bp + bp->s.size; p != bp->s.ptr; p += p->s.size) {
      if (p == page->endp) {
	goto escape;
      }
      if (! gc_is_marked(p)) {
	if (s == NIL) {
	  s = p;
	}
	else {
	  t->s.ptr = p;
	}
	t = p;
	t->s.ptr = NIL; /* For dead objects we can safely reuse ptr field */
      }
      gc_unmark(p);
    }
  }
 escape:

  /* free! */
  while (s != NIL) {
    t = s->s.ptr;
    gc_finalize_object(pic, (struct pic_object *)(s + 1));
    gc_free(pic, s);
    s = t;

#if GC_DEBUG
    c++;
#endif
  }

#if GC_DEBUG
  printf("freed objects count: %d\n", c);
#endif
}

static void
gc_sweep_phase(pic_state *pic)
{
  struct heap_page *page = pic->heap->pages;
  khiter_t it;
  khash_t(reg) *h;

  /* registries */
  while (pic->regs != NULL) {
    h = &pic->regs->hash;
    for (it = kh_begin(h); it != kh_end(h); ++it) {
      if (! kh_exist(h, it))
        continue;
      if (! gc_obj_is_marked(kh_key(h, it))) {
        kh_del(reg, h, it);
      }
    }
    pic->regs = pic->regs->prev;
  }

  gc_sweep_symbols(pic);

  while (page) {
    gc_sweep_page(pic, page);
    page = page->next;
  }
}

void
pic_gc_run(pic_state *pic)
{
#if GC_DEBUG
  struct heap_page *page;
#endif

  if (! pic->gc_enable) {
    return;
  }

#if DEBUG
  puts("gc run!");
#endif

  gc_mark_phase(pic);
  gc_sweep_phase(pic);

#if GC_DEBUG
  for (page = pic->heap->pages; page; page = page->next) {
    union header *bp, *p;
    unsigned char *c;

    for (bp = page->basep; ; bp = bp->s.ptr) {
      for (c = (unsigned char *)(bp+1); c != (unsigned char *)(bp + bp->s.size); ++c) {
	assert(*c == 0xAA);
      }
      for (p = bp + bp->s.size; p != bp->s.ptr; p += p->s.size) {
	if (p == page->endp) {
	  /* if (page->next) */
	  /*   assert(bp->s.ptr == page->next->basep); */
	  /* else */
	  /*   assert(bp->s.ptr == &pic->heap->base); */
	  goto escape;
	}
	assert(! gc_is_marked(p));
      }
    }
  escape:
    ((void)0);
  }

  puts("not error on heap found! gc successfully finished");
#endif
}

struct pic_object *
pic_obj_alloc_unsafe(pic_state *pic, size_t size, enum pic_tt tt)
{
  struct pic_object *obj;

#if GC_DEBUG
  printf("*allocating: %s\n", pic_type_repr(tt));
#endif

#if GC_STRESS
  pic_gc_run(pic);
#endif

  obj = (struct pic_object *)gc_alloc(pic, size);
  if (obj == NULL) {
    pic_gc_run(pic);
    obj = (struct pic_object *)gc_alloc(pic, size);
    if (obj == NULL) {
      add_heap_page(pic);
      obj = (struct pic_object *)gc_alloc(pic, size);
      if (obj == NULL)
	pic_panic(pic, "GC memory exhausted");
    }
  }
  obj->tt = tt;

  return obj;
}

struct pic_object *
pic_obj_alloc(pic_state *pic, size_t size, enum pic_tt tt)
{
  struct pic_object *obj;

  obj = pic_obj_alloc_unsafe(pic, size, tt);

  gc_protect(pic, obj);
  return obj;
}
