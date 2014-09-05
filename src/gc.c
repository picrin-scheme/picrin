/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/vector.h"
#include "picrin/irep.h"
#include "picrin/proc.h"
#include "picrin/port.h"
#include "picrin/blob.h"
#include "picrin/cont.h"
#include "picrin/error.h"
#include "picrin/macro.h"
#include "picrin/lib.h"
#include "picrin/var.h"
#include "picrin/data.h"
#include "picrin/dict.h"
#include "picrin/record.h"
#include "picrin/read.h"

#if GC_DEBUG
# include <string.h>
#endif

union header {
  struct {
    union header *ptr;
    size_t size;
    unsigned int mark : 1;
  } s;
  long alignment[4];
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
pic_heap_open()
{
  struct pic_heap *heap;

  heap = (struct pic_heap *)calloc(1, sizeof(struct pic_heap));
  heap_init(heap);
  return heap;
}

void
pic_heap_close(struct pic_heap *heap)
{
  struct heap_page *page;

  while (heap->pages) {
    page = heap->pages;
    heap->pages = heap->pages->next;
    free(page);
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

  up = (union header *)pic_calloc(pic, 1 + nu + 1, sizeof(union header));
  up->s.size = nu + 1;
  up->s.mark = PIC_GC_UNMARK;
  gc_free(pic, up);

  np = up + 1;
  np->s.size = nu;
  np->s.ptr = up->s.ptr;
  up->s.size = 1;
  up->s.ptr = np;

  page = (struct heap_page *)pic_alloc(pic, sizeof(struct heap_page));
  page->basep = up;
  page->endp = up + nu + 1;
  page->next = pic->heap->pages;

  pic->heap->pages = page;
}

static void *
alloc(void *ptr, size_t size)
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

void *
pic_alloc(pic_state *pic, size_t size)
{
  void *ptr;

  ptr = alloc(NULL, size);
  if (ptr == NULL && size > 0) {
    pic_abort(pic, "memory exhausted");
  }
  return ptr;
}

void *
pic_realloc(pic_state *pic, void *ptr, size_t size)
{
  ptr = alloc(ptr, size);
  if (ptr == NULL && size > 0) {
    pic_abort(pic, "memory exhausted");
  }
  return ptr;
}

void *
pic_calloc(pic_state *pic, size_t count, size_t size)
{
  void *ptr;

  size *= count;
  ptr = alloc(NULL, size);
  if (ptr == NULL && size > 0) {
    pic_abort(pic, "memory exhausted");
  }
  memset(ptr, 0, size);
  return ptr;
}

void
pic_free(pic_state *pic, void *ptr)
{
  UNUSED(pic);

  free(ptr);
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
  case PIC_TT_ENV: {
    struct pic_env *env = (struct pic_env *)obj;
    int i;

    for (i = 0; i < env->regc; ++i) {
      gc_mark(pic, env->regs[i]);
    }
    if (env->up) {
      gc_mark_object(pic, (struct pic_object *)env->up);
    }
    break;
  }
  case PIC_TT_PROC: {
    struct pic_proc *proc = (struct pic_proc *)obj;
    if (proc->env) {
      gc_mark_object(pic, (struct pic_object *)proc->env);
    }
    if (proc->attr) {
      gc_mark_object(pic, (struct pic_object *)proc->attr);
    }
    if (pic_proc_irep_p(proc)) {
      gc_mark_object(pic, (struct pic_object *)proc->u.irep);
    }
    break;
  }
  case PIC_TT_PORT: {
    break;
  }
  case PIC_TT_ERROR: {
    struct pic_error *err = (struct pic_error *)obj;
    gc_mark_object(pic,(struct pic_object *)err->msg);
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
  case PIC_TT_CONT: {
    struct pic_cont *cont = (struct pic_cont *)obj;
    pic_value *stack;
    pic_callinfo *ci;
    size_t i;

    /* block */
    gc_mark_object(pic, (struct pic_object *)cont->blk);

    /* stack */
    for (stack = cont->st_ptr; stack != cont->st_ptr + cont->sp_offset; ++stack) {
      gc_mark(pic, *stack);
    }

    /* callinfo */
    for (ci = cont->ci_ptr + cont->ci_offset; ci != cont->ci_ptr; --ci) {
      if (ci->env) {
	gc_mark_object(pic, (struct pic_object *)ci->env);
      }
    }

    /* arena */
    for (i = 0; i < (size_t)cont->arena_idx; ++i) {
      gc_mark_object(pic, cont->arena[i]);
    }

    /* error handlers */
    for (i = 0; i < cont->try_jmp_idx; ++i) {
      if (cont->try_jmps[i].handler) {
        gc_mark_object(pic, (struct pic_object *)cont->try_jmps[i].handler);
      }
    }

    /* result values */
    gc_mark(pic, cont->results);
    break;
  }
  case PIC_TT_MACRO: {
    struct pic_macro *mac = (struct pic_macro *)obj;

    if (mac->proc) {
      gc_mark_object(pic, (struct pic_object *)mac->proc);
    }
    if (mac->senv) {
      gc_mark_object(pic, (struct pic_object *)mac->senv);
    }
    break;
  }
  case PIC_TT_SENV: {
    struct pic_senv *senv = (struct pic_senv *)obj;

    if (senv->up) {
      gc_mark_object(pic, (struct pic_object *)senv->up);
    }
    break;
  }
  case PIC_TT_LIB: {
    struct pic_lib *lib = (struct pic_lib *)obj;
    gc_mark(pic, lib->name);
    gc_mark_object(pic, (struct pic_object *)lib->env);
    break;
  }
  case PIC_TT_VAR: {
    struct pic_var *var = (struct pic_var *)obj;
    gc_mark(pic, var->stack);
    if (var->conv) {
      gc_mark_object(pic, (struct pic_object *)var->conv);
    }
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
    xh_iter it;

    xh_begin(&it, &data->storage);
    while (xh_next(&it)) {
      gc_mark(pic, xh_val(it.e, pic_value));
    }
    break;
  }
  case PIC_TT_DICT: {
    struct pic_dict *dict = (struct pic_dict *)obj;
    xh_iter it;

    xh_begin(&it, &dict->hash);
    while (xh_next(&it)) {
      gc_mark(pic, xh_val(it.e, pic_value));
    }
    break;
  }
  case PIC_TT_RECORD: {
    struct pic_record *rec = (struct pic_record *)obj;
    xh_iter it;

    xh_begin(&it, &rec->hash);
    while (xh_next(&it)) {
      gc_mark(pic, xh_val(it.e, pic_value));
    }
    break;
  }
  case PIC_TT_BLK: {
    struct pic_block *blk = (struct pic_block *)obj;

    if (blk->prev) {
      gc_mark_object(pic, (struct pic_object *)blk->prev);
    }
    if (blk->in) {
      gc_mark_object(pic, (struct pic_object *)blk->in);
    }
    if (blk->out) {
      gc_mark_object(pic, (struct pic_object *)blk->out);
    }
    break;
  }
  case PIC_TT_NIL:
  case PIC_TT_BOOL:
  case PIC_TT_FLOAT:
  case PIC_TT_INT:
  case PIC_TT_SYMBOL:
  case PIC_TT_CHAR:
  case PIC_TT_EOF:
  case PIC_TT_UNDEF:
    pic_abort(pic, "logic flaw");
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

static void
gc_mark_trie(pic_state *pic, struct pic_trie *trie)
{
  size_t i;

  for (i = 0; i < sizeof trie->table / sizeof(struct pic_trie *); ++i) {
    if (trie->table[i] != NULL) {
      gc_mark_trie(pic, trie->table[i]);
    }
  }
  /* if (trie->proc != NULL) { */
  /*   gc_mark_object(pic, (struct pic_object *)trie->proc); */
  /* } */
}

static void
gc_mark_phase(pic_state *pic)
{
  pic_value *stack;
  pic_callinfo *ci;
  size_t i, j;
  xh_iter it;

  /* block */
  if (pic->blk) {
    gc_mark_object(pic, (struct pic_object *)pic->blk);
  }

  /* stack */
  for (stack = pic->stbase; stack != pic->sp; ++stack) {
    gc_mark(pic, *stack);
  }

  /* callinfo */
  for (ci = pic->ci; ci != pic->cibase; --ci) {
    if (ci->env) {
      gc_mark_object(pic, (struct pic_object *)ci->env);
    }
  }

  /* error object */
  if (pic->err) {
    gc_mark_object(pic, (struct pic_object *)pic->err);
  }

  /* arena */
  for (j = 0; j < pic->arena_idx; ++j) {
    gc_mark_object(pic, pic->arena[j]);
  }

  /* global variables */
  xh_begin(&it, &pic->globals);
  while (xh_next(&it)) {
    gc_mark(pic, xh_val(it.e, pic_value));
  }

  /* macro objects */
  xh_begin(&it, &pic->macros);
  while (xh_next(&it)) {
    gc_mark_object(pic, xh_val(it.e, struct pic_object *));
  }

  /* error handlers */
  for (i = 0; i < pic->try_jmp_idx; ++i) {
    if (pic->try_jmps[i].handler) {
      gc_mark_object(pic, (struct pic_object *)pic->try_jmps[i].handler);
    }
  }

  /* readers */
  gc_mark_trie(pic, pic->reader->trie);

  /* library table */
  gc_mark(pic, pic->libs);
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
  case PIC_TT_ENV: {
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
    XROPE_DECREF(((struct pic_string *)obj)->rope);
    break;
  }
  case PIC_TT_PORT: {
    break;
  }
  case PIC_TT_ERROR: {
    break;
  }
  case PIC_TT_CONT: {
    struct pic_cont *cont = (struct pic_cont *)obj;
    pic_free(pic, cont->stk_ptr);
    pic_free(pic, cont->st_ptr);
    pic_free(pic, cont->ci_ptr);
    pic_free(pic, cont->arena);
    pic_free(pic, cont->try_jmps);
    break;
  }
  case PIC_TT_SENV: {
    struct pic_senv *senv = (struct pic_senv *)obj;
    xh_destroy(&senv->map);
    break;
  }
  case PIC_TT_MACRO: {
    break;
  }
  case PIC_TT_LIB: {
    struct pic_lib *lib = (struct pic_lib *)obj;
    xh_destroy(&lib->exports);
    break;
  }
  case PIC_TT_VAR: {
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
    data->type->dtor(pic, data->data);
    xh_destroy(&data->storage);
    break;
  }
  case PIC_TT_DICT: {
    struct pic_dict *dict = (struct pic_dict *)obj;
    xh_destroy(&dict->hash);
    break;
  }
  case PIC_TT_RECORD: {
    struct pic_record *rec = (struct pic_record *)obj;
    xh_destroy(&rec->hash);
    break;
  }
  case PIC_TT_BLK: {
    break;
  }
  case PIC_TT_NIL:
  case PIC_TT_BOOL:
  case PIC_TT_FLOAT:
  case PIC_TT_INT:
  case PIC_TT_SYMBOL:
  case PIC_TT_CHAR:
  case PIC_TT_EOF:
  case PIC_TT_UNDEF:
    pic_abort(pic, "logic flaw");
  }
}

static void
gc_sweep_page(pic_state *pic, struct heap_page *page)
{
#if GC_DEBUG
  static union header *NIL = (union header *)0xdeadbeef;
#else
  static union header *NIL = NULL;
#endif
  union header *bp, *p, *s = NIL, *t;

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
	pic_abort(pic, "GC memory exhausted");
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
