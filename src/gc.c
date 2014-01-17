/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"
#include "picrin/irep.h"
#include "picrin/proc.h"
#include "picrin/port.h"
#include "picrin/blob.h"
#include "picrin/cont.h"
#include "picrin/error.h"
#include "picrin/macro.h"
#include "picrin/lib.h"
#include "picrin/var.h"
#include "xhash/xhash.h"

#if GC_DEBUG
# include <string.h>
# include <stdio.h>
# include <assert.h>
#endif

void
init_heap(struct pic_heap *heap)
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

void
finalize_heap(struct pic_heap *heap)
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

void *
pic_alloc(pic_state *pic, size_t size)
{
  void *ptr;

  ptr = malloc(size);
  if (ptr == NULL) {
    pic_abort(pic, "memory exhausted");
  }
  return ptr;
}

void *
pic_realloc(pic_state *pic, void *ptr, size_t size)
{
  ptr = realloc(ptr, size);
  if (ptr == NULL) {
    pic_abort(pic, "memory exhausted");
  }
  return ptr;
}

void *
pic_calloc(pic_state *pic, size_t count, size_t size)
{
  void *ptr;

  ptr = calloc(count ,size);
  if (ptr == NULL) {
    pic_abort(pic, "memory exhausted");
  }
  return ptr;
}

void
pic_free(pic_state *pic, void *ptr)
{
  free(ptr);
}

static void
gc_protect(pic_state *pic, struct pic_object *obj)
{
  if (pic->arena_idx >= PIC_ARENA_SIZE) {
    pic_abort(pic, "arena overflow");
  }
  pic->arena[pic->arena_idx++] = obj;
}

void
pic_gc_protect(pic_state *pic, pic_value v)
{
  struct pic_object *obj;

  if (pic_vtype(v) != PIC_VTYPE_HEAP) {
    return;
  }
  obj = pic_obj_ptr(v);

  gc_protect(pic, obj);
}

int
pic_gc_arena_preserve(pic_state *pic)
{
  return pic->arena_idx;
}

void
pic_gc_arena_restore(pic_state *pic, int state)
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
    int i, j;
    unsigned char *c;
    size_t s;
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

static void
gc_mark_block(pic_state *pic, struct pic_block *blk)
{
  while (blk) {
    if (blk->in)
      gc_mark_object(pic, (struct pic_object *)blk->in);
    if (blk->out)
      gc_mark_object(pic, (struct pic_object *)blk->out);
    blk = blk->prev;
  }
}

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

    for (i = 0; i < env->valuec; ++i) {
      gc_mark(pic, env->values[i]);
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
    break;
  }
  case PIC_TT_PORT: {
    break;
  }
  case PIC_TT_ERROR: {
    gc_mark(pic, ((struct pic_error *)obj)->irrs);
    break;
  }
  case PIC_TT_STRING: {
    break;
  }
  case PIC_TT_VECTOR: {
    int i;
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
    int i;

    /* block */
    gc_mark_block(pic, cont->blk);

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

    /* exception handlers */
    for (i = 0; i < cont->ridx; ++i) {
      gc_mark_object(pic, (struct pic_object *)cont->rescue[i]);
    }

    /* arena */
    for (i = 0; i < cont->arena_idx; ++i) {
      gc_mark_object(pic, cont->arena[i]);
    }

    gc_mark(pic, cont->result);
    break;
  }
  case PIC_TT_SYNTAX: {
    struct pic_syntax *stx = (struct pic_syntax *)obj;

    if (stx->macro) {
      gc_mark_object(pic, (struct pic_object *)stx->macro);
    }
    if (stx->senv) {
      gc_mark_object(pic, (struct pic_object *)stx->senv);
    }
    break;
  }
  case PIC_TT_SENV: {
    struct pic_senv *senv = (struct pic_senv *)obj;

    if (senv->up) {
      gc_mark_object(pic, (struct pic_object *)senv->up);
    }
    if (senv->stx) {
      int i;

      for (i = 0; i < senv->xlen; ++i) {
	gc_mark_object(pic, (struct pic_object *)senv->stx[i]);
      }
    }
    break;
  }
  case PIC_TT_SC: {
    struct pic_sc *sc = (struct pic_sc *)obj;
    gc_mark(pic, sc->expr);
    gc_mark_object(pic, (struct pic_object *)sc->senv);
    break;
  }
  case PIC_TT_LIB: {
    struct pic_lib *lib = (struct pic_lib *)obj;
    gc_mark(pic, lib->name);
    gc_mark_object(pic, (struct pic_object *)lib->senv);
    break;
  }
  case PIC_TT_VAR: {
    struct pic_var *var = (struct pic_var *)obj;
    gc_mark(pic, var->value);
    if (var->conv) {
      gc_mark_object(pic, (struct pic_object *)var->conv);
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
gc_mark_phase(pic_state *pic)
{
  pic_value *stack;
  pic_callinfo *ci;
  int i;

  /* block */
  gc_mark_block(pic, pic->blk);

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

  /* exception handlers */
  for (i = 0; i < pic->ridx; ++i) {
    gc_mark_object(pic, (struct pic_object *)pic->rescue[i]);
  }

  /* arena */
  for (i = 0; i < pic->arena_idx; ++i) {
    gc_mark_object(pic, pic->arena[i]);
  }

  /* globals */
  for (i = 0; i < pic->glen; ++i) {
    gc_mark(pic, pic->globals[i]);
  }

  /* pool */
  for (i = 0; i < pic->plen; ++i) {
    gc_mark(pic, pic->pool[i]);
  }

  /* library table */
  gc_mark(pic, pic->lib_tbl);
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
    pic_free(pic, ((struct pic_env *)obj)->values);
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
    pic_free(pic, (void*)((struct pic_string *)obj)->str);
    break;
  }
  case PIC_TT_PORT: {
    break;
  }
  case PIC_TT_ERROR: {
    pic_free(pic, ((struct pic_error *)obj)->msg);
    break;
  }
  case PIC_TT_CONT: {
    struct pic_cont *cont = (struct pic_cont *)obj;
    pic_free(pic, cont->stk_ptr);
    pic_free(pic, cont->st_ptr);
    pic_free(pic, cont->ci_ptr);
    pic_free(pic, cont->rescue);
    PIC_BLK_DECREF(pic, cont->blk);
    break;
  }
  case PIC_TT_SENV: {
    struct pic_senv *senv = (struct pic_senv *)obj;
    xh_destory(senv->tbl);
    if (senv->stx)
      pic_free(pic, senv->stx);
    break;
  }
  case PIC_TT_SYNTAX: {
    break;
  }
  case PIC_TT_SC: {
    break;
  }
  case PIC_TT_LIB: {
    struct pic_lib *lib = (struct pic_lib *)obj;
    xh_destory(lib->exports);
    break;
  }
  case PIC_TT_VAR: {
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
