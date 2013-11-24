#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"
#include "picrin/irep.h"
#include "picrin/proc.h"
#include "picrin/port.h"
#include "picrin/blob.h"
#include "picrin/cont.h"
#include "picrin/error.h"

#if GC_DEBUG
# include <stdio.h>
#endif

void
init_heap(struct pic_heap *heap)
{
  heap->base.s.ptr = heap->freep = &heap->base;
  heap->base.s.size = 0;	/* not 1, since it must never be fused into other headers */
  heap->base.s.mark = PIC_GC_UNMARK;

#if GC_DEBUG
  printf("freep = %p\n", heap->freep);
#endif
}

static void gc_free(pic_state *, union header *);

static void *
add_heap_page(pic_state *pic)
{
  int nu;
  union header *p, *ep;

#if DEBUG
  puts("heap page added");
#endif

  nu = (PIC_HEAP_PAGE_SIZE + sizeof(union header) - 1) / sizeof(union header) + 1;
  p = (union header *)calloc(nu, sizeof(union header));
  p->s.size = nu - 1;
  p->s.mark = PIC_GC_UNMARK;
  gc_free(pic, p);

  ep = p + p->s.size;		/* end of page */
  ep->s.size = 0;
  ep->s.mark = PIC_GC_UNMARK;
  ep->s.ptr = p->s.ptr;
  p->s.ptr = ep;

  return pic->heap->freep;
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
pic_calloc(pic_state *pic, unsigned count, size_t size)
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

  nunits = (size + sizeof(union header) - 1) / sizeof(union header) + 1;

  prevp = freep = pic->heap->freep;
  for (p = prevp->s.ptr; ; prevp = p, p = p->s.ptr) {
    if (p->s.size >= nunits)
      break;
    if (p == freep) {
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

  p->s.mark = PIC_GC_UNMARK;
  return (void *)(p + 1);
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

  /* macros */
  for (i = 0; i < pic->mlen; ++i) {
    gc_mark_object(pic, (struct pic_object *)pic->macros[i]);
  }

  /* pool */
  for (i = 0; i < pic->plen; ++i) {
    gc_mark(pic, pic->pool[i]);
  }
}

static void
gc_finalize_object(pic_state *pic, struct pic_object *obj)
{
#if GC_DEBUG
  printf("* finalizing object:");
  pic_debug(pic, pic_obj_value(obj));
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
    struct pic_port *port = (struct pic_port *)obj;
    if (port->status == PIC_PORT_OPEN) {
      fclose(port->file);
    }
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
gc_free(pic_state *pic, union header *p)
{
  union header *freep, *bp;

  freep = pic->heap->freep;
  for (bp = freep; ! (p > bp && p < bp->s.ptr); bp = bp->s.ptr) {
    if (bp >= bp->s.ptr && (p > bp || p < bp->s.ptr)) {
      break;
    }
  }
  if (p + p->s.size == bp->s.ptr) {
    p->s.size += bp->s.ptr->s.size;
    p->s.ptr = bp->s.ptr->s.ptr;
  }
  else {
    p->s.ptr = bp->s.ptr;
  }
  if (bp + bp->s.size == p) {
    bp->s.size += p->s.size;
    bp->s.ptr = p->s.ptr;
  }
  else {
    bp->s.ptr = p;
  }
  pic->heap->freep = bp;
}

static void
gc_sweep_phase(pic_state *pic)
{
  union header *basep, *bp, *p, *s = NULL, *t;

  basep = &pic->heap->base;
  for (bp = basep->s.ptr; bp != basep; bp = bp->s.ptr) {
    if (bp->s.size == 0)	/* end of page */
      continue;
    for (p = bp + bp->s.size; p != bp->s.ptr; p += p->s.size) {
      if (! gc_is_marked(p)) {
	if (s == NULL) {
	  s = t = p;
	}
	else {
	  t->s.ptr = p;
	  t = t->s.ptr;
	}
	t->s.ptr = NULL; /* For dead objects we can safely reuse ptr field */
      }
      gc_unmark(p);
    }
  }

  /* free! */
  while (s) {
    p = s->s.ptr;
    gc_finalize_object(pic, (struct pic_object *)(s + 1));
    gc_free(pic, s);
    s = p;
  }
}

void
pic_gc_run(pic_state *pic)
{
#if DEBUG
  puts("gc run!");
#endif
  gc_mark_phase(pic);
  gc_sweep_phase(pic);
}

struct pic_object *
pic_obj_alloc_unsafe(pic_state *pic, size_t size, enum pic_tt tt)
{
  struct pic_object *obj;

#if GC_STRESS
  pic_gc_run(pic);
#endif

  obj = (struct pic_object *)gc_alloc(pic, size);
  if (obj == NULL) {
    pic_gc_run(pic);
    obj = (struct pic_object *)gc_alloc(pic, size);
    if (obj == NULL) {
      if (add_heap_page(pic) == NULL)
	pic_abort(pic, "GC memory exhausted");
      obj = (struct pic_object *)gc_alloc(pic, size);
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
