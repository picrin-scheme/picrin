#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"
#include "picrin/irep.h"
#include "picrin/proc.h"
#include "picrin/port.h"
#include "picrin/blob.h"
#include "picrin/cont.h"

#if GC_DEBUG
# include <stdio.h>
#endif

void
init_heap_page(struct heap_page *heap)
{
  union header *base, *freep;
  void *p;

  p = (union header *)malloc(PIC_HEAP_SIZE);

  heap->base = base = (union header *)
    (((unsigned long)p + sizeof(union header) -1) & ~(sizeof(union header) - 1));
  base->s.ptr = base + 1;
  base->s.size = 1;

  heap->freep = freep = base->s.ptr;
  freep->s.ptr = base;
  freep->s.size = ((char *)p + PIC_HEAP_SIZE - (char *)freep) / sizeof(union header);

  heap->endp = freep + freep->s.size;

#if GC_DEBUG
  printf("base = %p\nbase->s.ptr = %p\n", base, base->s.ptr);
#endif
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

  freep = pic->heap->freep;
  prevp = freep;
  for (p = prevp->s.ptr; ; prevp = p, p = p->s.ptr) {
    if (p->s.size >= nunits)
      break;
    if (p == freep) {
      return 0;
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
    for (stack = cont->st_ptr; stack != cont->sp; ++stack) {
      gc_mark(pic, *stack);
    }

    /* callinfo */
    for (ci = cont->ci; ci != cont->ci_ptr; --ci) {
      if (ci->env) {
	gc_mark_object(pic, (struct pic_object *)ci->env);
      }
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
  printf("finalizing object type %d\n", obj->tt);
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
  case PIC_TT_CONT: {
    struct pic_cont *cont = (struct pic_cont *)obj;
    pic_free(pic, cont->stk_ptr);
    pic_free(pic, cont->st_ptr);
    pic_free(pic, cont->ci_ptr);
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
gc_sweep_phase(pic_state *pic)
{
  union header *base, *bp, *p;

#if GC_DEBUG
  int freed = 0;

  puts("sweep started");
#endif

  base = pic->heap->base;
#if GC_DEBUG
  printf("base = %p\nbase->s.ptr = %p\n", base, base->s.ptr);
#endif
  for (p = base->s.ptr; ; p = p->s.ptr) {

#if GC_DEBUG
    puts("sweeping block");
#endif

  retry:

    for (bp = p + p->s.size; bp != p->s.ptr; bp += bp->s.size) {

#if GC_DEBUG
      printf("  bp = %p\n  p  = %p\n  p->s.ptr = %p\n  endp = %p\n",bp, p, p->s.ptr, pic->heap->endp);
#endif

      if (p >= p->s.ptr && bp == pic->heap->endp)
	break;
      if (gc_is_marked(bp)) {

#if GC_DEBUG
	printf("marked:\t\t\t");
	pic_debug(pic, pic_obj_value((struct pic_object *)(bp + 1)));
	printf("\n");
#endif

	gc_unmark(bp);
	continue;
      }

#if GC_DEBUG
      puts("unmarked");
#endif

      /* free! */
      gc_finalize_object(pic, (struct pic_object *)(bp + 1));
      if (bp + bp->s.size == p->s.ptr) {
	bp->s.size += p->s.ptr->s.size;
	bp->s.ptr = p->s.ptr->s.ptr;
      }
      else {
	bp->s.ptr = p->s.ptr;
      }
      if (p + p->s.size == bp) {
	p->s.size += bp->s.size;
	p->s.ptr = bp->s.ptr;
      }
      else {
	p->s.ptr = bp;
	/* retry with next p */
	p = p->s.ptr;
      }

#if GC_DEBUG
      freed++;
#endif

      goto retry;
    }

    if (p->s.ptr == base)
      break;
  }

#if GC_DEBUG
  printf("freed: %d counts\n", freed);
#endif
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
    if (obj == NULL)
      pic_abort(pic, "GC memory exhausted");
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
