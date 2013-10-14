#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"
#include "picrin/irep.h"

void
init_heap_page(struct heap_page *heap)
{
  union header *base, *freep;
  void *p;

  p = (union header *)malloc(PIC_HEAP_SIZE);

  heap->base = base = (union header *)
    (((unsigned long)p + sizeof(union header) -1) & ~(sizeof(union header) - 1));
  base->s.ptr = base + 1;
  base->s.size = 0;

  heap->freep = freep = base->s.ptr;
  freep->s.ptr = base;
  freep->s.size = ((char *)p + PIC_HEAP_SIZE - (char *)freep) / sizeof(union header);
}

void *
pic_alloc(pic_state *pic, size_t size)
{
  void *ptr;

  ptr = malloc(size);
  if (ptr == NULL) {
    pic_raise(pic, "memory exhausted");
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
    pic_raise(pic, "arena overflow");
  }
  pic->arena[pic->arena_idx++] = obj;
}

void
pic_gc_protect(pic_state *pic, pic_value v)
{
  struct pic_object *obj;

  if (v.type != PIC_VTYPE_HEAP) {
    return;
  }
  obj = pic_object_ptr(v);

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

static void
gc_mark_object(pic_state *pic, struct pic_object *obj)
{
  union header *p;

  p = (union header *)obj - 1;
  p->s.mark = PIC_GC_MARK;

  switch (obj->tt) {
  case PIC_TT_PAIR: {
    gc_mark(pic, ((struct pic_pair *)obj)->car);
    gc_mark(pic, ((struct pic_pair *)obj)->cdr);
    break;
  }
  case PIC_TT_SYMBOL: {
    break;
  }
  case PIC_TT_PROC: {
    break;
  }
  default:
    pic_raise(pic, "logic flaw");
  }
}

static void
gc_mark(pic_state *pic, pic_value v)
{
  struct pic_object *obj;

  if (v.type != PIC_VTYPE_HEAP)
    return;
  obj = pic_object_ptr(v);

  gc_mark_object(pic, obj);
}

static void
gc_mark_phase(pic_state *pic)
{
  pic_value *stack;
  struct pic_env *env;
  int i;

  /* stack */
  for (stack = pic->stbase; stack != pic->sp; ++stack) {
    gc_mark(pic, *stack);
  }
  gc_mark(pic, *stack);

  /* arena */
  for (i = 0; i < pic->arena_idx; ++i) {
    gc_mark_object(pic, pic->arena[i]);
  }

  /* global env */
  env = pic->global_env;
  do {
    gc_mark(pic, env->assoc);
  } while ((env = env->parent) != NULL);
}

static bool
is_marked(union header *p)
{
  return p->s.mark == PIC_GC_MARK;
}

static void
gc_unmark(union header *p)
{
  p->s.mark = PIC_GC_UNMARK;
}

static void
gc_finalize_object(pic_state *pic, struct pic_object *obj)
{
  switch (obj->tt) {
  case PIC_TT_SYMBOL: {
    char *name;
    name = ((struct pic_symbol *)obj)->name;
    pic_free(pic, name);
    break;
  }
  case PIC_TT_PAIR: {
    break;
  }
  case PIC_TT_PROC: {
    struct pic_proc *proc;

    proc = (struct pic_proc *)obj;

    /* free irep */
    pic_free(pic, proc->u.irep->code);
    pic_free(pic, proc->u.irep);
    break;
  }
  default:
    pic_raise(pic, "logic flaw");
  }
}

static void
gc_sweep_phase(pic_state *pic)
{
  union header *freep, *bp, *p;

  freep = pic->heap->freep;
  for (p = freep; p != freep; p = p->s.ptr) {
    for (bp = p + p->s.size; bp != p->s.ptr; bp += bp->s.size) {
      if (is_marked(bp)) {
	gc_unmark(bp);
	continue;
      }
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
      }
    }
  }
}

void
pic_gc_run(pic_state *pic)
{
  puts("gc run!");
  gc_mark_phase(pic);
  gc_sweep_phase(pic);
}

struct pic_object *
pic_obj_alloc(pic_state *pic, size_t size, enum pic_tt tt)
{
  struct pic_object *obj;

  obj = (struct pic_object *)gc_alloc(pic, size);
  if (obj == NULL) {
    pic_gc_run(pic);
    obj = (struct pic_object *)gc_alloc(pic, size);
    if (obj == NULL)
      pic_raise(pic, "GC memory exhausted");
  }
  obj->tt = tt;

  gc_protect(pic, obj);
  return obj;
}
