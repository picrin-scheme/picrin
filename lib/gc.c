/**
 * See Copyright Notice in picrin.h
 */

#include <picrin.h>
#include "value.h"
#include "object.h"
#include "state.h"

struct object {
  union {
    struct basic basic;
    struct symbol sym;
    struct string str;
    struct blob blob;
    struct pair pair;
    struct vector vec;
    struct dict dict;
    struct attr attr;
    struct data data;
    struct record rec;
    struct proc proc;
    struct frame frame;
    struct irep irep;
  } u;
};

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
pic_realloc(pic_state *pic, void *ptr, size_t size)
{
  void *p;

 retry:
  p = pic->allocf(pic->userdata, ptr, size);
  if (p == NULL && size > 0) {
    pic->panicf(pic, "out of memory", 0, NULL);
    goto retry;
  }
  return p;
}

void *
pic_malloc(pic_state *pic, size_t size)
{
  return pic_realloc(pic, NULL, size);
}

void *
pic_calloc(pic_state *pic, size_t count, size_t size)
{
  void *ptr = pic_malloc(pic, count * size);
  memset(ptr, 0, count * size);
  return ptr;
}

void
pic_free(pic_state *pic, void *ptr)
{
  pic->allocf(pic->userdata, ptr, 0);
}

size_t
pic_enter(pic_state *pic)
{
  return pic->ai;
}

void
pic_leave(pic_state *pic, size_t ai)
{
  pic->ai = ai;
}

pic_value
pic_protect(pic_state *pic, pic_value v)
{
  if (! pic_obj_p(pic, v))
    return v;

  if (pic->ai >= pic->arena_size) {
    pic->arena_size = pic->arena_size * 2 + 1;
    pic->arena = pic_realloc(pic, pic->arena, sizeof(struct object *) * pic->arena_size);
  }
  pic->arena[pic->ai++] = pic_ptr(pic, v);
  return v;
}

void *
pic_alloca(pic_state *pic, size_t n)
{
  static const pic_data_type t = { "pic_alloca", pic_free };

  return pic_data(pic, pic_data_value(pic, pic_malloc(pic, n), &t));
}

/* GC */

#define is_alive(obj) ((obj)->u.basic.tt & GC_MARK)
#define mark(obj) ((obj)->u.basic.tt |= GC_MARK)
#define unmark(obj) ((obj)->u.basic.tt &= ~GC_MARK)

static void gc_mark_object(pic_state *, struct object *);

static void
gc_mark(pic_state *pic, pic_value v)
{
  if (! pic_obj_p(pic, v))
    return;

  gc_mark_object(pic, pic_ptr(pic, v));
}

static void
gc_mark_object(pic_state *pic, struct object *obj)
{
 loop:

  if (is_alive(obj))
    return;

  mark(obj);

#define LOOP(o) obj = (struct object *)(o); goto loop

  switch (obj_type(obj)) {
  case PIC_TYPE_PAIR: {
    gc_mark(pic, obj->u.pair.car);
    if (pic_obj_p(pic, obj->u.pair.cdr)) {
      LOOP(pic_ptr(pic, obj->u.pair.cdr));
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
  case PIC_TYPE_VECTOR: {
    int i;
    for (i = 0; i < obj->u.vec.len; ++i) {
      gc_mark(pic, obj->u.vec.data[i]);
    }
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
  case PIC_TYPE_ATTR: {
    struct attr *attr = (struct attr *)obj;

    attr->prev = pic->gc_attrs;
    pic->gc_attrs = attr;
    break;
  }

  case PIC_TYPE_STRING:
  case PIC_TYPE_BLOB:
  case PIC_TYPE_DATA:
    break;

  default:
    PIC_UNREACHABLE();
  }
}

static void
gc_finalize_object(pic_state *pic, struct object *obj)
{
  switch (obj_type(obj)) {
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
  case PIC_TYPE_ATTR: {
    kh_destroy(attr, &obj->u.attr.hash);
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
  case PIC_TYPE_FRAME: {
    pic_free(pic, obj->u.frame.regs);
    break;
  }

  case PIC_TYPE_PAIR:
  case PIC_TYPE_RECORD:
  case PIC_TYPE_PROC_FUNC:
  case PIC_TYPE_PROC_IREP:
    break;

  default:
    PIC_UNREACHABLE();
  }
}

void
pic_gc(pic_state *pic)
{
  struct context *cxt;
  size_t j;
  khash_t(oblist) *s = &pic->oblist;
  struct symbol *sym;
  int it;
  struct object *obj, *prev, *next;

  assert(pic->gc_attrs == NULL);

  if (! pic->gc_enable) {
    return;
  }

  /* scan objects */

  for (cxt = pic->cxt; cxt != NULL; cxt = cxt->prev) {
    if (cxt->fp) gc_mark_object(pic, (struct object *)cxt->fp);
    if (cxt->sp) gc_mark_object(pic, (struct object *)cxt->sp);
    if (cxt->irep) gc_mark_object(pic, (struct object *)cxt->irep);
    gc_mark(pic, cxt->conts);
  }

  for (j = 0; j < pic->ai; ++j) {
    gc_mark_object(pic, (struct object *)pic->arena[j]);
  }

  gc_mark(pic, pic->globals);
  gc_mark(pic, pic->halt);

  /* scan weak references */

  do {
    struct object *key;
    pic_value val;
    int it;
    khash_t(attr) *h;
    struct attr *attr;

    j = 0;
    attr = pic->gc_attrs;

    while (attr != NULL) {
      h = &attr->hash;
      for (it = kh_begin(h); it != kh_end(h); ++it) {
        if (! kh_exist(h, it))
          continue;
        key = kh_key(h, it);
        val = kh_val(h, it);
        if (is_alive(key)) {
          if (pic_obj_p(pic, val) && ! is_alive((struct object *) pic_ptr(pic, val))) {
            gc_mark(pic, val);
            ++j;
          }
        }
      }
      attr = attr->prev;
    }
  } while (j > 0);

  /* reclaim dead weak references */

  while (pic->gc_attrs != NULL) {
    khash_t(attr) *h = &pic->gc_attrs->hash;
    for (it = kh_begin(h); it != kh_end(h); ++it) {
      if (! kh_exist(h, it))
        continue;
      obj = kh_key(h, it);
      if (! is_alive(obj)) {
        kh_del(attr, h, it);
      }
    }
    pic->gc_attrs = pic->gc_attrs->prev;
  }

  for (it = kh_begin(s); it != kh_end(s); ++it) {
    if (! kh_exist(s, it))
      continue;
    sym = kh_val(s, it);
    if (sym && ! is_alive((struct object *)sym)) {
      kh_del(oblist, s, it);
    }
  }

  /* reclaim dead objects */

  for (prev = (struct object *) &pic->gc_head, obj = prev->u.basic.next; obj != (struct object *) &pic->gc_head; prev = obj, obj = next) {
    next = obj->u.basic.next;
    if (is_alive(obj)) {
      unmark(obj);
    } else {
      gc_finalize_object(pic, obj);
      pic_free(pic, obj);
      prev->u.basic.next = next;
      obj = prev;
    }
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
    case PIC_TYPE_ATTR: return sizeof(struct attr);
    case PIC_TYPE_IREP: return sizeof(struct irep);
    case PIC_TYPE_PAIR: return sizeof(struct pair);
    case PIC_TYPE_FRAME: return sizeof(struct frame);
    case PIC_TYPE_RECORD: return sizeof(struct record);
    case PIC_TYPE_PROC_FUNC: return sizeof(struct proc);
    case PIC_TYPE_PROC_IREP: return sizeof(struct proc);
    default: PIC_UNREACHABLE();
  }
}

struct object *
pic_obj_alloc_unsafe(pic_state *pic, int type)
{
  struct object *obj;
  size_t size = type2size(type);

  if (pic->gc_count > PIC_GC_PERIOD) {
    pic_gc(pic);
    pic->gc_count -= PIC_GC_PERIOD;
  }

  obj = pic_malloc(pic, size);
  obj->u.basic.tt = type;
  obj->u.basic.next = pic->gc_head.next;
  pic->gc_head.next = obj;

  pic->gc_count += size;

  return obj;
}

struct object *
pic_obj_alloc(pic_state *pic, int type)
{
  struct object *obj;

  obj = pic_obj_alloc_unsafe(pic, type);

  pic_protect(pic, obj_value(pic, obj));
  return obj;
}
