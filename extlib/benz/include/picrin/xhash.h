#ifndef XHASH_H
#define XHASH_H

/*
 * Copyright (c) 2013-2014 by Yuichi Nishiwaki <yuichi.nishiwaki@gmail.com>
 */

#if defined(__cplusplus)
extern "C" {
#endif

#define XHASH_ALLOCATOR pic->allocf

/* simple object to object hash table */

#define XHASH_INIT_SIZE 11
#define XHASH_RESIZE_RATIO(x) ((x) * 3 / 4)

#define XHASH_ALIGNMENT 3       /* quad word alignment */
#define XHASH_MASK (~(size_t)((1 << XHASH_ALIGNMENT) - 1))
#define XHASH_ALIGN(i) ((((i) - 1) & XHASH_MASK) + (1 << XHASH_ALIGNMENT))

typedef struct xh_entry {
  struct xh_entry *next;
  int hash;
  struct xh_entry *fw, *bw;
  const void *key;
  void *val;
} xh_entry;

#define xh_key(e,type) (*(type *)((e)->key))
#define xh_val(e,type) (*(type *)((e)->val))

typedef int (*xh_hashf)(const void *, void *);
typedef int (*xh_equalf)(const void *, const void *, void *);
typedef void *(*xh_allocf)(void *, size_t);

typedef struct xhash {
  xh_allocf allocf;
  xh_entry **buckets;
  size_t size, count, kwidth, vwidth;
  size_t koffset, voffset;
  xh_hashf hashf;
  xh_equalf equalf;
  xh_entry *head, *tail;
  void *data;
} xhash;

/** Protected Methods:
 * static inline void xh_init_(xhash *x, size_t, size_t, xh_hashf, xh_equalf, void *);
 * static inline xh_entry *xh_get_(xhash *x, const void *key);
 * static inline xh_entry *xh_put_(xhash *x, const void *key, void *val);
 * static inline void xh_del_(xhash *x, const void *key);
 */

/* string map */
PIC_INLINE void xh_init_str(xhash *x, size_t width);
PIC_INLINE xh_entry *xh_get_str(xhash *x, const char *key);
PIC_INLINE xh_entry *xh_put_str(xhash *x, const char *key, void *);
PIC_INLINE void xh_del_str(xhash *x, const char *key);

/* object map */
PIC_INLINE void xh_init_ptr(xhash *x, size_t width);
PIC_INLINE xh_entry *xh_get_ptr(xhash *x, const void *key);
PIC_INLINE xh_entry *xh_put_ptr(xhash *x, const void *key, void *);
PIC_INLINE void xh_del_ptr(xhash *x, const void *key);

/* int map */
PIC_INLINE void xh_init_int(xhash *x, size_t width);
PIC_INLINE xh_entry *xh_get_int(xhash *x, int key);
PIC_INLINE xh_entry *xh_put_int(xhash *x, int key, void *);
PIC_INLINE void xh_del_int(xhash *x, int key);

PIC_INLINE size_t xh_size(xhash *x);
PIC_INLINE void xh_clear(xhash *x);
PIC_INLINE void xh_destroy(xhash *x);

PIC_INLINE xh_entry *xh_begin(xhash *x);
PIC_INLINE xh_entry *xh_next(xh_entry *e);


PIC_INLINE void
xh_bucket_alloc(xhash *x, size_t newsize)
{
  x->size = newsize;
  x->buckets = x->allocf(NULL, (x->size + 1) * sizeof(xh_entry *));
  memset(x->buckets, 0, (x->size + 1) * sizeof(xh_entry *));
}

PIC_INLINE void
xh_init_(xhash *x, xh_allocf allocf, size_t kwidth, size_t vwidth, xh_hashf hashf, xh_equalf equalf, void *data)
{
  x->allocf = allocf;
  x->size = 0;
  x->buckets = NULL;
  x->count = 0;
  x->kwidth = kwidth;
  x->vwidth = vwidth;
  x->koffset = XHASH_ALIGN(sizeof(xh_entry));
  x->voffset = XHASH_ALIGN(sizeof(xh_entry)) + XHASH_ALIGN(kwidth);
  x->hashf = hashf;
  x->equalf = equalf;
  x->head = NULL;
  x->tail = NULL;
  x->data = data;

  xh_bucket_alloc(x, XHASH_INIT_SIZE);
}

PIC_INLINE xh_entry *
xh_get_(xhash *x, const void *key)
{
  int hash;
  size_t idx;
  xh_entry *e;

  hash = x->hashf(key, x->data);
  idx = ((unsigned)hash) % x->size;
  for (e = x->buckets[idx]; e; e = e->next) {
    if (e->hash == hash && x->equalf(key, e->key, x->data))
      break;
  }
  return e;
}

PIC_INLINE void
xh_resize_(xhash *x, size_t newsize)
{
  xhash y;
  xh_entry *it;
  size_t idx;

  xh_init_(&y, x->allocf, x->kwidth, x->vwidth, x->hashf, x->equalf, x->data);
  xh_bucket_alloc(&y, newsize);

  for (it = xh_begin(x); it != NULL; it = xh_next(it)) {
    idx = ((unsigned)it->hash) % y.size;
    /* reuse entry object */
    it->next = y.buckets[idx];
    y.buckets[idx] = it;
    y.count++;
  }

  y.head = x->head;
  y.tail = x->tail;

  x->allocf(x->buckets, 0);

  /* copy all members from y to x */
  memcpy(x, &y, sizeof(xhash));
}

PIC_INLINE xh_entry *
xh_put_(xhash *x, const void *key, void *val)
{
  int hash;
  size_t idx;
  xh_entry *e;

  if ((e = xh_get_(x, key))) {
    memcpy(e->val, val, x->vwidth);
    return e;
  }

  if (x->count + 1 > XHASH_RESIZE_RATIO(x->size)) {
    xh_resize_(x, x->size * 2 + 1);
  }

  hash = x->hashf(key, x->data);
  idx = ((unsigned)hash) % x->size;
  e = x->allocf(NULL, x->voffset + x->vwidth);
  e->next = x->buckets[idx];
  e->hash = hash;
  e->key = ((char *)e) + x->koffset;
  e->val = ((char *)e) + x->voffset;
  memcpy((void *)e->key, key, x->kwidth);
  memcpy(e->val, val, x->vwidth);

  if (x->head == NULL) {
    x->head = x->tail = e;
    e->fw = e->bw = NULL;
  } else {
    x->tail->bw = e;
    e->fw = x->tail;
    e->bw = NULL;
    x->tail = e;
  }

  x->count++;

  return x->buckets[idx] = e;
}

PIC_INLINE void
xh_del_(xhash *x, const void *key)
{
  int hash;
  size_t idx;
  xh_entry *p, *q, *r;

  hash = x->hashf(key, x->data);
  idx = ((unsigned)hash) % x->size;
  if (x->buckets[idx]->hash == hash && x->equalf(key, x->buckets[idx]->key, x->data)) {
    q = x->buckets[idx];
    if (q->fw == NULL) {
      x->head = q->bw;
    } else {
      q->fw->bw = q->bw;
    }
    if (q->bw == NULL) {
      x->tail = q->fw;
    } else {
      q->bw->fw = q->fw;
    }
    r = q->next;
    x->allocf(q, 0);
    x->buckets[idx] = r;
  }
  else {
    for (p = x->buckets[idx]; ; p = p->next) {
      if (p->next->hash == hash && x->equalf(key, p->next->key, x->data))
        break;
    }
    q = p->next;
    if (q->fw == NULL) {
      x->head = q->bw;
    } else {
      q->fw->bw = q->bw;
    }
    if (q->bw == NULL) {
      x->tail = q->fw;
    } else {
      q->bw->fw = q->fw;
    }
    r = q->next;
    x->allocf(q, 0);
    p->next = r;
  }

  x->count--;
}

PIC_INLINE size_t
xh_size(xhash *x)
{
  return x->count;
}

PIC_INLINE void
xh_clear(xhash *x)
{
  size_t i;
  xh_entry *e, *d;

  for (i = 0; i < x->size; ++i) {
    e = x->buckets[i];
    while (e) {
      d = e->next;
      x->allocf(e, 0);
      e = d;
    }
    x->buckets[i] = NULL;
  }

  x->head = x->tail = NULL;
  x->count = 0;
}

PIC_INLINE void
xh_destroy(xhash *x)
{
  xh_clear(x);
  x->allocf(x->buckets, 0);
}

/* string map */

PIC_INLINE int
xh_str_hash(const void *key, void *data)
{
  const char *str = *(const char **)key;
  int hash = 0;

  (void)data;

  while (*str) {
    hash = hash * 31 + *str++;
  }
  return hash;
}

PIC_INLINE int
xh_str_equal(const void *key1, const void *key2, void *data)
{
  const char *s1 = *(const char **)key1, *s2 = *(const char **)key2;

  (void)data;

  return strcmp(s1, s2) == 0;
}

#define xh_init_str(x, width)                                           \
  xh_init_(x, XHASH_ALLOCATOR, sizeof(const char *), width, xh_str_hash, xh_str_equal, NULL);

PIC_INLINE xh_entry *
xh_get_str(xhash *x, const char *key)
{
  return xh_get_(x, &key);
}

PIC_INLINE xh_entry *
xh_put_str(xhash *x, const char *key, void *val)
{
  return xh_put_(x, &key, val);
}

PIC_INLINE void
xh_del_str(xhash *x, const char *key)
{
  xh_del_(x, &key);
}

/* object map */

PIC_INLINE int
xh_ptr_hash(const void *key, void *data)
{
  (void)data;

  return (int)(size_t)*(const void **)key;
}

PIC_INLINE int
xh_ptr_equal(const void *key1, const void *key2, void *data)
{
  (void) data;

  return *(const void **)key1 == *(const void **)key2;
}

#define xh_init_ptr(x, width)                   \
  xh_init_(x, XHASH_ALLOCATOR, sizeof(const void *), width, xh_ptr_hash, xh_ptr_equal, NULL);

PIC_INLINE xh_entry *
xh_get_ptr(xhash *x, const void *key)
{
  return xh_get_(x, &key);
}

PIC_INLINE xh_entry *
xh_put_ptr(xhash *x, const void *key, void *val)
{
  return xh_put_(x, &key, val);
}

PIC_INLINE void
xh_del_ptr(xhash *x, const void *key)
{
  xh_del_(x, &key);
}

/* int map */

PIC_INLINE int
xh_int_hash(const void *key, void *data)
{
  (void)data;

  return *(int *)key;
}

PIC_INLINE int
xh_int_equal(const void *key1, const void *key2, void *data)
{
  (void)data;

  return *(int *)key1 == *(int *)key2;
}

#define xh_init_int(x, width)                   \
  xh_init_(x, XHASH_ALLOCATOR, sizeof(int), width, xh_int_hash, xh_int_equal, NULL);

PIC_INLINE xh_entry *
xh_get_int(xhash *x, int key)
{
  return xh_get_(x, &key);
}

PIC_INLINE xh_entry *
xh_put_int(xhash *x, int key, void *val)
{
  return xh_put_(x, &key, val);
}

PIC_INLINE void
xh_del_int(xhash *x, int key)
{
  xh_del_(x, &key);
}

/** iteration */

PIC_INLINE xh_entry *
xh_begin(xhash *x)
{
  return x->head;
}

PIC_INLINE xh_entry *
xh_next(xh_entry *e)
{
  return e->bw;
}

#if defined(__cplusplus)
}
#endif

#endif
