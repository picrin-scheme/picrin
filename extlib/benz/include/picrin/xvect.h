#ifndef XVECT_H__
#define XVECT_H__

/*
 * Copyright (c) 2014 by Yuichi Nishiwaki <yuichi@idylls.jp>
 */

#if defined(__cplusplus)
extern "C" {
#endif

typedef struct xvect {
  char *data;
  size_t size, mask, head, tail, width;
} xvect;

static inline void xv_init(xvect *, size_t);
static inline void xv_destroy(xvect *);

static inline size_t xv_size(xvect *);

static inline void xv_reserve(xvect *, size_t);
static inline void xv_shrink(xvect *, size_t);

static inline void *xv_get(xvect *, size_t);
static inline void xv_set(xvect *, size_t, void *);

static inline void xv_push(xvect *, void *);
static inline void *xv_pop(xvect *);

static inline void *xv_shift(xvect *);
static inline void xv_unshift(xvect *, void *);

static inline void xv_splice(xvect *, size_t, size_t);
static inline void xv_insert(xvect *, size_t, void *);

static inline void
xv_init(xvect *x, size_t width)
{
  x->data = NULL;
  x->width = width;
  x->size = 0;
  x->mask = (size_t)-1;
  x->head = 0;
  x->tail = 0;
}

static inline void
xv_destroy(xvect *x)
{
  free(x->data);
}

static inline size_t
xv_size(xvect *x)
{
  return x->tail < x->head
    ? x->tail + x->size - x->head
    : x->tail - x->head;
}

static inline size_t
xv_round2(size_t x)
{
  x -= 1;
  x |= (x >> 1);
  x |= (x >> 2);
  x |= (x >> 4);
  x |= (x >> 8);
  x |= (x >> 16);
  x |= (x >> 32);
  x++;
  return x;
}

static inline void
xv_rotate(xvect *x)
{
  if (x->tail < x->head) {
    char buf[x->size * x->width];

    /* perform rotation */
    memcpy(buf, x->data, sizeof buf);
    memcpy(x->data, buf + x->head * x->width, (x->size - x->head) * x->width);
    memcpy(x->data + (x->size - x->head) * x->width, buf, x->tail * x->width);
    x->tail = x->size - x->head + x->tail;
    x->head = 0;
  }
}

static inline void
xv_adjust(xvect *x, size_t size)
{
  size = xv_round2(size);
  if (size != x->size) {
    xv_rotate(x);
    x->data = realloc(x->data, size * x->width);
    x->size = size;
    x->mask = size - 1;
  }
}

static inline void
xv_reserve(xvect *x, size_t mincapa)
{
  if (x->size < mincapa + 1) {
    xv_adjust(x, mincapa + 1);  /* capa == size - 1 */
  }
}

static inline void
xv_shrink(xvect *x, size_t maxcapa)
{
  if (x->size > maxcapa + 1) {
    xv_adjust(x, maxcapa + 1);  /* capa == size - 1 */
  }
}

static inline void *
xv_get(xvect *x, size_t i)
{
  assert(i < xv_size(x));

  return x->data + ((x->head + i) & x->mask) * x->width;
}

static inline void
xv_set(xvect *x, size_t i, void *src)
{
  memcpy(xv_get(x, i), src, x->width);
}

static inline void
xv_push(xvect *x, void *src)
{
  xv_reserve(x, xv_size(x) + 1);
  x->tail = (x->tail + 1) & x->mask;
  xv_set(x, xv_size(x) - 1, src);
}

static inline void *
xv_pop(xvect *x)
{
  void *dat;

  assert(xv_size(x) >= 1);

  dat = xv_get(x, xv_size(x) - 1);
  x->tail = (x->tail - 1) & x->mask;
  return dat;
}

static inline void *
xv_shift(xvect *x)
{
  void *dat;

  assert(xv_size(x) >= 1);

  dat = xv_get(x, 0);
  x->head = (x->head + 1) & x->mask;
  return dat;
}

static inline void
xv_unshift(xvect *x, void *src)
{
  xv_reserve(x, xv_size(x) + 1);
  x->head = (x->head - 1) & x->mask;
  xv_set(x, 0, src);
}

static inline void
xv_splice(xvect *x, size_t i, size_t j)
{
  assert(i <= j && j < xv_size(x));

  xv_rotate(x);
  memmove(xv_get(x, i), xv_get(x, j), (xv_size(x) - j) * x->width);
  x->tail = (x->tail - j + i) & x->mask;
}

static inline void
xv_insert(xvect *x, size_t i, void *src)
{
  assert(i <= xv_size(x));

  xv_reserve(x, xv_size(x) + 1);
  xv_rotate(x);
  x->tail = (x->tail + 1) & x->mask;

  if (xv_size(x) - 1 != i) {
    memmove(xv_get(x, i + 1), xv_get(x, i), (xv_size(x) - 1 - i) * x->width);
  }
  xv_set(x, i, src);
}

#if defined(__cplusplus)
}
#endif

#endif
