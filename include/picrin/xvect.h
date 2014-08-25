#ifndef XVECT_H__
#define XVECT_H__

/*
 * Copyright (c) 2014 by Yuichi Nishiwaki <yuichi@idyll.jp>
 */

#if defined(__cplusplus)
extern "C" {
#endif

#include <stdlib.h>
#include <string.h>

typedef struct xvect {
  char *data;
  size_t size, capa, width;
} xvect;

static inline void xv_init(xvect *, size_t);
static inline void xv_destroy(xvect *);

static inline void xv_reserve(xvect *, size_t);

static inline void *xv_get(xvect *, size_t);
static inline void xv_set(xvect *, size_t, void *);

static inline void xv_push(xvect *, void *);
static inline void *xv_peek(xvect *);
static inline void *xv_pop(xvect *);

static inline void
xv_init(xvect *x, size_t width)
{
  x->data = NULL;
  x->size = 0;
  x->capa = 0;
  x->width = width;
}

static inline void
xv_destroy(xvect *x)
{
  free(x->data);
}

static inline void
xv_reserve(xvect *x, size_t newcapa)
{
  x->data = realloc(x->data, newcapa * x->width);
  x->capa = newcapa;
}

static inline void *
xv_get(xvect *x, size_t i)
{
  return x->data + i * x->width;
}

static inline void
xv_set(xvect *x, size_t i, void *src)
{
  memcpy(x->data + i * x->width, src, x->width);
}

static inline void
xv_push(xvect *x, void *src)
{
  if (x->capa <= x->size + 1) {
    xv_reserve(x, x->size * 2 + 1);
  }
  xv_set(x, x->size++, src);
}

static inline void *
xv_peek(xvect *x)
{
  return xv_get(x, x->size);
}

static inline void *
xv_pop(xvect *x)
{
  return xv_get(x, --x->size);
}

#if defined(__cplusplus)
}
#endif

#endif
