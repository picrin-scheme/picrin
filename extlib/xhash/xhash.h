#ifndef XHASH_H__
#define XHASH_H__

/*
 * Copyright (c) 2013 by Yuichi Nishiwaki <yuichi.nishiwaki@gmail.com>
 */

#include <stdlib.h>
#include <string.h>

/* simple string to int hash table */

#define XHASH_INIT_SIZE 11

struct xh_entry {
  struct xh_entry *next;
  const char *key;
  int val;
};

struct xhash {
  struct xh_entry **buckets;
  size_t size;
};

static inline struct xhash *
xh_new()
{
  struct xhash *x;

  x = (struct xhash *)malloc(sizeof(struct xhash));
  x->size = XHASH_INIT_SIZE;
  x->buckets = (struct xh_entry **)calloc(XHASH_INIT_SIZE, sizeof(struct xh_entry *));
  return x;
}

static int
xh_hash(const char *str)
{
  int hash = 0;

  while (*str) {
    hash = hash * 31 + *str++;
  }
  return hash;
}

static inline struct xh_entry *
xh_get(struct xhash *x, const char *key)
{
  int idx;
  struct xh_entry *e;

  idx = xh_hash(key) % x->size;
  for (e = x->buckets[idx]; e; e = e->next) {
    if (! strcmp(key, e->key))
      return e;
  }
  return NULL;
}

static inline struct xh_entry *
xh_put(struct xhash *x, const char *key, int val)
{
  int idx;
  struct xh_entry *e;

  if ((e = xh_get(x, key))) {
    e->val = val;
    return e;
  }

  idx = xh_hash(key) % x->size;
  e = (struct xh_entry *)malloc(sizeof(struct xh_entry));
  e->next = x->buckets[idx];
  e->key = strdup(key);
  e->val = val;

  return x->buckets[idx] = e;
}

static inline void
xh_destory(struct xhash *x)
{
  int i;
  struct xh_entry *e, *d;

  for (i = 0; i < x->size; ++i) {
    e = x->buckets[i];
    while (e) {
      d = e->next;
      free((void*)e->key);
      free(e);
      e = d;
    }
  }
  free(x);
}

#endif
