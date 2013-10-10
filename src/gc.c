#include <stdlib.h>

#include "picrin.h"

void *
pic_alloc(pic_state *pic, size_t size)
{
  /* mock */
  return malloc(size);
}

struct pic_object *
pic_gc_alloc(pic_state *pic, size_t size, enum pic_tt tt)
{
  struct pic_object *obj;

  obj = (struct pic_object *)malloc(size);
  obj->tt = tt;

  return obj;
}

void
pic_free(pic_state *pic, void *ptr)
{
  free(ptr);
}
