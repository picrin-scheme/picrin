#include <stdlib.h>

#include "picrin.h"

static struct pic_env *
pic_new_empty_env()
{
  struct pic_env *env;

  env = (struct pic_env *)malloc(sizeof(struct pic_env));
  env->assoc = pic_nil_value();
  env->parent = NULL;

  return env;
}

pic_state *
pic_open()
{
  pic_state *pic;

  pic = (pic_state *)malloc(sizeof(pic_state));

  /* prepare VM stack */
  pic->stbase = pic->sp = (pic_value *)malloc(sizeof(pic_value) * 1024);
  pic->stend = pic->stbase + 1024;

  pic->global_env = pic_new_empty_env();

  return pic;
}

void
pic_close(pic_state *pic)
{
  free(pic);
}
