#include "picrin.h"

void pic_init_port(pic_state *);

#define DONE pic_gc_arena_restore(pic, ai);

void
pic_init_core(pic_state *pic)
{
  int ai;

  ai = pic_gc_arena_preserve(pic);
  pic_init_port(pic); DONE;
}
