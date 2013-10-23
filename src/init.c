#include "picrin.h"

void pic_init_pair(pic_state *);
void pic_init_port(pic_state *);
void pic_init_number(pic_state *);
void pic_init_time(pic_state *);
void pic_init_system(pic_state *);
void pic_init_file(pic_state *);

#define DONE pic_gc_arena_restore(pic, ai);

void
pic_init_core(pic_state *pic)
{
  int ai;

  ai = pic_gc_arena_preserve(pic);
  pic_init_pair(pic); DONE;
  pic_init_port(pic); DONE;
  pic_init_number(pic); DONE;
  pic_init_time(pic); DONE;
  pic_init_system(pic); DONE;
  pic_init_file(pic); DONE;
}
