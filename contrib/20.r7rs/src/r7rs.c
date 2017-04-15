/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

void pic_nitro_init_load(pic_state *);
void pic_nitro_init_system(pic_state *);
void pic_nitro_init_time(pic_state *);

void
pic_nitro_init_r7rs(pic_state *pic)
{
  pic_nitro_init_load(pic);
  pic_nitro_init_system(pic);
  pic_nitro_init_time(pic);

  pic_add_feature(pic, "r7rs");
}
