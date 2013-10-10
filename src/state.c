#include <stdlib.h>

#include "picrin.h"

pic_state *
pic_open()
{
  pic_state *pic;

  pic = (pic_state *)calloc(sizeof(pic_state));

  return pic;
}

void
pic_close(pic_state *pic)
{
  free(pic);
}
