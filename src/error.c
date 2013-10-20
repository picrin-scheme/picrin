#include <stdlib.h>
#include <stdio.h>

#include "picrin.h"

void
pic_raise(pic_state *pic, const char *str)
{
  puts(str);
  abort();
}
