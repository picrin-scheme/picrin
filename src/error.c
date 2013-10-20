#include <stdlib.h>
#include <stdio.h>

#include "picrin.h"

void
pic_abort(pic_state *pic, const char *msg)
{
  puts(msg);
  abort();
}
