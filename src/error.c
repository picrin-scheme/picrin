#include <stdlib.h>
#include <stdio.h>

#include "picrin.h"

void
pic_error(pic_state *pic, const char *msg)
{
  pic->errmsg = msg;
  if (! pic->jmp) {
    puts(msg);
    abort();
  }
  longjmp(*pic->jmp, 1);
}

void
pic_abort(pic_state *pic, const char *msg)
{
  puts(msg);
  abort();
}
