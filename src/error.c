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
pic_errorf(pic_state *pic, const char *msg, size_t n, ...)
{
  pic_error(pic, msg);
}

void
pic_abort(pic_state *pic, const char *msg)
{
  puts(msg);
  abort();
}

void
pic_warn(pic_state *pic, const char *msg)
{
  fprintf(stderr, "warn: %s\n", msg);
}
