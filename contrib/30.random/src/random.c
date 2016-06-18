#include "picrin.h"
#include "picrin/extra.h"

double genrand_real3(void);

static pic_value
pic_random_real(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_float_value(pic, genrand_real3());
}

void
pic_init_random(pic_state *pic)
{
  pic_deflibrary(pic, "srfi.27");

  pic_defun(pic, "random-real", pic_random_real);
}
