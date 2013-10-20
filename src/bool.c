#include <string.h>

#include "picrin.h"

bool
pic_eq_p(pic_state *pic, pic_value x, pic_value y)
{
  if (pic_type(x) != pic_type(y))
    return false;

  switch (pic_type(x)) {
  case PIC_TT_NIL:
    return true;
  case PIC_TT_SYMBOL:
    return pic_symbol_ptr(x) == pic_symbol_ptr(y);
  default:
    return false;
  }
}
