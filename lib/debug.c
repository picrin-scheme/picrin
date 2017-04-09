/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"
#include "state.h"

#if PIC_USE_WRITE

void
pic_print_error(pic_state *pic, pic_value port, pic_value err)
{
  if (! pic_error_p(pic, err)) {
    pic_fprintf(pic, port, "raise: ~s", err);
  } else {
    struct error *e;
    pic_value elem, it;

    e = error_ptr(pic, err);
    if (! pic_eq_p(pic, obj_value(pic, e->type), pic_intern_lit(pic, ""))) {
      pic_fprintf(pic, port, "~s-", obj_value(pic, e->type));
    }
    pic_fprintf(pic, port, "error: ~s", obj_value(pic, e->msg));

    pic_for_each (elem, e->irrs, it) { /* print error irritants */
      pic_fprintf(pic, port, " ~s", elem);
    }
    pic_fprintf(pic, port, "\n");
  }
}

#endif
