/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "picrin/private/object.h"
#include "picrin/private/state.h"

pic_value
pic_get_backtrace(pic_state *pic)
{
  return pic_lit_value(pic, ""); /* FIXME */
}

#if PIC_USE_WRITE

void
pic_print_error(pic_state *pic, pic_value port, pic_value err)
{
  if (! pic_error_p(pic, err)) {
    pic_fprintf(pic, port, "raise: ~s", err);
  } else {
    struct error *e;
    pic_value elem, it;

    e = pic_error_ptr(pic, err);
    if (! pic_eq_p(pic, pic_obj_value(e->type), pic_intern_lit(pic, ""))) {
      pic_fprintf(pic, port, "~s-", pic_obj_value(e->type));
    }
    pic_fprintf(pic, port, "error: ~s", pic_obj_value(e->msg));

    pic_for_each (elem, e->irrs, it) { /* print error irritants */
      pic_fprintf(pic, port, " ~s", elem);
    }
    pic_fprintf(pic, port, "\n%s", pic_str(pic, pic_obj_value(e->stack)));
  }
}

#endif
