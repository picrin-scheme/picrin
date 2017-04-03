/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"

int
main()
{
  pic_state *pic;
  pic_value e, form;
  int status;

  pic = pic_open(pic_default_allocf, NULL);

  pic_try {
    while (1) {
      size_t ai = pic_enter(pic);
      pic_printf(pic, "> ");
      form = pic_read(pic, pic_stdin(pic));
      if (pic_eof_p(pic, form)) {
        break;
      }
      pic_printf(pic, "~s\n", pic_funcall(pic, "eval", 1, form));
      pic_leave(pic, ai);
    }

    status = 0;
  }
  pic_catch(e) {
    pic_print_error(pic, pic_stderr(pic), e);
    status = 1;
  }

  pic_close(pic);

  return status;
}
