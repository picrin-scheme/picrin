#include "picrin.h"
#include "picrin/extra.h"

#include <unistd.h>

static pic_value
pic_repl_tty_p(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_bool_value(pic, (isatty(STDIN_FILENO)));
}

void
pic_init_repl(pic_state *pic)
{
  pic_defun(pic, "picrin.repl:tty?", pic_repl_tty_p);
}
