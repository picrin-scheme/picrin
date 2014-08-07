/*
  cc -g test.c -o test -ledit -ltermcap
*/

/* This will include all our libedit functions.  If you use C++ don't
forget to use the C++ extern "C" to get it to compile.
*/
#include <editline/readline.h>
#include <editline/history.h>
#include <histedit.h>
#include "picrin.h"
#include "picrin/string.h"
#include "picrin/port.h"


static pic_value
pic_rl_readline(pic_state *pic)
{
  char *prompt, *result;
  
  pic_get_args(pic, "z", &prompt);

  result = readline(prompt);

  if(result)
    return pic_obj_value(pic_str_new_cstr(pic, result));
  else
    return pic_eof_object();
}

static pic_value
pic_rl_add_history(pic_state *pic)
{
  char *line;

  pic_get_args(pic, "z", &line);

  add_history(line);

  return pic_undef_value();
}

static pic_value
pic_rl_clear_history(pic_state *pic)
{
  pic_get_args(pic, "");

  clear_history();

  return pic_undef_value();
}

static pic_value
pic_rl_remove_history(pic_state *pic)
{
  int line;

  pic_get_args(pic, "i", &line);

  remove_history(line);
  
  return pic_undef_value();
}

void
pic_init_readline(pic_state *pic){
  pic_deflibrary (pic, "(picrin readline)") {
    pic_defun(pic, "readline", pic_rl_readline);
    pic_defun(pic, "add-history", pic_rl_add_history);
    pic_defun(pic, "clear-history", pic_rl_clear_history);
    pic_defun(pic, "remove-history", pic_rl_remove_history);
  }
}
