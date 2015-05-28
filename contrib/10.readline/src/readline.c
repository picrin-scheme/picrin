/*
  cc -g test.c -o test -ledit -ltermcap
*/

/* This will include all our libedit functions.  If you use C++ don't
forget to use the C++ extern "C" to get it to compile.
*/
#include "picrin.h"

#if PIC_READLINE_INCLUDE_DIR_SUFFIX == readline
#include <readline/readline.h>
#include <readline/history.h>
#else
#include <editline/readline.h>
#include <editline/history.h>
#endif

static pic_value
pic_rl_readline(pic_state *pic)
{
  char *prompt, *result;
  
  pic_get_args(pic, "z", &prompt);

  result = readline(prompt);

  if(result)
    return pic_obj_value(pic_make_str_cstr(pic, result));
  else
    return pic_eof_object();
}

static pic_value
pic_rl_history_length(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_int_value(history_get_history_state()->length);
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
pic_rl_stifle_history(pic_state *pic)
{
  int  i;
  
  pic_get_args(pic, "i", &i);

  stifle_history(i);

  return pic_undef_value();
}

static pic_value
pic_rl_unstifle_history(pic_state *pic)
{
  pic_get_args(pic, "");

  unstifle_history();
  
  return pic_undef_value();
}

static pic_value
pic_rl_history_is_stifled(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_bool_value(history_is_stifled());
}

static pic_value
pic_rl_where_history(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_int_value(where_history());
}

static pic_value
pic_rl_current_history(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_obj_value(pic_make_str_cstr(pic, current_history()->line));
}

static pic_value
pic_rl_history_get(pic_state *pic)
{
  int i;
  HIST_ENTRY *e;

  pic_get_args(pic, "i", &i);
  
  e = history_get(i);

  return e ? pic_obj_value(pic_make_str_cstr(pic, e->line))
    : pic_false_value();
}

static pic_value
pic_rl_remove_history(pic_state *pic)
{
  int i;
  HIST_ENTRY *e;

  pic_get_args(pic, "i", &i);
  
  e = remove_history(i);

  return e ? pic_obj_value(pic_make_str_cstr(pic, e->line))
    : pic_false_value();
}

static pic_value
pic_rl_clear_history(pic_state *pic)
{
  pic_get_args(pic, "");

  clear_history();

  return pic_undef_value();
}

static pic_value
pic_rl_history_set_pos(pic_state *pic)
{
  int i;

  pic_get_args(pic, "i", &i);


  return pic_int_value(history_set_pos(i));
}

static pic_value
pic_rl_previous_history(pic_state *pic)
{
  HIST_ENTRY *e;

  pic_get_args(pic, "");

  e = previous_history();

  return e ? pic_obj_value(pic_make_str_cstr(pic, e->line))
    : pic_false_value();
}

static pic_value
pic_rl_next_history(pic_state *pic)
{
  HIST_ENTRY *e;

  pic_get_args(pic, "");

  e = next_history();

  return e ? pic_obj_value(pic_make_str_cstr(pic, e->line))
    : pic_false_value();
}

static pic_value
pic_rl_history_search(pic_state *pic)
{
  char *key;
  int direction, pos, argc;

  argc = pic_get_args(pic, "zi|i", &key, &direction, &pos);
  if(argc == 2)
    return pic_int_value(history_search(key, direction));
  else
    return pic_int_value(history_search_pos(key, direction, pos));
}

static pic_value
pic_rl_history_search_prefix(pic_state *pic)
{
  char *key;
  int direction;

  pic_get_args(pic, "zi", &key, &direction);

  return pic_int_value(history_search_prefix(key, direction));
}

static pic_value
pic_rl_read_history(pic_state *pic)
{
  char *filename;

  pic_get_args(pic, "z", &filename);

  if(read_history(filename))
    pic_errorf(pic, "cannot read history file : %s", filename);
  
  return pic_undef_value();
}

static pic_value
pic_rl_write_history(pic_state *pic)
{
  char *filename;

  pic_get_args(pic, "z", &filename);

  if(write_history(filename))
    pic_errorf(pic, "cannot write history file: %s", filename);
  
  return pic_undef_value();
}

static pic_value
pic_rl_truncate_file(pic_state *pic)
{
  char *filename;
  int i;

  pic_get_args(pic, "zi", &filename, &i);

  history_truncate_file(filename, i);
  
  return pic_undef_value();
}

static pic_value
pic_rl_history_expand(pic_state *pic)
{
  char *input, *result;
  int status;

  pic_get_args(pic, "z", &input);
  
  status = history_expand(input,  &result);
  if(status == -1 || status == 2)
    pic_errorf(pic, "%s\n", result);

  return pic_obj_value(pic_make_str_cstr(pic, result));
}

void
pic_init_readline(pic_state *pic){
  using_history();
  pic_deflibrary (pic, "(picrin readline)") {
    pic_defun(pic, "readline", pic_rl_readline);
  }
  pic_deflibrary (pic, "(picrin readline history)") {
    /* pic_defun(pic, "history-offset", pic_rl_history_offset); */
    pic_defun(pic, "history-length", pic_rl_history_length);
    pic_defun(pic, "add-history", pic_rl_add_history);
    pic_defun(pic, "stifle-history", pic_rl_stifle_history);
    pic_defun(pic, "unstifle-history", pic_rl_unstifle_history);
    pic_defun(pic, "history-stifled?", pic_rl_history_is_stifled);
    pic_defun(pic, "where-history", pic_rl_where_history);
    pic_defun(pic, "current-history", pic_rl_current_history);
    pic_defun(pic, "history-get", pic_rl_history_get);
    pic_defun(pic, "clear-history", pic_rl_clear_history);
    pic_defun(pic, "remove-history", pic_rl_remove_history);
    pic_defun(pic, "history-set-pos", pic_rl_history_set_pos);
    pic_defun(pic, "previous-history", pic_rl_previous_history);
    pic_defun(pic, "next-history", pic_rl_next_history);
    pic_defun(pic, "history-search", pic_rl_history_search);
    pic_defun(pic, "history-search-prefix", pic_rl_history_search_prefix);
    pic_defun(pic, "read-history", pic_rl_read_history);
    pic_defun(pic, "write-history", pic_rl_write_history);
    pic_defun(pic, "truncate-file", pic_rl_truncate_file);
    pic_defun(pic, "history-expand", pic_rl_history_expand);
  }
}
