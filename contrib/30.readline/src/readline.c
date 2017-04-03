/*
  cc -g test.c -o test -ledit -ltermcap
*/

/* This will include all our libedit functions.  If you use C++ don't
forget to use the C++ extern "C" to get it to compile.
*/
#include "picrin.h"
#include "picrin/extra.h"

#include <editline/readline.h>

static pic_value
pic_rl_readline(pic_state *pic)
{
  char *prompt, *result;
  
  pic_get_args(pic, "z", &prompt);

  result = readline(prompt);

  if(result)
    return pic_cstr_value(pic, result);
  else
    return pic_eof_object(pic);
}

static pic_value
pic_rl_history_length(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_int_value(pic, history_get_history_state()->length);
}

static pic_value
pic_rl_add_history(pic_state *pic)
{
  char *line;

  pic_get_args(pic, "z", &line);

  add_history(line);

  return pic_undef_value(pic);
}

static pic_value
pic_rl_stifle_history(pic_state *pic)
{
  int  i;
  
  pic_get_args(pic, "i", &i);

  stifle_history(i);

  return pic_undef_value(pic);
}

static pic_value
pic_rl_unstifle_history(pic_state *pic)
{
  pic_get_args(pic, "");

  unstifle_history();
  
  return pic_undef_value(pic);
}

static pic_value
pic_rl_history_is_stifled(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_bool_value(pic, history_is_stifled());
}

static pic_value
pic_rl_where_history(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_int_value(pic, where_history());
}

static pic_value
pic_rl_current_history(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_cstr_value(pic, current_history()->line);
}

static pic_value
pic_rl_history_get(pic_state *pic)
{
  int i;
  HIST_ENTRY *e;

  pic_get_args(pic, "i", &i);
  
  e = history_get(i);

  return e ? pic_cstr_value(pic, e->line) : pic_false_value(pic);
}

static pic_value
pic_rl_remove_history(pic_state *pic)
{
  int i;
  HIST_ENTRY *e;

  pic_get_args(pic, "i", &i);
  
  e = remove_history(i);

  return e ? pic_cstr_value(pic, e->line) : pic_false_value(pic);
}

static pic_value
pic_rl_clear_history(pic_state *pic)
{
  pic_get_args(pic, "");

  clear_history();

  return pic_undef_value(pic);
}

static pic_value
pic_rl_history_set_pos(pic_state *pic)
{
  int i;

  pic_get_args(pic, "i", &i);


  return pic_int_value(pic, history_set_pos(i));
}

static pic_value
pic_rl_previous_history(pic_state *pic)
{
  HIST_ENTRY *e;

  pic_get_args(pic, "");

  e = previous_history();

  return e ? pic_cstr_value(pic, e->line) : pic_false_value(pic);
}

static pic_value
pic_rl_next_history(pic_state *pic)
{
  HIST_ENTRY *e;

  pic_get_args(pic, "");

  e = next_history();

  return e ? pic_cstr_value(pic, e->line) : pic_false_value(pic);
}

static pic_value
pic_rl_history_search(pic_state *pic)
{
  char *key;
  int direction, pos, argc;

  argc = pic_get_args(pic, "zi|i", &key, &direction, &pos);
  if(argc == 2)
    return pic_int_value(pic, history_search(key, direction));
  else
    return pic_int_value(pic, history_search_pos(key, direction, pos));
}

static pic_value
pic_rl_history_search_prefix(pic_state *pic)
{
  char *key;
  int direction;

  pic_get_args(pic, "zi", &key, &direction);

  return pic_int_value(pic, history_search_prefix(key, direction));
}

static pic_value
pic_rl_read_history(pic_state *pic)
{
  char *filename;

  pic_get_args(pic, "z", &filename);

  if(read_history(filename))
    pic_error(pic, "cannot read history file", 1, pic_cstr_value(pic, filename));
  
  return pic_undef_value(pic);
}

static pic_value
pic_rl_write_history(pic_state *pic)
{
  char *filename;

  pic_get_args(pic, "z", &filename);

  if(write_history(filename))
    pic_error(pic, "cannot write history file", 1, pic_cstr_value(pic, filename));
  
  return pic_undef_value(pic);
}

static pic_value
pic_rl_truncate_file(pic_state *pic)
{
  char *filename;
  int i;

  pic_get_args(pic, "zi", &filename, &i);

  history_truncate_file(filename, i);
  
  return pic_undef_value(pic);
}

static pic_value
pic_rl_history_expand(pic_state *pic)
{
  char *input, *result;
  int status;

  pic_get_args(pic, "z", &input);
  
  status = history_expand(input,  &result);
  if(status == -1 || status == 2)
    pic_error(pic, result, 0);

  return pic_cstr_value(pic, result);
}

void
pic_nitro_init_readline(pic_state *pic){
  using_history();

  pic_deflibrary(pic, "picrin.readline");
  pic_in_library(pic, "picrin.readline");
  pic_export(pic, 1, "readline");

  pic_defun(pic, "picrin.readline:readline", pic_rl_readline);

  pic_deflibrary(pic, "picrin.readline.history");
  pic_in_library(pic, "picrin.readline.history");
  pic_export(pic, 19,
             "history-length", "add-history", "stifle-history",
             "unstifle-history", "history-stifled?",
             "where-history", "current-history", "history-get",
             "clear-history", "remove-history", "history-set-pos",
             "previous-history", "next-history", "history-search",
             "history-search-prefix", "read-history",
             "write-history", "truncate-file", "history-expand");

  /* pic_defun(pic, "picrin.readline.history:history-offset", pic_rl_history_offset); */
  pic_defun(pic, "picrin.readline.history:history-length", pic_rl_history_length);
  pic_defun(pic, "picrin.readline.history:add-history", pic_rl_add_history);
  pic_defun(pic, "picrin.readline.history:stifle-history", pic_rl_stifle_history);
  pic_defun(pic, "picrin.readline.history:unstifle-history", pic_rl_unstifle_history);
  pic_defun(pic, "picrin.readline.history:history-stifled?", pic_rl_history_is_stifled);
  pic_defun(pic, "picrin.readline.history:where-history", pic_rl_where_history);
  pic_defun(pic, "picrin.readline.history:current-history", pic_rl_current_history);
  pic_defun(pic, "picrin.readline.history:history-get", pic_rl_history_get);
  pic_defun(pic, "picrin.readline.history:clear-history", pic_rl_clear_history);
  pic_defun(pic, "picrin.readline.history:remove-history", pic_rl_remove_history);
  pic_defun(pic, "picrin.readline.history:history-set-pos", pic_rl_history_set_pos);
  pic_defun(pic, "picrin.readline.history:previous-history", pic_rl_previous_history);
  pic_defun(pic, "picrin.readline.history:next-history", pic_rl_next_history);
  pic_defun(pic, "picrin.readline.history:history-search", pic_rl_history_search);
  pic_defun(pic, "picrin.readline.history:history-search-prefix", pic_rl_history_search_prefix);
  pic_defun(pic, "picrin.readline.history:read-history", pic_rl_read_history);
  pic_defun(pic, "picrin.readline.history:write-history", pic_rl_write_history);
  pic_defun(pic, "picrin.readline.history:truncate-file", pic_rl_truncate_file);
  pic_defun(pic, "picrin.readline.history:history-expand", pic_rl_history_expand);
}
