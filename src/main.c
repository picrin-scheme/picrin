#include <stdio.h>

#include "picrin.h"

#if PIC_ENABLE_READLINE
# include <string.h>
# include <stdlib.h>
# include <readline/readline.h>
# include <readline/history.h>
#endif

void
test_object_creation(pic_state *pic)
{
  pic_value v;

  {
    v = pic_intern_cstr(pic, "symbol");
    pic_debug(pic, v);
    puts(" [should be `symbol`]");
  }
  {
    v = pic_nil_value();
    pic_debug(pic, v);
    puts(" [should be `()`]");
  }
  {
    v = pic_cons(pic, pic_intern_cstr(pic, "foo"), pic_intern_cstr(pic, "bar"));
    pic_debug(pic, v);
    puts(" [should be `(foo . bar)`]");
  }
}

#define LINE_MAX_LENGTH 256

int
main()
{
  pic_state *pic;
  char line[LINE_MAX_LENGTH], last_char, *read_line;
  int char_index;
  pic_value v;
  struct pic_proc *proc;
  int ai;

  pic = pic_open();

#if OBJECT_CREATION_DEBUG
  test_object_creation(pic);
#endif

  ai = pic_gc_arena_preserve(pic);

  while (1) {

#if PIC_ENABLE_READLINE
    read_line = readline("> ");
    if (read_line == NULL) {
      line[0] = '\0';
    }
    else {
      strncpy(line, read_line, LINE_MAX_LENGTH - 1);
      add_history(read_line);
      free(read_line);
    }
#else
    printf("> ");

    char_index = 0;
    while ((last_char = getchar()) != '\n') {
      if (last_char == EOF)
	goto eof;
      if (char_index == LINE_MAX_LENGTH)
	goto overflow;
      line[char_index++] = last_char;
    }
    line[char_index] = '\0';
#endif

    /* read */
    v = pic_parse(pic, line);

#if DEBUG
    printf("[read: ");
    pic_debug(pic, v);
    printf("]\n");
#endif

    if (pic_undef_p(v)) {
      pic_gc_arena_restore(pic, ai);
      continue;
    }

    /* eval */
    proc = pic_codegen(pic, v, pic->global_env);
    v = pic_run(pic, proc, pic_nil_value());

    /* print */
    printf("=> ");
    pic_debug(pic, v);
    printf("\n");

    pic_gc_arena_restore(pic, ai);
  }

 eof:
  puts("");
  goto exit;

 overflow:
  puts("** [fatal] line input overflow");
  goto exit;

 exit:
  pic_close(pic);

  return 0;
}
