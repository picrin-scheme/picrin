#include <stdio.h>

#include "picrin.h"

#if PIC_ENABLE_READLINE
# include <string.h>
# include <stdlib.h>
# include <readline/readline.h>
# include <readline/history.h>
#endif

#define CODE_MAX_LENGTH 1024
#define LINE_MAX_LENGTH 256

int
main(int argc, char *argv[], char **envp)
{
  pic_state *pic;
  char code[CODE_MAX_LENGTH] = "", line[LINE_MAX_LENGTH];
  char *read_line, *prompt;
  pic_value v;
  struct pic_proc *proc;
  int ai;
  bool r;

#if ! PIC_ENABLE_READLINE
  char last_char;
  int char_index;
#endif

  pic = pic_open(argc, argv, envp);

  ai = pic_gc_arena_preserve(pic);

  while (1) {
    prompt = code[0] == '\0' ? "> " : "* ";

#if PIC_ENABLE_READLINE
    read_line = readline(prompt);
    if (read_line == NULL) {
      goto eof;
    }
    else {
      strncpy(line, read_line, LINE_MAX_LENGTH - 1);
      add_history(read_line);
      free(read_line);
    }
#else
    printf(prompt);

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

    if (strlen(code) + strlen(line) >= CODE_MAX_LENGTH)
      goto overflow;
    strcat(code, line);

    /* read */
    r = pic_parse(pic, code, &v);
    if (! r) {			/* wait for more input */
      goto next;
    }
    code[0] = '\0';
    if (pic_undef_p(v)) {	/* parse error */
      goto next;
    }

#if DEBUG
    printf("[read: ");
    pic_debug(pic, v);
    printf("]\n");
#endif

    /* eval */
    proc = pic_codegen(pic, v, pic->global_env);
    if (proc == NULL) {
      printf("compilation error: %s\n", pic->errmsg);
      pic->errmsg = NULL;
      goto next;
    }
    v = pic_run(pic, proc, pic_nil_value());
    if (pic_undef_p(v)) {
      printf("runtime error: %s\n", pic->errmsg);
      pic->errmsg = NULL;
      goto next;
    }

    /* print */
    printf("=> ");
    pic_debug(pic, v);
    printf("\n");

  next:
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
