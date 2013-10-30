#include <stdio.h>
#include <unistd.h>

#include "picrin.h"
#include "picrin/pair.h"

#if PIC_ENABLE_READLINE
# include <string.h>
# include <stdlib.h>
# include <readline/readline.h>
# include <readline/history.h>
#endif

#define CODE_MAX_LENGTH 1024
#define LINE_MAX_LENGTH 256

static char *fname;

void
print_help(void)
{
  const char *help =
    "picrin scheme\n"
    "\n"
    "Usage: picrin [options] [file]\n"
    "\n"
    "Options:\n"
    "  -h			show this help";

  puts(help);
}

bool
parse_opt(int argc, char *argv[])
{
  int r;

  while (~(r = getopt(argc, argv, "h"))) {
    switch (r) {
    case 'h':
      print_help();
      exit(0);
    }
  }
  argc -= optind;
  argv += optind;

  if (argc == 0) {
    return 1;
  }
  else {
    fname = argv[0];
    return 0;
  }
}

int
repl(pic_state *pic)
{
  char code[CODE_MAX_LENGTH] = "", line[LINE_MAX_LENGTH];
  char *read_line, *prompt;
  pic_value v, vs;
  struct pic_proc *proc;
  int ai, n, i;

#if ! PIC_ENABLE_READLINE
  char last_char;
  int char_index;
#endif

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
    n = pic_parse_cstr(pic, code, &vs);
    if (n == 0) {		/* wait for more input */
      goto next;
    }
    code[0] = '\0';
    if (n == -1) {		/* parse error */
      goto next;
    }

    for (i = 0; i < n; ++i) {
      v = pic_car(pic, vs);

#if DEBUG
    printf("[read: ");
    pic_debug(pic, v);
    printf("]\n");
#endif

      /* eval */
      proc = pic_codegen(pic, v);
      if (proc == NULL) {
	printf("compilation error: %s\n", pic->errmsg);
	pic->errmsg = NULL;
	goto next;
      }
      v = pic_apply(pic, proc, pic_nil_value());
      if (pic_undef_p(v)) {
	printf("runtime error: %s\n", pic->errmsg);
	pic->errmsg = NULL;
	goto next;
      }

      /* print */
      printf("=> ");
      pic_debug(pic, v);
      printf("\n");

      vs = pic_cdr(pic, vs);
    }

  next:
    pic_gc_arena_restore(pic, ai);
  }

 eof:
  puts("");
  return 0;

 overflow:
  puts("** [fatal] line input overflow");
  return 1;
}

int
exec_file(pic_state *pic, const char *fname)
{
  FILE *file;
  bool r;
  pic_value v;
  struct pic_proc *proc;

  file = fopen(fname, "r");
  if (file == NULL) {
    fprintf(stderr, "fatal error: could not read %s\n", fname);
    return 1;
  }

  r = pic_parse_file(pic, file, &v);
  if (! r) {
    fprintf(stderr, "fatal error: %s broken\n", fname);
    return 1;
  }

  proc = pic_codegen(pic, v);
  if (proc == NULL) {
    fputs(pic->errmsg, stderr);
    fprintf(stderr, "fatal error: %s compilation failure\n", fname);
    return 1;
  }

  v = pic_apply(pic, proc, pic_nil_value());
  if (pic_undef_p(v)) {
    fputs(pic->errmsg, stderr);
    fprintf(stderr, "fatal error: %s evaluation failure\n", fname);
    return 1;
  }

  return 0;
}

int
main(int argc, char *argv[], char **envp)
{
  pic_state *pic;
  int res;

  pic = pic_open(argc, argv, envp);

  if (parse_opt(argc, argv)) {
    res = repl(pic);
  }
  else {
    res = exec_file(pic, fname);
  }

  pic_close(pic);

  return res;
}
