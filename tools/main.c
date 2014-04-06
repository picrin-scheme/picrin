/**
 * See Copyright Notice in picrin.h
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/error.h"

#if PIC_ENABLE_READLINE
# include <readline/readline.h>
# include <readline/history.h>
#endif

#define CODE_MAX_LENGTH 1024
#define LINE_MAX_LENGTH 256

void
print_help(void)
{
  const char *help =
    "picrin scheme\n"
    "\n"
    "Usage: picrin [options] [file]\n"
    "\n"
    "Options:\n"
    "  -e [program]             run one liner ecript\n"
    "  -h                       show this help";

  puts(help);
}

void
import_repllib(pic_state *pic)
{
  int ai = pic_gc_arena_preserve(pic);

  pic_import(pic, pic_read(pic, "(scheme base)"));
  pic_import(pic, pic_read(pic, "(scheme load)"));
  pic_import(pic, pic_read(pic, "(scheme process-context)"));
  pic_import(pic, pic_read(pic, "(scheme write)"));
  pic_import(pic, pic_read(pic, "(scheme file)"));
  pic_import(pic, pic_read(pic, "(scheme inexact)"));
  pic_import(pic, pic_read(pic, "(scheme cxr)"));
  pic_import(pic, pic_read(pic, "(scheme lazy)"));
  pic_import(pic, pic_read(pic, "(scheme time)"));
  pic_import(pic, pic_read(pic, "(picrin macro)"));

#if DEBUG
  puts("* imported repl libraries");
#endif

  pic_gc_arena_restore(pic, ai);
}

int exit_status;

void
repl(pic_state *pic)
{
  char code[CODE_MAX_LENGTH] = "", line[LINE_MAX_LENGTH];
  char *prompt;
  pic_value v, exprs;
  int ai;

#if PIC_ENABLE_READLINE
  char *read_line;
#else
  char last_char;
  int char_index;
#endif

#if PIC_ENABLE_READLINE
  using_history();

  char histfile[snprintf(NULL, 0, "%s/.picrin_history", getenv("HOME")) + 1];
  sprintf(histfile, "%s/.picrin_history", getenv("HOME"));
  read_history(histfile);
#endif

  ai = pic_gc_arena_preserve(pic);

  while (1) {
    prompt = code[0] == '\0' ? "> " : "* ";

#if DEBUG
    printf("[current ai = %d]\n", ai);
#endif

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
    printf("%s", prompt);

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

    pic_try {

      /* read */
      exprs = pic_parse_cstr(pic, code);

      if (pic_undef_p(exprs)) {
        /* wait for more input */
      }
      else {
        code[0] = '\0';

        pic_for_each (v, exprs) {

          /* eval */
          v = pic_eval(pic, v);

          /* print */
          pic_printf(pic, "=> ~s\n", v);
        }
      }
    }
    pic_catch {
      pic_print_backtrace(pic, pic->err);
      pic->err = NULL;
      pic->ci = pic->cibase;
      code[0] = '\0';
    }

    pic_gc_arena_restore(pic, ai);
  }

 eof:
  puts("");
  exit_status = 0;
#if PIC_ENABLE_READLINE
  write_history(histfile);
#endif
  return;

 overflow:
  puts("** [fatal] line input overflow");
  exit_status = 1;
  return;
}

void
exec_file(pic_state *pic, const char *fname)
{
  FILE *file;
  pic_value v, exprs;
  struct pic_proc *proc;

  file = fopen(fname, "r");
  if (file == NULL) {
    fprintf(stderr, "fatal error: could not read %s\n", fname);
    goto abort;
  }

  exprs = pic_parse_file(pic, file);
  if (pic_undef_p(exprs)) {
    fprintf(stderr, "fatal error: %s broken\n", fname);
    goto abort;
  }

  pic_for_each (v, exprs) {

    proc = pic_compile(pic, v);
    if (proc == NULL) {
      fputs(pic_errmsg(pic), stderr);
      fprintf(stderr, "fatal error: %s compilation failure\n", fname);
      goto abort;
    }

    v = pic_apply(pic, proc, pic_nil_value());
    if (pic_undef_p(v)) {
      fputs(pic_errmsg(pic), stderr);
      fprintf(stderr, "fatal error: %s evaluation failure\n", fname);
      goto abort;
    }

  }

  return;

 abort:
  exit_status = 1;
  return;
}

void
exec_string(pic_state *pic, const char *str)
{
  pic_value v, exprs;
  struct pic_proc *proc;
  int ai;

  exprs = pic_parse_cstr(pic, str);
  if (pic_undef_p(exprs)) {
    goto abort;
  }

  ai = pic_gc_arena_preserve(pic);
  pic_for_each (v, exprs) {

    proc = pic_compile(pic, v);
    if (proc == NULL) {
      goto abort;
    }
    v = pic_apply(pic, proc, pic_nil_value());
    if (pic_undef_p(v)) {
      goto abort;
    }

    pic_gc_arena_restore(pic, ai);
  }

  return;

 abort:
  exit_status = 1;
  return;
}

static char *fname;
static char *script;

enum {
  NO_MODE = 0,
  INTERACTIVE_MODE,
  FILE_EXEC_MODE,
  ONE_LINER_MODE,
} mode;

void
parse_opt(int argc, char *argv[])
{
  int r;

  while (~(r = getopt(argc, argv, "he:"))) {
    switch (r) {
    case 'h':
      print_help();
      exit(0);
    case 'e':
      script = optarg;
      mode = ONE_LINER_MODE;
    }
  }
  argc -= optind;
  argv += optind;

  if (argc == 0) {
    if (mode == NO_MODE)
      mode = INTERACTIVE_MODE;
  }
  else {
    fname = argv[0];
    mode = FILE_EXEC_MODE;
  }
}

int
main(int argc, char *argv[], char **envp)
{
  pic_state *pic;

  pic = pic_open(argc, argv, envp);

  parse_opt(argc, argv);

  if (mode == INTERACTIVE_MODE || mode == ONE_LINER_MODE) {
    import_repllib(pic);
  }

  switch (mode) {
  case NO_MODE:
    puts("logic flaw");
    abort();
  case INTERACTIVE_MODE:
    repl(pic);
    break;
  case FILE_EXEC_MODE:
    exec_file(pic, fname);
    break;
  case ONE_LINER_MODE:
    exec_string(pic, script);
    break;
  }

  pic_close(pic);

#if DEBUG
  puts("* picrin successfully closed");
#endif

  return exit_status;
}
