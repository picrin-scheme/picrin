#include <stdio.h>

#include "picrin.h"

#define LINE_MAX_LENGTH 256

int
main()
{
  pic_state *pic;
  char line[LINE_MAX_LENGTH], last_char;
  int char_index;

  pic = pic_open();

  while (1) {
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

    /* echo */
    printf("%s", line);

    printf("\n");
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
