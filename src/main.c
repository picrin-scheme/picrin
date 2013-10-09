#include <stdio.h>

int
main()
{
  char line[256], last_char;
  int char_index;

  while (1) {
    printf("> ");

    char_index = 0;
    while ((last_char = getchar()) != '\n') {
      if (last_char == EOF)
	goto eof;
      line[char_index++] = last_char;
    }
    line[char_index] = '\0';

    /* echo */
    printf("%s", line);

    printf("\n");
  }

 eof:
  puts("");

  return 0;
}
