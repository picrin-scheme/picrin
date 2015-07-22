void abort()
{
  while (1);
}

typedef char jmp_buf[1];

int setjmp(jmp_buf buf)
{
  (void)buf;
  return 0;
}

void longjmp(jmp_buf buf, int r)
{
  (void)buf;
  (void)r;
  while (1);
}

