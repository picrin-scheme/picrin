/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_COMPAT_H
#define PICRIN_COMPAT_H

#if defined(__cplusplus)
extern "C" {
#endif

#if PIC_ENABLE_LIBC

#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <stdlib.h>

#else

# define assert(v) 0

PIC_INLINE int
isspace(int c)
{
  return c == ' ' || c == '\t' || c == '\r' || c == '\v' || c == '\f' || c == '\n';
}

PIC_INLINE int
tolower(int c)
{
  return ('A' <= c && c <= 'Z') ? c - 'A' + 'a' : c;
}

PIC_INLINE int
isdigit(int c)
{
  return '0' <= c && c <= '9';
}

PIC_INLINE char *
strchr(const char *s, int c)
{
  do {
    if (*s == c)
      return (char *)s;
  } while (*s++ != '\0');
  return NULL;
}

PIC_INLINE size_t
strlen(const char *s)
{
  size_t l = 0;

  while (*s++) {
    l++;
  }
  return l;
}

PIC_INLINE int
strcmp(const char *s1, const char *s2)
{
  while (*s1 && *s1 == *s2) {
    s1++;
    s2++;
  }
  return (unsigned)*s1 - (unsigned)*s2;
}

PIC_INLINE long
strtol(const char *nptr, char **endptr, int base)
{
  long l = 0;
  char c;
  int n;

  while (1) {
    c = *nptr;
    if ('0' <= c && c <= '9')
      n = c - '0';
    else if ('a' <= c && c <= 'z')
      n = c - 'a' + 10;
    else if ('A' <= c && c <= 'Z')
      n = c - 'A' + 10;
    else
      goto exit;

    if (base <= n)
      goto exit;

    l = l * base + n;
    nptr++;
  }
 exit:
  if (endptr)
    *endptr = (char *)nptr;
  return l;
}

PIC_INLINE void *
memset(void *s, int n, size_t c)
{
  char *p = s;

  while (c-- > 0) {
    *p++ = n;
  }
  return s;
}

PIC_INLINE void *
memcpy(void *dst, const void *src, size_t n)
{
  const char *s = src;
  char *d = dst;

  while (n-- > 0) {
    *d++ = *s++;
  }
  return d;
}

PIC_INLINE char *
strcpy(char *dst, const char *src)
{
  char *d = dst;

  while ((*dst++ = *src++) != 0);

  return d;
}

#endif

#if defined(__cplusplus)
}
#endif

#endif
