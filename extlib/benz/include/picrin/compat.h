/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_COMPAT_H
#define PICRIN_COMPAT_H

#if defined(__cplusplus)
extern "C" {
#endif

PIC_INLINE int
pic_isspace(int c)
{
  return c == ' ' || c == '\t' || c == '\r' || c == '\v' || c == '\f' || c == '\n';
}

PIC_INLINE int
pic_tolower(int c)
{
  return 'A' <= c && c <= 'Z' ? c - 'a' + 'A' : c;
}

PIC_INLINE int
pic_isdigit(int c)
{
  return '0' <= c && c <= '9';
}

PIC_INLINE char *
pic_strchr(const char *s, int c)
{
  do {
    if (*s == c)
      return (char *)s;
  } while (*s++ != '\0');
  return NULL;
}

#if defined(__cplusplus)
}
#endif

#endif
