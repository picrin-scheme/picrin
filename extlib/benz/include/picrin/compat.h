/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_COMPAT_H
#define PICRIN_COMPAT_H

#if defined(__cplusplus)
extern "C" {
#endif

#if __STDC_VERSION__ >= 199901L
# include <stdbool.h>
#else
# define bool char
# define true 1
# define false 0
#endif

#if __STDC_VERSION__ >= 199901L
# include <stddef.h>
#elif ! defined(offsetof)
# define offsetof(s,m) ((size_t)&(((s *)NULL)->m))
#endif

#if __STDC_VERSION__ >= 199901L
# include <stdint.h>
#else
# if INT_MAX > 2147483640L      /* borrowed from luaconf.h */
typedef int int32_t;
typedef unsigned int uint32_t;
# else
typedef long int32_t;
typedef unsigned long uint32_t;
# endif
#endif

#if __STDC_VERSION__ >= 201112L
# include <stdnoreturn.h>
# define PIC_NORETURN noreturn
#elif __GNUC__ || __clang__
# define PIC_NORETURN __attribute__((noreturn))
#else
# define PIC_NORETURN
#endif

#if __STDC_VERSION__ >= 199901L
# define PIC_INLINE static inline
#elif __GNUC__ || __clang__
# define PIC_INLINE static __inline__
#else
# define PIC_INLINE static
#endif

#define PIC_FALLTHROUGH ((void)0)

#if __GNUC__ || __clang__
# define PIC_UNUSED(v) __attribute__((unused)) v
#else
# define PIC_UNUSED(v) v
#endif

#define PIC_GENSYM2_(x,y) PIC_G##x##_##y##_
#define PIC_GENSYM1_(x,y) PIC_GENSYM2_(x,y)
#if defined(__COUNTER__)
# define PIC_GENSYM(x) PIC_GENSYM1_(__COUNTER__,x)
#else
# define PIC_GENSYM(x) PIC_GENSYM1_(__LINE__,x)
#endif

#if __GNUC__
# define GCC_VERSION (__GNUC__ * 10000          \
   + __GNUC_MINOR__ * 100                       \
   + __GNUC_PATCHLEVEL__)
#endif
#if GCC_VERSION >= 40500 || __clang__
# define PIC_UNREACHABLE() (__builtin_unreachable())
#else
# define PIC_UNREACHABLE() (assert(false))
#endif
#if __GNUC__
# undef GCC_VERSION
#endif

#define PIC_SWAP(type,a,b)                      \
  PIC_SWAP_HELPER_(type, PIC_GENSYM(tmp), a, b)
#define PIC_SWAP_HELPER_(type,tmp,a,b)          \
  do {                                          \
    type tmp = (a);                             \
    (a) = (b);                                  \
    (b) = tmp;                                  \
  } while (0)


#if PIC_ENABLE_LIBC

#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <stdlib.h>

#else

# define assert(v) (void)0

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

#if PIC_ENABLE_STDIO
# include <stdio.h>
#endif

#if defined(__cplusplus)
}
#endif

#endif
