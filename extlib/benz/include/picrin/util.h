/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_UTIL_H
#define PICRIN_UTIL_H

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

#if defined(__cplusplus)
}
#endif

#endif
