/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_UTIL_H
#define PICRIN_UTIL_H

#if defined(__cplusplus)
extern "C" {
#endif

#if __STDC_VERSION__ >= 201112L
# include <stdnoreturn.h>
# define pic_noreturn noreturn
#elif __GNUC__ || __clang__
# define pic_noreturn __attribute__((noreturn))
#else
# define pic_noreturn
#endif

#define PIC_FALLTHROUGH ((void)0)
#define PIC_UNUSED(v) ((void)(v))

#define PIC_GENSYM2_(x,y) PIC_G##x##_##y##_
#define PIC_GENSYM1_(x,y) PIC_GENSYM2_(x,y)
#if defined(__COUNTER__)
# define PIC_GENSYM(x) PIC_GENSYM1_(__COUNTER__,x)
#else
# define PIC_GENSYM(x) PIC_GENSYM1_(__LINE__,x)
#endif

#if GCC_VERSION >= 40500 || __clang__
# define PIC_UNREACHABLE() (__builtin_unreachable())
#else
# define PIC_UNREACHABLE() (assert(false))
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
