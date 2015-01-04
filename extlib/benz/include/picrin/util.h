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
#elif __GNUC__ || __clang__
# define noreturn __attribute__((noreturn))
#else
# define noreturn
#endif

#define FALLTHROUGH ((void)0)
#define UNUSED(v) ((void)(v))

#define GENSYM2_(x,y) G##x##_##y##__
#define GENSYM1_(x,y) GENSYM2_(x,y)
#if defined(__COUNTER__)
# define GENSYM(x) GENSYM1_(__COUNTER__,x)
#else
# define GENSYM(x) GENSYM1_(__LINE__,x)
#endif

#if GCC_VERSION >= 40500 || __clang__
# define UNREACHABLE() (__builtin_unreachable())
#else
# include <assert.h>
# define UNREACHABLE() (assert(false))
#endif

#define SWAP(type,a,b)                          \
  SWAP_HELPER_(type,GENSYM(tmp),a,b)
#define SWAP_HELPER_(type,tmp,a,b)             \
  do {                                          \
    type tmp = (a);                             \
    (a) = (b);                                  \
    (b) = tmp;                                  \
  } while (0)

#if defined(__cplusplus)
}
#endif

#endif
