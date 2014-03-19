/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_UTIL_H__
#define PICRIN_UTIL_H__

#if defined(__cplusplus)
extern "C" {
#endif

#if __STDC_VERSION__ >= 201112L
# define noreturn _Noreturn
#elif __GNUC__ || __clang__
# define noreturn __attribute__((noreturn))
#endif

#define FALLTHROUGH ((void)0)
#define UNUSED(v) ((void)(v))

#define GENSYM2__(x,y) x##y
#define GENSYM1__(x,y) GENSYM2__(x,y)
#if defined(__COUNTER__)
# define GENSYM(x) GENSYM1__(x,__COUNTER__)
#else
# define GENSYM(x) GENSYM1__(x,__LINE__)
#endif

#if __GNUC__ || __clang__
# define UNREACHABLE() (__builtin_unreachable())
#else
# include <assert.h>
# define UNREACHABLE() (assert(false))
#endif

#if defined(__cplusplus)
}
#endif

#endif
