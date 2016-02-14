/**
 * See Copyright Notice in picrin.h
 */

#include "picrin/config.h"

#ifndef PIC_DIRECT_THREADED_VM
# if (defined(__GNUC__) || defined(__clang__)) && __STRICT_ANSI__ != 1
#  define PIC_DIRECT_THREADED_VM 1
# endif
#endif

#if PIC_NAN_BOXING && PIC_WORD_BOXING
# error cannot enable both PIC_NAN_BOXING and PIC_WORD_BOXING simultaneously
#endif

#ifndef PIC_WORD_BOXING
# define PIC_WORD_BOXING 0
#endif

#if ! PIC_WORD_BOXING
# ifndef PIC_NAN_BOXING
#  if __x86_64__ && (defined(__GNUC__) || defined(__clang__)) && __STRICT_ANSI__ != 1
#   define PIC_NAN_BOXING 1
#  endif
# endif
#endif

#ifndef PIC_ENABLE_LIBC
# define PIC_ENABLE_LIBC 1
#endif

#ifndef PIC_ENABLE_STDIO
# define PIC_ENABLE_STDIO 1
#endif

#ifndef PIC_JMPBUF
# include <setjmp.h>
# define PIC_JMPBUF jmp_buf
#endif

#ifndef PIC_SETJMP
# include <setjmp.h>
# define PIC_SETJMP(pic, buf) setjmp(buf)
#endif

#ifndef PIC_LONGJMP
# include <setjmp.h>
# define PIC_LONGJMP(pic, buf, val) longjmp((buf), (val))
#endif

#ifndef PIC_ABORT
# define PIC_ABORT(pic) abort()
#endif

#ifndef PIC_ARENA_SIZE
# define PIC_ARENA_SIZE (8 * 1024)
#endif

#ifndef PIC_HEAP_PAGE_SIZE
# define PIC_HEAP_PAGE_SIZE (4 * 1024 * 1024)
#endif

#ifndef PIC_PAGE_REQUEST_THRESHOLD
# define PIC_PAGE_REQUEST_THRESHOLD(total) ((total) * 77 / 100)
#endif

#ifndef PIC_STACK_SIZE
# define PIC_STACK_SIZE 2048
#endif

#ifndef PIC_RESCUE_SIZE
# define PIC_RESCUE_SIZE 30
#endif

#ifndef PIC_SYM_POOL_SIZE
# define PIC_SYM_POOL_SIZE (2 * 1024)
#endif

#ifndef PIC_IREP_SIZE
# define PIC_IREP_SIZE 8
#endif

#ifndef PIC_POOL_SIZE
# define PIC_POOL_SIZE 8
#endif

#ifndef PIC_SYMS_SIZE
# define PIC_SYMS_SIZE 32
#endif

#ifndef PIC_ISEQ_SIZE
# define PIC_ISEQ_SIZE 1024
#endif

#if DEBUG
# include <stdio.h>
# define GC_STRESS 0
# define VM_DEBUG 1
# define GC_DEBUG 0
# define GC_DEBUG_DETAIL 0
#endif

#include "picrin/compat.h"
