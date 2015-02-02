/**
 * See Copyright Notice in picrin.h
 */

/** switch normal VM and direct threaded VM */
#define PIC_DIRECT_THREADED_VM 0

/** switch internal value representation */
#define PIC_NAN_BOXING 0

/** enable word boxing  */
#define PIC_WORD_BOXING 1

/** enable floating point number support */
#define PIC_ENABLE_FLOAT 0

/** no dependency on libc  */
#define PIC_ENABLE_LIBC 0

/** treat false value as none */
/* #define PIC_NONE_IS_FALSE 1 */

/** initial memory size (to be dynamically extended if necessary) */
#define PIC_ARENA_SIZE 100

#define PIC_HEAP_PAGE_SIZE 1024

#define PIC_STACK_SIZE 1024

#define PIC_RESCUE_SIZE 30

#define PIC_SYM_POOL_SIZE 128

#define PIC_IREP_SIZE 8

#define PIC_POOL_SIZE 8

#define PIC_SYMS_SIZE 32

#define PIC_ISEQ_SIZE 32

/** enable all debug flags */
/* #define DEBUG 1 */

/** auxiliary debug flags */
/* #define GC_STRESS 1 */
/* #define VM_DEBUG 1 */
/* #define GC_DEBUG 1 */
/* #define GC_DEBUG_DETAIL 1 */

#ifndef PIC_DIRECT_THREADED_VM
# if (defined(__GNUC__) || defined(__clang__)) && __STRICT_ANSI__ != 1
#  define PIC_DIRECT_THREADED_VM 1
# endif
#endif

#if PIC_NAN_BOXING && PIC_WORD_BOXING
# error cannot enable both PIC_NAN_BOXING and PIC_WORD_BOXING simultaneously
#endif

#if PIC_WORD_BOXING && PIC_ENABLE_FLOAT
# error cannot enable both PIC_WORD_BOXING and PIC_ENABLE_FLOAT simultaneously
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

#ifndef PIC_ENABLE_FLOAT
# define PIC_ENABLE_FLOAT 1
#endif

#ifndef PIC_ENABLE_LIBC
# define PIC_ENABLE_LIBC 1
#endif

#if PIC_NAN_BOXING && defined(PIC_ENABLE_FLOAT) && ! PIC_ENABLE_FLOAT
# error cannot disable float support when nan boxing is on
#endif

#ifndef PIC_NONE_IS_FALSE
# define PIC_NONE_IS_FALSE 1
#endif

#ifndef PIC_ARENA_SIZE
# define PIC_ARENA_SIZE (8 * 1024)
#endif

#ifndef PIC_HEAP_PAGE_SIZE
# define PIC_HEAP_PAGE_SIZE (2 * 1024 * 1024)
#endif

#ifndef PIC_STACK_SIZE
# define PIC_STACK_SIZE 1024
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
# define GC_STRESS 0
# define VM_DEBUG 1
# define GC_DEBUG 0
# define GC_DEBUG_DETAIL 0
#endif
