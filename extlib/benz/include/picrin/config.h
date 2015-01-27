/**
 * See Copyright Notice in picrin.h
 */

/** switch normal VM and direct threaded VM */
/* #define PIC_DIRECT_THREADED_VM 1 */

/** switch internal value representation */
/* #define PIC_NAN_BOXING 1 */

/** treat false value as none */
/* #define PIC_NONE_IS_FALSE 1 */

/** initial memory size (to be dynamically extended if necessary) */
/* #define PIC_ARENA_SIZE 1000 */

/* #define PIC_HEAP_PAGE_SIZE 10000 */

/* #define PIC_STACK_SIZE 1024 */

/* #define PIC_RESCUE_SIZE 30 */

/* #define PIC_SYM_POOL_SIZE 128 */

/* #define PIC_IREP_SIZE 8 */

/* #define PIC_POOL_SIZE 8 */

/* #define PIC_SYMS_SIZE 32 */

/* #define PIC_ISEQ_SIZE 1024 */

/** enable all debug flags */
/* #define DEBUG 1 */

/** auxiliary debug flags */
/* #define GC_STRESS 1 */
/* #define VM_DEBUG 1 */
/* #define GC_DEBUG 1 */
/* #define GC_DEBUG_DETAIL 1 */

#if __STDC_VERSION__ < 199901L
# error please activate c99 features
#endif

#ifndef PIC_DIRECT_THREADED_VM
# if defined(__GNUC__) || defined(__clang__)
#  define PIC_DIRECT_THREADED_VM 1
# endif
#endif

#ifndef PIC_NAN_BOXING
# if __x86_64__ && __STDC_VERSION__ >= 201112L
#  define PIC_NAN_BOXING 1
# endif
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
