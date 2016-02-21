/**
 * See Copyright Notice in picrin.h
 */

/** switch normal VM and direct threaded VM */
/* #define PIC_DIRECT_THREADED_VM 1 */

/** switch internal value representation */
/* #define PIC_NAN_BOXING 1 */

/** no dependency on libc */
/* #define PIC_ENABLE_LIBC 1 */

/** use stdio or not */
/* #define PIC_ENABLE_STDIO 1 */

/** custom setjmp/longjmp */
/* #define PIC_JMPBUF jmp_buf */
/* #define PIC_SETJMP(pic, buf) setjmp(buf) */
/* #define PIC_LONGJMP(pic, buf, val) longjmp((buf), (val)) */

/** custom abort */
/* #define PIC_ABORT(pic) abort() */

/** initial memory size (to be dynamically extended if necessary) */
/* #define PIC_ARENA_SIZE 1000 */

/* #define PIC_HEAP_PAGE_SIZE 10000 */

/* #define PIC_PAGE_REQUEST_THRESHOLD(total) ((total) * 77 / 100) */

/* #define PIC_STACK_SIZE 1024 */

/* #define PIC_RESCUE_SIZE 30 */

/* #define PIC_SYM_POOL_SIZE 128 */

/* #define PIC_IREP_SIZE 8 */

/* #define PIC_POOL_SIZE 8 */

/* #define PIC_SYMS_SIZE 32 */

/* #define PIC_ISEQ_SIZE 1024 */
