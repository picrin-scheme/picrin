/**
 * Copyright (c) 2012-2013 Yuichi Nishiwaki and other picrin contributors.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef PICRIN_H__
#define PICRIN_H__

#if defined(__cplusplus)
extern "C" {
#endif

#include <stddef.h>
#include <stdbool.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>

#include "xhash/xhash.h"
#include "xfile/xfile.h"
#include "xrope/xrope.h"

#if __STDC_VERSION__ >= 201112L
# define NORETURN _Noreturn
#elif __GNUC__ || __clang__
# define NORETURN __attribute__((noreturn))
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

#include "config.h"
#include "picrin/value.h"

struct pic_code;

typedef struct {
  int argc, retc;
  struct pic_code *ip;
  pic_value *fp;
  struct pic_env *env;
} pic_callinfo;

struct pic_block {
  struct pic_block *prev;
  int depth;
  struct pic_proc *in, *out;
  unsigned refcnt;
};

typedef struct {
  int argc;
  char **argv, **envp;

  struct pic_block *blk;

  pic_value *sp;
  pic_value *stbase, *stend;

  pic_callinfo *ci;
  pic_callinfo *cibase, *ciend;

  struct pic_code *ip;

  struct pic_proc **rescue;
  size_t ridx, rlen;

  pic_sym sDEFINE, sLAMBDA, sIF, sBEGIN, sQUOTE, sSETBANG;
  pic_sym sQUASIQUOTE, sUNQUOTE, sUNQUOTE_SPLICING;
  pic_sym sDEFINE_SYNTAX, sDEFINE_MACRO;
  pic_sym sDEFINE_LIBRARY, sIMPORT, sEXPORT;
  pic_sym sCONS, sCAR, sCDR, sNILP;
  pic_sym sADD, sSUB, sMUL, sDIV, sMINUS;
  pic_sym sEQ, sLT, sLE, sGT, sGE, sNOT;

  xhash *syms;                  /* name to symbol */
  xhash *sym_names;             /* symbol to name */
  int sym_cnt;
  int uniq_sym_cnt;

  xhash *global_tbl;
  pic_value *globals;
  size_t glen, gcapa;

  xhash *macros;

  pic_value lib_tbl;
  struct pic_lib *lib;

  jmp_buf *jmp;
  struct pic_error *err;

  struct pic_heap *heap;
  struct pic_object *arena[PIC_ARENA_SIZE];
  int arena_idx;

  pic_value *native_stack_start;
} pic_state;

typedef pic_value (*pic_func_t)(pic_state *);

void *pic_malloc(pic_state *, size_t);
#define pic_alloc(pic,size) pic_malloc(pic,size) /* obsoleted */
void *pic_realloc(pic_state *, void *, size_t);
void *pic_calloc(pic_state *, size_t, size_t);
struct pic_object *pic_obj_alloc(pic_state *, size_t, enum pic_tt);
struct pic_object *pic_obj_alloc_unsafe(pic_state *, size_t, enum pic_tt);
void pic_free(pic_state *, void *);

void pic_gc_run(pic_state *);
void pic_gc_protect(pic_state *, pic_value);
int pic_gc_arena_preserve(pic_state *);
void pic_gc_arena_restore(pic_state *, int);

pic_state *pic_open(int argc, char *argv[], char **envp);
void pic_close(pic_state *);

void pic_define(pic_state *, const char *, pic_value); /* symbol is automatically exported */
pic_value pic_ref(pic_state *, const char *);
void pic_set(pic_state *, const char *, pic_value);

#define pic_try                                                         \
  pic_try_helper__(GENSYM(pic_try_i__), GENSYM(pic_try_jmp__), GENSYM(pic_try_prev_jmp__))
#define pic_try_helper__(i, here, prev_jmp)                             \
  for (int i = 0; ! i; )                                                \
    for (jmp_buf here, *prev_jmp = pic->jmp; ! i; )                     \
      for (pic->jmp = &here; ! i++; pic->jmp = prev_jmp)                \
        if (setjmp(here) == 0)
#define pic_catch else

struct pic_proc *pic_get_proc(pic_state *);
int pic_get_args(pic_state *, const char *, ...);
void pic_defun(pic_state *, const char *, pic_func_t);
void pic_defmacro(pic_state *, const char *, struct pic_proc *);
void pic_defvar(pic_state *, const char *, pic_value);

bool pic_equal_p(pic_state *, pic_value, pic_value);

pic_sym pic_intern(pic_state *, const char *, size_t);
pic_sym pic_intern_cstr(pic_state *, const char *);
const char *pic_symbol_name(pic_state *, pic_sym);
pic_sym pic_gensym(pic_state *, pic_sym);
bool pic_interned_p(pic_state *, pic_sym);

char *pic_strdup(pic_state *, const char *);
char *pic_strndup(pic_state *, const char *, size_t);

pic_value pic_read(pic_state *, const char *);
pic_list pic_read_file(pic_state *, FILE *); /* When input string is incomplete, returns undef. */
pic_list pic_read_cstr(pic_state *, const char *);

pic_value pic_load(pic_state *, const char *);

pic_value pic_apply(pic_state *, struct pic_proc *, pic_value);
pic_value pic_apply_argv(pic_state *, struct pic_proc *, size_t, ...);
pic_value pic_apply_trampoline(pic_state *, struct pic_proc *, pic_value);
pic_value pic_eval(pic_state *, pic_value);
struct pic_proc *pic_compile(pic_state *, pic_value);
pic_value pic_macroexpand(pic_state *, pic_value);

void pic_in_library(pic_state *, pic_value);
struct pic_lib *pic_make_library(pic_state *, pic_value);
struct pic_lib *pic_find_library(pic_state *, pic_value);

#define pic_deflibrary(spec)                                            \
  pic_deflibrary_helper__(GENSYM(pic_deflib_i__), GENSYM(pic_deflib_prev_lib__), spec)
#define pic_deflibrary_helper__(i, prev_lib, spec)                      \
  for (int i = 0; ! i; )                                                \
    for (struct pic_lib *prev_lib; ! i; )                               \
      for ((prev_lib = pic->lib), pic_make_library(pic, pic_read(pic, spec)), pic_in_library(pic, pic_read(pic, spec)); ! i++; pic->lib = prev_lib)

void pic_import(pic_state *, pic_value);
void pic_export(pic_state *, pic_sym);

NORETURN void pic_abort(pic_state *, const char *);
NORETURN void pic_raise(pic_state *, struct pic_error *);
NORETURN void pic_error(pic_state *, const char *);
NORETURN void pic_errorf(pic_state *, const char *, ...);
void pic_warn(pic_state *, const char *);

const char *pic_errmsg(pic_state *);

pic_value pic_debug(pic_state *, pic_value);
pic_value pic_fdebug(pic_state *, pic_value, xFILE *);

#if defined(__cplusplus)
}
#endif

#endif
