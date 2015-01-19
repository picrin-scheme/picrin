/**
 * Copyright (c) 2013-2014 Yuichi Nishiwaki and other picrin contributors.
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

#ifndef PICRIN_H
#define PICRIN_H

#if defined(__cplusplus)
extern "C" {
#endif

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>
#include <stdarg.h>

#include <stdio.h>
#include <setjmp.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

#include "picrin/xvect.h"
#include "picrin/xhash.h"
#include "picrin/xfile.h"
#include "picrin/xrope.h"

#include "picrin/config.h"
#include "picrin/util.h"
#include "picrin/value.h"

typedef struct pic_code pic_code;

struct pic_winder {
  struct pic_proc *in;
  struct pic_proc *out;
  int depth;
  struct pic_winder *prev;
};

typedef struct {
  int argc, retc;
  pic_code *ip;
  pic_value *fp;
  struct pic_env *env;
  int regc;
  pic_value *regs;
  struct pic_env *up;
} pic_callinfo;

typedef struct {
  int argc;
  char **argv, **envp;

  struct pic_winder *wind;

  pic_value *sp;
  pic_value *stbase, *stend;

  pic_callinfo *ci;
  pic_callinfo *cibase, *ciend;

  struct pic_proc **xp;
  struct pic_proc **xpbase, **xpend;

  pic_code *ip;

  struct pic_lib *lib;

  pic_sym sDEFINE, sLAMBDA, sIF, sBEGIN, sQUOTE, sSETBANG;
  pic_sym sQUASIQUOTE, sUNQUOTE, sUNQUOTE_SPLICING;
  pic_sym sDEFINE_SYNTAX, sIMPORT, sEXPORT;
  pic_sym sDEFINE_LIBRARY, sIN_LIBRARY;
  pic_sym sCOND_EXPAND, sAND, sOR, sELSE, sLIBRARY;
  pic_sym sONLY, sRENAME, sPREFIX, sEXCEPT;
  pic_sym sCONS, sCAR, sCDR, sNILP;
  pic_sym sSYMBOLP, sPAIRP;
  pic_sym sADD, sSUB, sMUL, sDIV, sMINUS;
  pic_sym sEQ, sLT, sLE, sGT, sGE, sNOT;
  pic_sym sREAD, sFILE;

  pic_sym rDEFINE, rLAMBDA, rIF, rBEGIN, rQUOTE, rSETBANG;
  pic_sym rDEFINE_SYNTAX, rIMPORT, rEXPORT;
  pic_sym rDEFINE_LIBRARY, rIN_LIBRARY;
  pic_sym rCOND_EXPAND;

  struct pic_lib *PICRIN_BASE;
  struct pic_lib *PICRIN_USER;

  pic_value features;

  xhash syms;                   /* name to symbol */
  struct pic_dict *globals;
  struct pic_dict *macros;
  pic_value libs;
  xhash attrs;

  struct pic_reader *reader;

  bool gc_enable;
  struct pic_heap *heap;
  struct pic_object **arena;
  size_t arena_size, arena_idx;

  struct pic_port *xSTDIN, *xSTDOUT, *xSTDERR;

  pic_value err;

  char *native_stack_start;
} pic_state;

typedef pic_value (*pic_func_t)(pic_state *);

void *pic_alloc(pic_state *, size_t);
#define pic_malloc(pic,size) pic_alloc(pic,size) /* obsoleted */
void *pic_realloc(pic_state *, void *, size_t);
void *pic_calloc(pic_state *, size_t, size_t);
struct pic_object *pic_obj_alloc(pic_state *, size_t, enum pic_tt);
struct pic_object *pic_obj_alloc_unsafe(pic_state *, size_t, enum pic_tt);
void pic_free(pic_state *, void *);

void pic_gc_run(pic_state *);
pic_value pic_gc_protect(pic_state *, pic_value);
size_t pic_gc_arena_preserve(pic_state *);
void pic_gc_arena_restore(pic_state *, size_t);
#define pic_void(exec)                          \
  pic_void_(PIC_GENSYM(ai), exec)
#define pic_void_(ai,exec) do {                 \
    size_t ai = pic_gc_arena_preserve(pic);     \
    exec;                                       \
    pic_gc_arena_restore(pic, ai);              \
  } while (0)

pic_state *pic_open(int argc, char *argv[], char **envp);
void pic_close(pic_state *);

void pic_add_feature(pic_state *, const char *);

void pic_define(pic_state *, const char *, pic_value);
void pic_define_noexport(pic_state *, const char *, pic_value);
void pic_defun(pic_state *, const char *, pic_func_t);

struct pic_proc *pic_make_var(pic_state *, pic_value, struct pic_proc *);
void pic_defvar(pic_state *, const char *, pic_value, struct pic_proc *);

struct pic_proc *pic_get_proc(pic_state *);
int pic_get_args(pic_state *, const char *, ...);

bool pic_eq_p(pic_value, pic_value);
bool pic_eqv_p(pic_value, pic_value);
bool pic_equal_p(pic_state *, pic_value, pic_value);

pic_sym pic_intern(pic_state *, pic_str *);
pic_sym pic_intern_cstr(pic_state *, const char *);
const char *pic_symbol_name(pic_state *, pic_sym);
pic_sym pic_gensym(pic_state *, pic_sym);
bool pic_interned_p(pic_state *, pic_sym);

pic_value pic_read(pic_state *, struct pic_port *);
pic_value pic_read_cstr(pic_state *, const char *);

void pic_load_port(pic_state *, struct pic_port *);
void pic_load_cstr(pic_state *, const char *);

pic_value pic_funcall(pic_state *pic, struct pic_lib *, const char *, pic_list);
pic_value pic_ref(pic_state *, struct pic_lib *, const char *);
void pic_set(pic_state *, struct pic_lib *, const char *, pic_value);

pic_value pic_apply(pic_state *, struct pic_proc *, pic_value);
pic_value pic_apply0(pic_state *, struct pic_proc *);
pic_value pic_apply1(pic_state *, struct pic_proc *, pic_value);
pic_value pic_apply2(pic_state *, struct pic_proc *, pic_value, pic_value);
pic_value pic_apply3(pic_state *, struct pic_proc *, pic_value, pic_value, pic_value);
pic_value pic_apply4(pic_state *, struct pic_proc *, pic_value, pic_value, pic_value, pic_value);
pic_value pic_apply5(pic_state *, struct pic_proc *, pic_value, pic_value, pic_value, pic_value, pic_value);
pic_value pic_apply_trampoline(pic_state *, struct pic_proc *, pic_value);
pic_value pic_eval(pic_state *, pic_value, struct pic_lib *);
struct pic_proc *pic_compile(pic_state *, pic_value, struct pic_lib *);
pic_value pic_macroexpand(pic_state *, pic_value, struct pic_lib *);

void pic_in_library(pic_state *, pic_value);
struct pic_lib *pic_open_library(pic_state *, pic_value);
struct pic_lib *pic_find_library(pic_state *, pic_value);

#define pic_deflibrary(pic, spec)                                       \
  pic_deflibrary_helper_(pic, PIC_GENSYM(i), PIC_GENSYM(prev_lib), spec)
#define pic_deflibrary_helper_(pic, i, prev_lib, spec)                  \
  for (int i = 0; ! i; )                                                \
    for (struct pic_lib *prev_lib; ! i; )                               \
      for ((prev_lib = pic->lib), pic_open_library(pic, pic_read_cstr(pic, spec)), pic_in_library(pic, pic_read_cstr(pic, spec)); ! i++; pic->lib = prev_lib)

void pic_import(pic_state *, pic_value);
void pic_import_library(pic_state *, struct pic_lib *);
void pic_export(pic_state *, pic_sym);

pic_noreturn void pic_panic(pic_state *, const char *);
pic_noreturn void pic_errorf(pic_state *, const char *, ...);
void pic_warnf(pic_state *, const char *, ...);
const char *pic_errmsg(pic_state *);
pic_str *pic_get_backtrace(pic_state *);
void pic_print_backtrace(pic_state *);

/* obsoleted */
static inline void pic_warn(pic_state *pic, const char *msg)
{
  pic_warnf(pic, msg);
}

struct pic_dict *pic_attr(pic_state *, pic_value);
pic_value pic_attr_ref(pic_state *, pic_value, const char *);
void pic_attr_set(pic_state *, pic_value, const char *, pic_value);

struct pic_port *pic_stdin(pic_state *);
struct pic_port *pic_stdout(pic_state *);
struct pic_port *pic_stderr(pic_state *);

pic_value pic_write(pic_state *, pic_value); /* returns given obj */
pic_value pic_fwrite(pic_state *, pic_value, xFILE *);
void pic_printf(pic_state *, const char *, ...);
pic_value pic_display(pic_state *, pic_value);
pic_value pic_fdisplay(pic_state *, pic_value, xFILE *);
/* obsoleted macros */
#define pic_debug(pic,obj) pic_write(pic,obj)
#define pic_fdebug(pic,obj,file) pic_fwrite(pic,obj,file)

#if defined(__cplusplus)
}
#endif

#endif
