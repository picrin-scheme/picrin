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
#include <limits.h>
#include <stdarg.h>

#include "picrin/config.h"
#include "picrin/util.h"
#include "picrin/compat.h"

#if PIC_ENABLE_FLOAT
# include <math.h>
#endif

#include "picrin/xvect.h"
#include "picrin/xhash.h"

#include "picrin/value.h"

typedef struct pic_state pic_state;

#include "picrin/irep.h"
#include "picrin/file.h"

typedef struct pic_jmpbuf {
  PIC_JMPBUF buf;
  struct pic_jmpbuf *prev;
} pic_jmpbuf;

typedef struct pic_checkpoint {
  struct pic_proc *in;
  struct pic_proc *out;
  int depth;
  struct pic_checkpoint *prev;
} pic_checkpoint;

typedef struct {
  int argc, retc;
  pic_code *ip;
  pic_value *fp;
  struct pic_context *cxt;
  int regc;
  pic_value *regs;
  struct pic_context *up;
} pic_callinfo;

typedef void *(*pic_allocf)(void *, size_t);

struct pic_state {
  int argc;
  char **argv, **envp;

  pic_allocf allocf;

  pic_jmpbuf *jmp;
  pic_checkpoint *cp;

  pic_value *sp;
  pic_value *stbase, *stend;

  pic_callinfo *ci;
  pic_callinfo *cibase, *ciend;

  struct pic_proc **xp;
  struct pic_proc **xpbase, **xpend;

  pic_code *ip;

  pic_value ptable;

  struct pic_lib *lib, *prev_lib;

  pic_sym *sDEFINE, *sLAMBDA, *sIF, *sBEGIN, *sQUOTE, *sSETBANG;
  pic_sym *sQUASIQUOTE, *sUNQUOTE, *sUNQUOTE_SPLICING;
  pic_sym *sSYNTAX_QUOTE, *sSYNTAX_QUASIQUOTE, *sSYNTAX_UNQUOTE;
  pic_sym *sSYNTAX_UNQUOTE_SPLICING;
  pic_sym *sDEFINE_MACRO, *sIMPORT, *sEXPORT;
  pic_sym *sDEFINE_LIBRARY;
  pic_sym *sCOND_EXPAND, *sAND, *sOR, *sELSE, *sLIBRARY;
  pic_sym *sONLY, *sRENAME, *sPREFIX, *sEXCEPT;
  pic_sym *sCONS, *sCAR, *sCDR, *sNILP;
  pic_sym *sSYMBOLP, *sPAIRP;
  pic_sym *sADD, *sSUB, *sMUL, *sDIV, *sMINUS;
  pic_sym *sEQ, *sLT, *sLE, *sGT, *sGE, *sNOT;
  pic_sym *sREAD, *sFILE;
  pic_sym *sGREF, *sCREF, *sLREF;
  pic_sym *sCALL, *sTAILCALL, *sRETURN;
  pic_sym *sCALL_WITH_VALUES, *sTAILCALL_WITH_VALUES;

  pic_sym *uDEFINE, *uLAMBDA, *uIF, *uBEGIN, *uQUOTE, *uSETBANG;
  pic_sym *uDEFINE_MACRO, *uIMPORT, *uEXPORT;
  pic_sym *uDEFINE_LIBRARY;
  pic_sym *uCOND_EXPAND;
  pic_sym *uCONS, *uCAR, *uCDR, *uNILP;
  pic_sym *uSYMBOLP, *uPAIRP;
  pic_sym *uADD, *uSUB, *uMUL, *uDIV;
  pic_sym *uEQ, *uLT, *uLE, *uGT, *uGE, *uNOT;
  pic_sym *uVALUES, *uCALL_WITH_VALUES;

  struct pic_lib *PICRIN_BASE;
  struct pic_lib *PICRIN_USER;

  pic_value features;

  xhash syms;                   /* name to symbol */
  int ucnt;
  struct pic_dict *globals;
  struct pic_dict *macros;
  pic_value libs;
  struct pic_reg *attrs;

  struct pic_reader *reader;
  xFILE files[XOPEN_MAX];
  pic_code iseq[2];             /* for pic_apply_trampoline */

  bool gc_enable;
  struct pic_heap *heap;
  struct pic_object **arena;
  size_t arena_size, arena_idx;
  struct pic_reg *regs;

  pic_value err;

  char *native_stack_start;
};

typedef pic_value (*pic_func_t)(pic_state *);

void *pic_malloc(pic_state *, size_t);
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

pic_state *pic_open(int argc, char *argv[], char **envp, pic_allocf);
void *pic_default_allocf(void *, size_t);
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

pic_sym *pic_intern(pic_state *, pic_str *);
pic_sym *pic_intern_cstr(pic_state *, const char *);
const char *pic_symbol_name(pic_state *, pic_sym *);

pic_value pic_read(pic_state *, struct pic_port *);
pic_value pic_read_cstr(pic_state *, const char *);

void pic_load_port(pic_state *, struct pic_port *);
void pic_load_cstr(pic_state *, const char *);

pic_value pic_funcall(pic_state *pic, struct pic_lib *, const char *, pic_list);
pic_value pic_funcall0(pic_state *pic, struct pic_lib *, const char *);
pic_value pic_funcall1(pic_state *pic, struct pic_lib *, const char *, pic_value);
pic_value pic_funcall2(pic_state *pic, struct pic_lib *, const char *, pic_value, pic_value);
pic_value pic_funcall3(pic_state *pic, struct pic_lib *, const char *, pic_value, pic_value, pic_value);
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
pic_value pic_eval(pic_state *, pic_value, struct pic_env *);
pic_value pic_expand(pic_state *, pic_value, struct pic_env *);
struct pic_proc *pic_compile(pic_state *, pic_value, struct pic_env *);

struct pic_lib *pic_make_library(pic_state *, pic_value);
struct pic_lib *pic_find_library(pic_state *, pic_value);

#define pic_deflibrary(pic, spec)                                       \
  for (((assert(pic->prev_lib == NULL)),                                \
        (pic->prev_lib = pic->lib),                                     \
        (pic->lib = pic_find_library(pic, pic_read_cstr(pic, (spec)))), \
        (pic->lib = pic->lib                                            \
         ? pic->lib                                                     \
         : pic_make_library(pic, pic_read_cstr(pic, (spec)))));         \
       pic->prev_lib != NULL;                                           \
       ((pic->lib = pic->prev_lib),                                     \
        (pic->prev_lib = NULL)))

void pic_import(pic_state *, struct pic_lib *);
void pic_export(pic_state *, pic_sym *);

PIC_NORETURN void pic_panic(pic_state *, const char *);
PIC_NORETURN void pic_errorf(pic_state *, const char *, ...);
void pic_warnf(pic_state *, const char *, ...);
const char *pic_errmsg(pic_state *);
pic_str *pic_get_backtrace(pic_state *);
void pic_print_backtrace(pic_state *, xFILE *);
struct pic_dict *pic_attr(pic_state *, pic_value);
pic_value pic_attr_ref(pic_state *, pic_value, const char *);
void pic_attr_set(pic_state *, pic_value, const char *, pic_value);

struct pic_port *pic_stdin(pic_state *);
struct pic_port *pic_stdout(pic_state *);
struct pic_port *pic_stderr(pic_state *);

pic_value pic_write(pic_state *, pic_value); /* returns given obj */
pic_value pic_fwrite(pic_state *, pic_value, xFILE *);
void pic_printf(pic_state *, const char *, ...);
void pic_fprintf(pic_state *, struct pic_port *, const char *, ...);
pic_value pic_display(pic_state *, pic_value);
pic_value pic_fdisplay(pic_state *, pic_value, xFILE *);

#if DEBUG
# define pic_debug(pic,obj) pic_fwrite(pic,obj,xstderr)
# define pic_fdebug(pic,obj,file) pic_fwrite(pic,obj,file)
#endif

#include "picrin/blob.h"
#include "picrin/cont.h"
#include "picrin/data.h"
#include "picrin/dict.h"
#include "picrin/error.h"
#include "picrin/gc.h"
#include "picrin/lib.h"
#include "picrin/macro.h"
#include "picrin/pair.h"
#include "picrin/port.h"
#include "picrin/proc.h"
#include "picrin/read.h"
#include "picrin/record.h"
#include "picrin/string.h"
#include "picrin/symbol.h"
#include "picrin/read.h"
#include "picrin/vector.h"
#include "picrin/reg.h"

#if defined(__cplusplus)
}
#endif

#endif
