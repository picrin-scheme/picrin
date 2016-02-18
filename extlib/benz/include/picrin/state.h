/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_STATE_H
#define PICRIN_STATE_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "picrin/khash.h"

#include "picrin/irep.h"
#include "picrin/file.h"
#include "picrin/read.h"
#include "picrin/gc.h"

struct pic_lib {
  struct pic_string *name;
  struct pic_env *env;
  struct pic_dict *exports;
};

typedef struct pic_checkpoint {
  PIC_OBJECT_HEADER
  struct pic_proc *in;
  struct pic_proc *out;
  int depth;
  struct pic_checkpoint *prev;
} pic_checkpoint;

typedef struct {
  int argc, retc;
  pic_code *ip;
  pic_value *fp;
  struct pic_irep *irep;
  struct pic_context *cxt;
  int regc;
  pic_value *regs;
  struct pic_context *up;
} pic_callinfo;

KHASH_DECLARE(oblist, struct pic_string *, pic_sym *)
KHASH_DECLARE(ltable, const char *, struct pic_lib)

struct pic_state {
  pic_allocf allocf;
  void *userdata;

  pic_checkpoint *cp;
  struct pic_cont *cc;
  int ccnt;

  pic_value *sp;
  pic_value *stbase, *stend;

  pic_callinfo *ci;
  pic_callinfo *cibase, *ciend;

  struct pic_proc **xp;
  struct pic_proc **xpbase, **xpend;

  pic_code *ip;

  pic_value ptable;             /* list of ephemerons */

  struct pic_lib *lib;

  pic_sym *sDEFINE, *sDEFINE_MACRO, *sLAMBDA, *sIF, *sBEGIN, *sSETBANG;
  pic_sym *sQUOTE, *sQUASIQUOTE, *sUNQUOTE, *sUNQUOTE_SPLICING;
  pic_sym *sSYNTAX_QUOTE, *sSYNTAX_QUASIQUOTE;
  pic_sym *sSYNTAX_UNQUOTE, *sSYNTAX_UNQUOTE_SPLICING;
  pic_sym *sDEFINE_LIBRARY, *sIMPORT, *sEXPORT, *sCOND_EXPAND;
  pic_sym *sCONS, *sCAR, *sCDR, *sNILP, *sSYMBOLP, *sPAIRP;
  pic_sym *sADD, *sSUB, *sMUL, *sDIV, *sEQ, *sLT, *sLE, *sGT, *sGE, *sNOT;

  pic_value features;

  khash_t(oblist) oblist;       /* string to symbol */
  int ucnt;
  struct pic_weak *globals;
  struct pic_weak *macros;
  khash_t(ltable) ltable;
  struct pic_list ireps;        /* chain */

  pic_reader reader;
  xFILE files[XOPEN_MAX];
  pic_code iseq[2];             /* for pic_apply_trampoline */

  bool gc_enable;
  struct pic_heap *heap;
  struct pic_object **arena;
  size_t arena_size, arena_idx;

  pic_value err;

  char *native_stack_start;
};

#if defined(__cplusplus)
}
#endif

#endif
