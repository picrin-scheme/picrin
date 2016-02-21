/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_STATE_H
#define PICRIN_STATE_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "picrin/private/khash.h"
#include "picrin/private/file.h"

#include "picrin/private/vm.h"
#include "picrin/private/gc.h"

struct pic_lib {
  struct pic_string *name;
  struct pic_env *env;
  struct pic_dict *exports;
};

struct pic_callinfo {
  int argc, retc;
  struct pic_code *ip;
  pic_value *fp;
  struct pic_irep *irep;
  struct pic_context *cxt;
  int regc;
  pic_value *regs;
  struct pic_context *up;
};

KHASH_DECLARE(oblist, struct pic_string *, struct pic_identifier *)
KHASH_DECLARE(ltable, const char *, struct pic_lib)

struct pic_state {
  pic_allocf allocf;
  void *userdata;

  struct pic_checkpoint *cp;
  struct pic_cont *cc;
  int ccnt;

  pic_value *sp;
  pic_value *stbase, *stend;

  struct pic_callinfo *ci;
  struct pic_callinfo *cibase, *ciend;

  struct pic_proc **xp;
  struct pic_proc **xpbase, **xpend;

  struct pic_code *ip;

  pic_value ptable;             /* list of ephemerons */

  struct pic_lib *lib;

  pic_value features;

  khash_t(oblist) oblist;       /* string to symbol */
  int ucnt;
  pic_value globals;            /* weak */
  pic_value macros;             /* weak */
  khash_t(ltable) ltable;
  struct pic_list_head ireps;        /* chain */

  xFILE files[XOPEN_MAX];
  struct pic_code iseq[2];             /* for pic_apply_trampoline */

  bool gc_enable;
  struct pic_heap *heap;
  struct pic_object **arena;
  size_t arena_size, arena_idx;

  pic_value err;
  pic_panicf panicf;

  char *native_stack_start;
};

#if defined(__cplusplus)
}
#endif

#endif
