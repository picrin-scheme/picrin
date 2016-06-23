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

struct lib {
  struct string *name;
  struct env *env;
  struct dict *exports;
};

KHASH_DECLARE(oblist, struct string *, struct symbol *)
KHASH_DECLARE(ltable, const char *, struct lib)

struct pic_state {
  pic_allocf allocf;
  void *userdata;

  struct checkpoint *cp;
  struct cont *cc;

  const char *lib;

  pic_value features;

  khash_t(oblist) oblist;       /* string to symbol */
  pic_value globals;            /* weak */
  khash_t(ltable) ltable;
  struct list_head ireps;

  struct file files[PIC_OPEN_MAX];

  bool gc_enable;
  struct heap *heap;
  struct object **arena;
  size_t arena_size, arena_idx;

  pic_value err;

  pic_panicf panicf;
};

#if defined(__cplusplus)
}
#endif

#endif
