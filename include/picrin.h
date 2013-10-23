#ifndef PICRIN_H__
#define PICRIN_H__

#include <stddef.h>
#include <stdbool.h>
#include <setjmp.h>

#include "picconf.h"
#include "picrin/value.h"

struct pic_code;

typedef struct pic_callinfo {
  int argc;
  struct pic_code *pc;
  pic_value *fp;
} pic_callinfo;

typedef struct {
  int argc;
  char **argv, **envp;

  pic_value *sp;
  pic_value *stbase, *stend;

  pic_callinfo *ci;
  pic_callinfo *cibase, *ciend;

  pic_value sDEFINE, sLAMBDA, sIF, sBEGIN, sQUOTE;
  pic_value sQUASIQUOTE, sUNQUOTE, sUNQUOTE_SPLICING;
  pic_value sCONS, sCAR, sCDR, sNILP;
  pic_value sADD, sSUB, sMUL, sDIV;

  struct sym_tbl *sym_tbl;

  struct xhash *global_tbl;
  pic_value *globals;
  size_t glen, gcapa;

  struct pic_irep **irep;
  size_t ilen, icapa;
  pic_value *pool;
  size_t plen, pcapa;

  jmp_buf *jmp;
  const char *errmsg;

  struct heap_page *heap;
  struct pic_object *arena[PIC_ARENA_SIZE];
  int arena_idx;
} pic_state;

typedef pic_value (*pic_func_t)(pic_state *);

void *pic_alloc(pic_state *, size_t);
void *pic_realloc(pic_state *, void *, size_t);
void *pic_calloc(pic_state *, unsigned, size_t);
struct pic_object *pic_obj_alloc(pic_state *, size_t, enum pic_tt);
void pic_free(pic_state *, void *);

void pic_gc_protect(pic_state *, pic_value);
int pic_gc_arena_preserve(pic_state *);
void pic_gc_arena_restore(pic_state *, int);

pic_state *pic_open(int argc, char *argv[], char **envp);
void pic_close(pic_state *);

int pic_get_args(pic_state *, const char *, ...);
void pic_defun(pic_state *, const char *, pic_func_t);

bool pic_eq_p(pic_state *, pic_value, pic_value);

pic_value pic_intern_cstr(pic_state *, const char *);

pic_value pic_str_new(pic_state *, const char *, size_t);
pic_value pic_str_new_cstr(pic_state *, const char *);

bool pic_parse(pic_state *, const char *, pic_value *);

pic_value pic_run(pic_state *, struct pic_proc *, pic_value);
struct pic_proc *pic_codegen(pic_state *, pic_value);

void pic_abort(pic_state *, const char *);
void pic_raise(pic_state *, pic_value);
void pic_error(pic_state *, const char *);

void pic_debug(pic_state *, pic_value);

#endif
