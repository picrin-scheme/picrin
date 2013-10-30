#ifndef PICRIN_H__
#define PICRIN_H__

#include <stddef.h>
#include <stdbool.h>
#include <setjmp.h>
#include <stdio.h>

#include "picconf.h"
#include "picrin/value.h"

struct pic_code;

typedef struct pic_callinfo {
  int argc;
  struct pic_code *pc;
  pic_value *fp;
  struct pic_env *env;
} pic_callinfo;

typedef struct {
  int argc;
  char **argv, **envp;

  pic_value *sp;
  pic_value *stbase, *stend;

  pic_callinfo *ci;
  pic_callinfo *cibase, *ciend;

  pic_sym sDEFINE, sLAMBDA, sIF, sBEGIN, sQUOTE, sSETBANG;
  pic_sym sQUASIQUOTE, sUNQUOTE, sUNQUOTE_SPLICING;
  pic_sym sDEFINE_SYNTAX, sDEFINE_MACRO;
  pic_sym sCONS, sCAR, sCDR, sNILP;
  pic_sym sADD, sSUB, sMUL, sDIV;
  pic_sym sEQ, sLT, sLE, sGT, sGE;

  struct xhash *sym_tbl;
  const char **sym_pool;
  size_t slen, scapa;

  /* positive for variables, negative for macros (bitnot) */
  struct xhash *global_tbl;
  pic_value *globals;
  size_t glen, gcapa;
  struct pic_proc **macros;
  size_t mlen, mcapa;

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
bool pic_eqv_p(pic_state *, pic_value, pic_value);
bool pic_equal_p(pic_state *, pic_value, pic_value);

pic_sym pic_intern_cstr(pic_state *, const char *);
const char *pic_symbol_name(pic_state *, pic_sym);

pic_value pic_str_new(pic_state *, const char *, size_t);
pic_value pic_str_new_cstr(pic_state *, const char *);

struct pic_vector *pic_vec_new(pic_state *, size_t);
struct pic_vector *pic_vec_new_from_list(pic_state *, pic_value);

int pic_parse_file(pic_state *, FILE *file, pic_value *);
int pic_parse_cstr(pic_state *, const char *, pic_value *);

pic_value pic_apply(pic_state *pic, struct pic_proc *, pic_value);
pic_value pic_apply_argv(pic_state *pic, struct pic_proc *, size_t, ...);
struct pic_proc *pic_codegen(pic_state *, pic_value);
pic_value pic_expand(pic_state *, pic_value);

void pic_abort(pic_state *, const char *);
void pic_raise(pic_state *, pic_value);
void pic_error(pic_state *, const char *);
void pic_warn(pic_state *, const char *);

void pic_debug(pic_state *, pic_value);

#endif
