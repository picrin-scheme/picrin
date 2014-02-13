/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"
#include "picrin/proc.h"
#include "picrin/macro.h"
#include "picrin/cont.h"

void pic_init_core(pic_state *);

pic_state *
pic_open(int argc, char *argv[], char **envp)
{
  pic_value t;

  pic_state *pic;
  int ai;

  pic = (pic_state *)malloc(sizeof(pic_state));

  /* command line */
  pic->argc = argc;
  pic->argv = argv;
  pic->envp = envp;

  /* root block */
  pic->blk = (struct pic_block *)malloc(sizeof(struct pic_block));
  pic->blk->prev = NULL;
  pic->blk->depth = 0;
  pic->blk->in = pic->blk->out = NULL;
  pic->blk->refcnt = 1;

  /* prepare VM stack */
  pic->stbase = pic->sp = (pic_value *)calloc(PIC_STACK_SIZE, sizeof(pic_value));
  pic->stend = pic->stbase + PIC_STACK_SIZE;

  /* callinfo */
  pic->cibase = pic->ci = (pic_callinfo *)calloc(PIC_STACK_SIZE, sizeof(pic_callinfo));
  pic->ciend = pic->cibase + PIC_STACK_SIZE;

  /* exception handlers */
  pic->rescue = (struct pic_proc **)calloc(PIC_RESCUE_SIZE, sizeof(struct pic_proc *));
  pic->ridx = 0;
  pic->rlen = PIC_RESCUE_SIZE;

  /* memory heap */
  pic->heap = (struct pic_heap *)calloc(1, sizeof(struct pic_heap));
  init_heap(pic->heap);

  /* symbol table */
  pic->syms = xh_new_str();
  pic->sym_names = xh_new_int();
  pic->sym_cnt = 0;
  pic->uniq_sym_cnt = 0;

  /* global variables */
  pic->global_tbl = xh_new_int();
  pic->globals = (pic_value *)calloc(PIC_GLOBALS_SIZE, sizeof(pic_value));
  pic->glen = 0;
  pic->gcapa = PIC_GLOBALS_SIZE;

  /* macros */
  pic->macros = xh_new_int();

  /* libraries */
  pic->lib_tbl = pic_nil_value();
  pic->lib = NULL;

  /* error handling */
  pic->jmp = NULL;
  pic->err = NULL;

  /* GC arena */
  pic->arena_idx = 0;

  /* native stack marker */
  pic->native_stack_start = &t;

#define register_core_symbol(pic,slot,name) do {	\
    pic->slot = pic_intern_cstr(pic, name);		\
  } while (0)

  ai = pic_gc_arena_preserve(pic);
  register_core_symbol(pic, sDEFINE, "define");
  register_core_symbol(pic, sLAMBDA, "lambda");
  register_core_symbol(pic, sIF, "if");
  register_core_symbol(pic, sBEGIN, "begin");
  register_core_symbol(pic, sSETBANG, "set!");
  register_core_symbol(pic, sQUOTE, "quote");
  register_core_symbol(pic, sQUASIQUOTE, "quasiquote");
  register_core_symbol(pic, sUNQUOTE, "unquote");
  register_core_symbol(pic, sUNQUOTE_SPLICING, "unquote-splicing");
  register_core_symbol(pic, sDEFINE_SYNTAX, "define-syntax");
  register_core_symbol(pic, sDEFINE_MACRO, "define-macro");
  register_core_symbol(pic, sDEFINE_LIBRARY, "define-library");
  register_core_symbol(pic, sIMPORT, "import");
  register_core_symbol(pic, sEXPORT, "export");
  register_core_symbol(pic, sCONS, "cons");
  register_core_symbol(pic, sCAR, "car");
  register_core_symbol(pic, sCDR, "cdr");
  register_core_symbol(pic, sNILP, "null?");
  register_core_symbol(pic, sADD, "+");
  register_core_symbol(pic, sSUB, "-");
  register_core_symbol(pic, sMUL, "*");
  register_core_symbol(pic, sDIV, "/");
  register_core_symbol(pic, sMINUS, "minus");
  register_core_symbol(pic, sEQ, "=");
  register_core_symbol(pic, sLT, "<");
  register_core_symbol(pic, sLE, "<=");
  register_core_symbol(pic, sGT, ">");
  register_core_symbol(pic, sGE, ">=");
  register_core_symbol(pic, sNOT, "not");
  pic_gc_arena_restore(pic, ai);

  pic_init_core(pic);

  /* set library */
  pic_make_library(pic, pic_parse(pic, "user"));
  pic_in_library(pic, pic_parse(pic, "user"));

  return pic;
}

void
pic_close(pic_state *pic)
{
  xh_iter it;

  /* free global stacks */
  free(pic->stbase);
  free(pic->cibase);
  free(pic->rescue);
  free(pic->globals);

  xh_destroy(pic->syms);
  xh_destroy(pic->global_tbl);

  pic->glen = 0;
  pic->rlen = 0;
  pic->arena_idx = 0;
  pic->lib_tbl = pic_undef_value();

  xh_clear(pic->macros);

  /* free all values */
  pic_gc_run(pic);

  xh_destroy(pic->macros);

  /* free heaps */
  finalize_heap(pic->heap);
  free(pic->heap);

  /* free symbol names */
  for (xh_begin(pic->sym_names, &it); ! xh_isend(&it); xh_next(&it)) {
    free((void *)it.e->val);
  }
  free(pic->sym_names);

  PIC_BLK_DECREF(pic, pic->blk);

  free(pic);
}
