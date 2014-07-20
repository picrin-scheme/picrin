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
  char t;

  pic_state *pic;
  size_t ai;

  pic = (pic_state *)malloc(sizeof(pic_state));

  /* command line */
  pic->argc = argc;
  pic->argv = argv;
  pic->envp = envp;

  /* root block */
  pic->blk = (pic_block *)malloc(sizeof(pic_block));
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

  /* memory heap */
  pic->heap = pic_heap_open();

  /* symbol table */
  xh_init_str(&pic->syms, sizeof(pic_sym));
  xh_init_int(&pic->sym_names, sizeof(const char *));
  pic->sym_cnt = 0;
  pic->uniq_sym_cnt = 0;

  /* global variables */
  xh_init_int(&pic->global_tbl, sizeof(size_t));
  pic->globals = (pic_value *)calloc(PIC_GLOBALS_SIZE, sizeof(pic_value));
  pic->glen = 0;
  pic->gcapa = PIC_GLOBALS_SIZE;

  /* macros */
  xh_init_int(&pic->macros, sizeof(struct pic_macro *));

  /* libraries */
  pic->lib_tbl = pic_nil_value();
  pic->lib = NULL;

  /* reader */
  xh_init_int(&pic->rlabels, sizeof(pic_value));

  /* error handling */
  pic->jmp = NULL;
  pic->err = NULL;
  pic->try_jmps = NULL;

  /* GC arena */
  pic->arena = (struct pic_object **)calloc(PIC_ARENA_SIZE, sizeof(struct pic_object **));
  pic->arena_size = PIC_ARENA_SIZE;
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
  register_core_symbol(pic, sLET_SYNTAX, "let-syntax");
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

#define register_renamed_symbol(pic,slot,name) do {              \
    pic->slot = pic_gensym(pic, pic_intern_cstr(pic, name));     \
  } while (0)

  ai = pic_gc_arena_preserve(pic);
  register_renamed_symbol(pic, rDEFINE, "define");
  register_renamed_symbol(pic, rLAMBDA, "lambda");
  register_renamed_symbol(pic, rIF, "if");
  register_renamed_symbol(pic, rBEGIN, "begin");
  register_renamed_symbol(pic, rSETBANG, "set!");
  register_renamed_symbol(pic, rQUOTE, "quote");
  register_renamed_symbol(pic, rDEFINE_SYNTAX, "define-syntax");
  register_renamed_symbol(pic, rLET_SYNTAX, "let-syntax");
  register_renamed_symbol(pic, rDEFINE_LIBRARY, "define-library");
  register_renamed_symbol(pic, rIMPORT, "import");
  register_renamed_symbol(pic, rEXPORT, "export");
  pic_gc_arena_restore(pic, ai);

  pic_init_core(pic);

  /* set library */
  pic_make_library(pic, pic_read_cstr(pic, "(picrin user)"));
  pic_in_library(pic, pic_read_cstr(pic, "(picrin user)"));

  return pic;
}

void
pic_close(pic_state *pic)
{
  xh_iter it;

  /* invoke exit handlers */
  PIC_BLK_EXIT(pic);

  /* clear out root objects */
  pic->sp = pic->stbase;
  pic->ci = pic->cibase;
  pic->arena_idx = 0;
  pic->err = NULL;
  pic->glen = 0;
  xh_clear(&pic->macros);
  pic->lib_tbl = pic_nil_value();

  /* free all heap objects */
  pic_gc_run(pic);

  /* free heaps */
  pic_heap_close(pic->heap);

  /* free runtime context */
  free(pic->stbase);
  free(pic->cibase);

  /* free global stacks */
  free(pic->globals);
  xh_destroy(&pic->syms);
  xh_destroy(&pic->global_tbl);
  xh_destroy(&pic->macros);
  xh_destroy(&pic->rlabels);

  /* free GC arena */
  free(pic->arena);

  /* free symbol names */
  xh_begin(&it, &pic->sym_names);
  while (xh_next(&it)) {
    free(xh_val(it.e, char *));
  }
  xh_destroy(&pic->sym_names);

  free(pic);
}
