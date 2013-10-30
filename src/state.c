#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"
#include "picrin/proc.h"
#include "xhash/xhash.h"

void pic_init_core(pic_state *);

pic_state *
pic_open(int argc, char *argv[], char **envp)
{
  pic_state *pic;
  int ai;

  pic = (pic_state *)malloc(sizeof(pic_state));

  /* command line */
  pic->argc = argc;
  pic->argv = argv;
  pic->envp = envp;

  /* prepare VM stack */
  pic->stbase = pic->sp = (pic_value *)malloc(sizeof(pic_value) * PIC_STACK_SIZE);
  pic->stend = pic->stbase + PIC_STACK_SIZE;

  /* callinfo */
  pic->cibase = pic->ci = (pic_callinfo *)malloc(sizeof(pic_callinfo) * PIC_STACK_SIZE);
  pic->ciend = pic->ciend + PIC_STACK_SIZE;

  /* memory heap */
  pic->heap = (struct heap_page *)malloc(sizeof(struct heap_page));
  init_heap_page(pic->heap);

  /* symbol table */
  pic->sym_tbl = xh_new();
  pic->sym_pool = (const char **)malloc(sizeof(const char *) * PIC_SYM_POOL_SIZE);
  pic->slen = 0;
  pic->scapa = pic->slen + PIC_SYM_POOL_SIZE;

  /* irep */
  pic->irep = (struct pic_irep **)malloc(sizeof(struct pic_irep *) * PIC_IREP_SIZE);
  pic->ilen = 0;
  pic->icapa = PIC_IREP_SIZE;

  /* globals */
  pic->global_tbl = xh_new();
  pic->globals = (pic_value *)malloc(sizeof(pic_value) * PIC_GLOBALS_SIZE);
  pic->glen = 0;
  pic->gcapa = PIC_GLOBALS_SIZE;
  pic->macros = (struct pic_proc **)malloc(sizeof(struct pic_proc *) * PIC_MACROS_SIZE);
  pic->mlen = 0;
  pic->mcapa = PIC_MACROS_SIZE;

  /* pool */
  pic->pool = (pic_value *)malloc(sizeof(pic_value) * PIC_POOL_SIZE);
  pic->plen = 0;
  pic->pcapa = PIC_POOL_SIZE;

  /* error handling */
  pic->jmp = NULL;
  pic->errmsg = NULL;

  /* GC arena */
  pic->arena_idx = 0;

  ai = pic_gc_arena_preserve(pic);
  pic->sDEFINE = pic_intern_cstr(pic, "define");
  pic->sLAMBDA = pic_intern_cstr(pic, "lambda");
  pic->sIF = pic_intern_cstr(pic, "if");
  pic->sBEGIN = pic_intern_cstr(pic, "begin");
  pic->sSETBANG = pic_intern_cstr(pic, "set!");
  pic->sQUOTE = pic_intern_cstr(pic, "quote");
  pic->sQUASIQUOTE = pic_intern_cstr(pic, "quasiquote");
  pic->sUNQUOTE = pic_intern_cstr(pic, "unquote");
  pic->sUNQUOTE_SPLICING = pic_intern_cstr(pic, "unquote-splicing");
  pic->sDEFINE_SYNTAX = pic_intern_cstr(pic, "define-syntax");
  pic->sDEFINE_MACRO = pic_intern_cstr(pic, "define-macro");
  pic->sCONS = pic_intern_cstr(pic, "cons");
  pic->sCAR = pic_intern_cstr(pic, "car");
  pic->sCDR = pic_intern_cstr(pic, "cdr");
  pic->sNILP = pic_intern_cstr(pic, "null?");
  pic->sADD = pic_intern_cstr(pic, "+");
  pic->sSUB = pic_intern_cstr(pic, "-");
  pic->sMUL = pic_intern_cstr(pic, "*");
  pic->sDIV = pic_intern_cstr(pic, "/");
  pic->sEQ = pic_intern_cstr(pic, "=");
  pic->sLT = pic_intern_cstr(pic, "<");
  pic->sLE = pic_intern_cstr(pic, "<=");
  pic->sGT = pic_intern_cstr(pic, ">");
  pic->sGE = pic_intern_cstr(pic, ">=");
  pic_gc_arena_restore(pic, ai);

  pic_init_core(pic);

  return pic;
}

void
pic_close(pic_state *pic)
{
  free(pic);
}
