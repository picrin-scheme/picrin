#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"
#include "picrin/proc.h"
#include "picrin/symbol.h"
#include "xhash/xhash.h"

struct sym_tbl *
sym_tbl_new()
{
  struct sym_tbl *s_tbl;
  int i;

  s_tbl = (struct sym_tbl *)malloc(sizeof(struct sym_tbl));
  s_tbl->size = PIC_SYM_TBL_SIZE;

  for (i = 0; i < PIC_SYM_TBL_SIZE; ++i) {
    s_tbl->tbl[i] = pic_nil_value();
  }
  return s_tbl;
}

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
  pic->sym_tbl = sym_tbl_new();

  /* irep */
  pic->irep = (struct pic_irep **)malloc(sizeof(struct pic_irep *) * PIC_IREP_SIZE);
  pic->ilen = 0;
  pic->icapa = PIC_IREP_SIZE;

  /* globals */
  pic->global_tbl = xh_new();
  pic->globals = (pic_value *)malloc(sizeof(pic_value) * PIC_GLOBALS_SIZE);
  pic->glen = 0;
  pic->gcapa = PIC_GLOBALS_SIZE;

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
  pic->sQUOTE = pic_intern_cstr(pic, "quote");
  pic->sCONS = pic_intern_cstr(pic, "cons");
  pic->sCAR = pic_intern_cstr(pic, "car");
  pic->sCDR = pic_intern_cstr(pic, "cdr");
  pic->sNILP = pic_intern_cstr(pic, "null?");
  pic->sADD = pic_intern_cstr(pic, "+");
  pic->sSUB = pic_intern_cstr(pic, "-");
  pic->sMUL = pic_intern_cstr(pic, "*");
  pic->sDIV = pic_intern_cstr(pic, "/");
  pic_gc_arena_restore(pic, ai);

  pic_init_core(pic);

  return pic;
}

void
pic_close(pic_state *pic)
{
  free(pic);
}
