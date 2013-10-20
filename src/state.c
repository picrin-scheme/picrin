#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"
#include "picrin/proc.h"
#include "picrin/symbol.h"

static struct pic_env *
new_empty_env()
{
  struct pic_env *env;

  env = (struct pic_env *)malloc(sizeof(struct pic_env));
  env->assoc = pic_nil_value();
  env->parent = NULL;

  return env;
}

void pic_init_core(pic_state *);

pic_state *
pic_open()
{
  pic_state *pic;

  pic = (pic_state *)malloc(sizeof(pic_state));

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
  pic->globals = (pic_value *)malloc(sizeof(pic_value) * PIC_GLOBALS_SIZE);
  pic->glen = 0;
  pic->gcapa = PIC_GLOBALS_SIZE;

  /* GC arena */
  pic->arena_idx = 0;

  pic->sDEFINE = pic_intern_cstr(pic, "define");
  pic->sLAMBDA = pic_intern_cstr(pic, "lambda");
  pic->sIF = pic_intern_cstr(pic, "if");
  pic->sBEGIN = pic_intern_cstr(pic, "begin");
  pic->sCONS = pic_intern_cstr(pic, "cons");
  pic->sCAR = pic_intern_cstr(pic, "car");
  pic->sCDR = pic_intern_cstr(pic, "cdr");
  pic->sNILP = pic_intern_cstr(pic, "null?");
  pic->sADD = pic_intern_cstr(pic, "+");
  pic->sSUB = pic_intern_cstr(pic, "-");
  pic->sMUL = pic_intern_cstr(pic, "*");
  pic->sDIV = pic_intern_cstr(pic, "/");

  /* global environment */
  pic->global_env = new_empty_env();
  pic_init_core(pic);

  return pic;
}

void
pic_close(pic_state *pic)
{
  free(pic);
}
