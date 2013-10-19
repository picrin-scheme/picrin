#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"
#include "picrin/proc.h"

static struct pic_env *
pic_new_empty_env()
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
  pic->sADD = pic_intern_cstr(pic, "+");
  pic->sSUB = pic_intern_cstr(pic, "-");
  pic->sMUL = pic_intern_cstr(pic, "*");
  pic->sDIV = pic_intern_cstr(pic, "/");

  /* global environment */
  pic->global_env = pic_new_empty_env();
  pic_init_core(pic);

  return pic;
}

void
pic_close(pic_state *pic)
{
  free(pic);
}
