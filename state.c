/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>

#include "picrin.h"
#include "picrin/gc.h"
#include "picrin/read.h"
#include "picrin/proc.h"
#include "picrin/macro.h"
#include "picrin/cont.h"
#include "picrin/port.h"
#include "picrin/error.h"

void pic_init_core(pic_state *);

pic_state *
pic_open(int argc, char *argv[], char **envp)
{
  struct pic_port *pic_port_make_stdport(pic_state *, xFILE *, short);
  char t;

  pic_state *pic;
  size_t ai;

  pic = malloc(sizeof(pic_state));

  /* root block */
  pic->blk = NULL;

  /* command line */
  pic->argc = argc;
  pic->argv = argv;
  pic->envp = envp;

  /* prepare VM stack */
  pic->stbase = pic->sp = calloc(PIC_STACK_SIZE, sizeof(pic_value));
  pic->stend = pic->stbase + PIC_STACK_SIZE;

  /* callinfo */
  pic->cibase = pic->ci = calloc(PIC_STACK_SIZE, sizeof(pic_callinfo));
  pic->ciend = pic->cibase + PIC_STACK_SIZE;

  /* memory heap */
  pic->heap = pic_heap_open();

  /* symbol table */
  xh_init_str(&pic->syms, sizeof(pic_sym));
  xh_init_int(&pic->sym_names, sizeof(const char *));
  pic->sym_cnt = 0;
  pic->uniq_sym_cnt = 0;

  /* global variables */
  xh_init_int(&pic->globals, sizeof(pic_value));

  /* macros */
  xh_init_int(&pic->macros, sizeof(struct pic_macro *));

  /* libraries */
  pic->libs = pic_nil_value();
  pic->lib = NULL;

  /* reader */
  pic->reader = malloc(sizeof(struct pic_reader));
  pic->reader->typecase = PIC_CASE_DEFAULT;
  pic->reader->trie = pic_trie_new(pic);
  xh_init_int(&pic->reader->labels, sizeof(pic_value));

  /* error handling */
  pic->jmp = NULL;
  pic->err = NULL;
  pic->try_jmps = calloc(PIC_RESCUE_SIZE, sizeof(struct pic_jmpbuf));
  pic->try_jmp_idx = 0;
  pic->try_jmp_size = PIC_RESCUE_SIZE;

  /* standard ports */
  pic->xSTDIN = NULL;
  pic->xSTDOUT = NULL;
  pic->xSTDERR = NULL;

  /* GC arena */
  pic->arena = calloc(PIC_ARENA_SIZE, sizeof(struct pic_object **));
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
  register_core_symbol(pic, sIMPORT, "import");
  register_core_symbol(pic, sEXPORT, "export");
  register_core_symbol(pic, sDEFINE_LIBRARY, "define-library");
  register_core_symbol(pic, sIN_LIBRARY, "in-library");
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
  register_renamed_symbol(pic, rIMPORT, "import");
  register_renamed_symbol(pic, rEXPORT, "export");
  register_renamed_symbol(pic, rDEFINE_LIBRARY, "define-library");
  register_renamed_symbol(pic, rIN_LIBRARY, "in-library");
  pic_gc_arena_restore(pic, ai);

  /* root block */
  pic->blk = (struct pic_block *)pic_obj_alloc(pic, sizeof(struct pic_block), PIC_TT_BLK);
  pic->blk->prev = NULL;
  pic->blk->depth = 0;
  pic->blk->in = pic->blk->out = NULL;

  /* init readers */
  pic_init_reader(pic);

  /* standard libraries */
  pic->PICRIN_BASE = pic_open_library(pic, pic_read_cstr(pic, "(picrin base)"));
  pic->PICRIN_USER = pic_open_library(pic, pic_read_cstr(pic, "(picrin user)"));
  pic->lib = pic->PICRIN_USER;

  /* standard I/O */
  pic->xSTDIN = pic_port_make_stdport(pic, xstdin, PIC_PORT_IN);
  pic->xSTDOUT = pic_port_make_stdport(pic, xstdout, PIC_PORT_OUT);
  pic->xSTDERR = pic_port_make_stdport(pic, xstderr, PIC_PORT_OUT);

  pic_init_core(pic);

  return pic;
}

void
pic_close(pic_state *pic)
{
  xh_iter it;

  /* invoke exit handlers */
  while (pic->blk) {
    if (pic->blk->out) {
      pic_apply0(pic, pic->blk->out);
    }
    pic->blk = pic->blk->prev;
  }

  /* clear out root objects */
  pic->sp = pic->stbase;
  pic->ci = pic->cibase;
  pic->arena_idx = 0;
  pic->err = NULL;
  xh_clear(&pic->macros);
  pic->libs = pic_nil_value();

  /* free all heap objects */
  pic_gc_run(pic);

  /* free heaps */
  pic_heap_close(pic->heap);

  /* free runtime context */
  free(pic->stbase);
  free(pic->cibase);

  /* free reader struct */
  xh_destroy(&pic->reader->labels);
  pic_trie_delete(pic, pic->reader->trie);
  free(pic->reader);

  /* free global stacks */
  free(pic->try_jmps);
  xh_destroy(&pic->syms);
  xh_destroy(&pic->globals);
  xh_destroy(&pic->macros);

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
