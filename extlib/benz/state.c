/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

void
pic_set_argv(pic_state *pic, int argc, char *argv[], char **envp)
{
  pic->argc = argc;
  pic->argv = argv;
  pic->envp = envp;
}

void
pic_add_feature(pic_state *pic, const char *feature)
{
  pic_push(pic, pic_obj_value(pic_intern(pic, feature)), pic->features);
}

void pic_init_bool(pic_state *);
void pic_init_pair(pic_state *);
void pic_init_port(pic_state *);
void pic_init_number(pic_state *);
void pic_init_proc(pic_state *);
void pic_init_symbol(pic_state *);
void pic_init_vector(pic_state *);
void pic_init_blob(pic_state *);
void pic_init_cont(pic_state *);
void pic_init_char(pic_state *);
void pic_init_error(pic_state *);
void pic_init_str(pic_state *);
void pic_init_macro(pic_state *);
void pic_init_var(pic_state *);
void pic_init_write(pic_state *);
void pic_init_read(pic_state *);
void pic_init_dict(pic_state *);
void pic_init_record(pic_state *);
void pic_init_eval(pic_state *);
void pic_init_lib(pic_state *);
void pic_init_attr(pic_state *);
void pic_init_reg(pic_state *);

extern const char pic_boot[][80];

static void
pic_init_features(pic_state *pic)
{
  pic_add_feature(pic, "picrin");

#if __STDC_IEC_559__
  pic_add_feature(pic, "ieee-float");
#endif

#if _POSIX_SOURCE
  pic_add_feature(pic, "posix");
#endif

#if _WIN32
  pic_add_feature(pic, "windows");
#endif

#if __unix__
  pic_add_feature(pic, "unix");
#endif
#if __gnu_linux__
  pic_add_feature(pic, "gnu-linux");
#endif
#if __FreeBSD__
  pic_add_feature(pic, "freebsd");
#endif

#if __i386__
  pic_add_feature(pic, "i386");
#elif __x86_64__
  pic_add_feature(pic, "x86-64");
#elif __ppc__
  pic_add_feature(pic, "ppc");
#elif __sparc__
  pic_add_feature(pic, "sparc");
#endif

#if __ILP32__
  pic_add_feature(pic, "ilp32");
#elif __LP64__
  pic_add_feature(pic, "lp64");
#endif

#if defined(__BYTE_ORDER__)
# if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  pic_add_feature(pic, "little-endian");
# elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  pic_add_feature(pic, "big-endian");
# endif
#else
# if __LITTLE_ENDIAN__
  pic_add_feature(pic, "little-endian");
# elif __BIG_ENDIAN__
  pic_add_feature(pic, "big-endian");
# endif
#endif
}

static pic_value
pic_features(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic->features;
}

#define DONE pic_gc_arena_restore(pic, ai);

#define define_builtin_syntax(uid, name)                                \
  pic_define_syntactic_keyword_(pic, pic->lib->env, pic_intern(pic, name), uid)

#define VM(uid, name)                                                   \
  pic_define_syntactic_keyword_(pic, pic->lib->env, pic_intern(pic, name), uid)

#define VM3(name)                                       \
  pic->c##name = pic_vm_gref_slot(pic, pic->u##name);

#define VM2(proc, name)                         \
  proc = pic_ref(pic, pic->lib, name)

static void
pic_init_core(pic_state *pic)
{
  void pic_define_syntactic_keyword_(pic_state *, struct pic_env *, pic_sym *, pic_sym *);
  struct pic_box *pic_vm_gref_slot(pic_state *, pic_sym *);

  pic_init_features(pic);

  pic_deflibrary (pic, "(picrin base)") {
    size_t ai = pic_gc_arena_preserve(pic);

    define_builtin_syntax(pic->uDEFINE, "builtin:define");
    define_builtin_syntax(pic->uSETBANG, "builtin:set!");
    define_builtin_syntax(pic->uQUOTE, "builtin:quote");
    define_builtin_syntax(pic->uLAMBDA, "builtin:lambda");
    define_builtin_syntax(pic->uIF, "builtin:if");
    define_builtin_syntax(pic->uBEGIN, "builtin:begin");
    define_builtin_syntax(pic->uDEFINE_MACRO, "builtin:define-macro");

    pic_defun(pic, "features", pic_features);

    VM(pic->uCONS, "cons");
    VM(pic->uCAR, "car");
    VM(pic->uCDR, "cdr");
    VM(pic->uNILP, "null?");
    VM(pic->uSYMBOLP, "symbol?");
    VM(pic->uPAIRP, "pair?");
    VM(pic->uNOT, "not");
    VM(pic->uADD, "+");
    VM(pic->uSUB, "-");
    VM(pic->uMUL, "*");
    VM(pic->uDIV, "/");
    VM(pic->uEQ, "=");
    VM(pic->uLT, "<");
    VM(pic->uLE, "<=");
    VM(pic->uGT, ">");
    VM(pic->uGE, ">=");

    pic_init_bool(pic); DONE;
    pic_init_pair(pic); DONE;
    pic_init_port(pic); DONE;
    pic_init_number(pic); DONE;
    pic_init_proc(pic); DONE;
    pic_init_symbol(pic); DONE;
    pic_init_vector(pic); DONE;
    pic_init_blob(pic); DONE;
    pic_init_cont(pic); DONE;
    pic_init_char(pic); DONE;
    pic_init_error(pic); DONE;
    pic_init_str(pic); DONE;
    pic_init_macro(pic); DONE;
    pic_init_var(pic); DONE;
    pic_init_write(pic); DONE;
    pic_init_read(pic); DONE;
    pic_init_dict(pic); DONE;
    pic_init_record(pic); DONE;
    pic_init_eval(pic); DONE;
    pic_init_lib(pic); DONE;
    pic_init_attr(pic); DONE;
    pic_init_reg(pic); DONE;

    VM3(CONS);
    VM3(CAR);
    VM3(CDR);
    VM3(NILP);
    VM3(SYMBOLP);
    VM3(PAIRP);
    VM3(NOT);
    VM3(ADD);
    VM3(SUB);
    VM3(MUL);
    VM3(DIV);
    VM3(EQ);
    VM3(LT);
    VM3(LE);
    VM3(GT);
    VM3(GE);

    VM2(pic->pCONS, "cons");
    VM2(pic->pCAR, "car");
    VM2(pic->pCDR, "cdr");
    VM2(pic->pNILP, "null?");
    VM2(pic->pSYMBOLP, "symbol?");
    VM2(pic->pPAIRP, "pair?");
    VM2(pic->pNOT, "not");
    VM2(pic->pADD, "+");
    VM2(pic->pSUB, "-");
    VM2(pic->pMUL, "*");
    VM2(pic->pDIV, "/");
    VM2(pic->pEQ, "=");
    VM2(pic->pLT, "<");
    VM2(pic->pLE, "<=");
    VM2(pic->pGT, ">");
    VM2(pic->pGE, ">=");

    pic_try {
      pic_load_cstr(pic, &pic_boot[0][0]);
    }
    pic_catch {
      pic_print_backtrace(pic, xstdout);
      pic_panic(pic, "");
    }
  }
}

pic_state *
pic_open(pic_allocf allocf, void *userdata)
{
  struct pic_port *pic_make_standard_port(pic_state *, xFILE *, short);
  char t;

  pic_state *pic;
  size_t ai;

  pic = allocf(NULL, sizeof(pic_state));

  if (! pic) {
    goto EXIT_PIC;
  }

  /* allocator */
  pic->allocf = allocf;

  /* user data */
  pic->userdata = userdata;

  /* turn off GC */
  pic->gc_enable = false;

  /* continuation chain */
  pic->cc = NULL;
  pic->ccnt = 0;

  /* root block */
  pic->cp = NULL;

  /* command line */
  pic->argc = 0;
  pic->argv = NULL;
  pic->envp = NULL;

  /* prepare VM stack */
  pic->stbase = pic->sp = allocf(NULL, PIC_STACK_SIZE * sizeof(pic_value));
  pic->stend = pic->stbase + PIC_STACK_SIZE;

  if (! pic->sp) {
    goto EXIT_SP;
  }

  /* callinfo */
  pic->cibase = pic->ci = allocf(NULL, PIC_STACK_SIZE * sizeof(pic_callinfo));
  pic->ciend = pic->cibase + PIC_STACK_SIZE;

  if (! pic->ci) {
    goto EXIT_CI;
  }

  /* exception handler */
  pic->xpbase = pic->xp = allocf(NULL, PIC_RESCUE_SIZE * sizeof(struct pic_proc *));
  pic->xpend = pic->xpbase + PIC_RESCUE_SIZE;

  if (! pic->xp) {
    goto EXIT_XP;
  }

  /* GC arena */
  pic->arena = allocf(NULL, PIC_ARENA_SIZE * sizeof(struct pic_object *));
  pic->arena_size = PIC_ARENA_SIZE;
  pic->arena_idx = 0;

  if (! pic->arena) {
    goto EXIT_ARENA;
  }

  /* memory heap */
  pic->heap = pic_heap_open(pic);

  /* symbol table */
  kh_init(s, &pic->syms);

  /* unique symbol count */
  pic->ucnt = 0;

  /* global variables */
  pic->globals = NULL;

  /* macros */
  pic->macros = NULL;

  /* attributes */
  pic->attrs = NULL;

  /* features */
  pic->features = pic_nil_value();

  /* libraries */
  pic->libs = pic_nil_value();
  pic->lib = NULL;

  /* raised error object */
  pic->err = pic_invalid_value();

  /* file pool */
  memset(pic->files, 0, sizeof pic->files);

  /* parameter table */
  pic->ptable = pic_nil_value();

  /* native stack marker */
  pic->native_stack_start = &t;

  ai = pic_gc_arena_preserve(pic);

#define S(slot,name) pic->slot = pic_intern(pic, name)

  S(sQUOTE, "quote");
  S(sQUASIQUOTE, "quasiquote");
  S(sUNQUOTE, "unquote");
  S(sUNQUOTE_SPLICING, "unquote-splicing");
  S(sSYNTAX_QUOTE, "syntax-quote");
  S(sSYNTAX_QUASIQUOTE, "syntax-quasiquote");
  S(sSYNTAX_UNQUOTE, "syntax-unquote");
  S(sSYNTAX_UNQUOTE_SPLICING, "syntax-unquote-splicing");
  S(sIMPORT, "import");
  S(sEXPORT, "export");
  S(sDEFINE_LIBRARY, "define-library");
  S(sCOND_EXPAND, "cond-expand");
  S(sCALL, "call");
  S(sGREF, "gref");
  S(sLREF, "lref");
  S(sCREF, "cref");

  pic_gc_arena_restore(pic, ai);

#define U(slot,name) pic->slot = pic_uniq(pic, pic_obj_value(pic_intern(pic, name)))

  U(uDEFINE, "define");
  U(uLAMBDA, "lambda");
  U(uIF, "if");
  U(uBEGIN, "begin");
  U(uSETBANG, "set!");
  U(uQUOTE, "quote");
  U(uDEFINE_MACRO, "define-macro");
  U(uIMPORT, "import");
  U(uEXPORT, "export");
  U(uDEFINE_LIBRARY, "define-library");
  U(uCOND_EXPAND, "cond-expand");
  U(uCONS, "cons");
  U(uCAR, "car");
  U(uCDR, "cdr");
  U(uNILP, "null?");
  U(uSYMBOLP, "symbol?");
  U(uPAIRP, "pair?");
  U(uADD, "+");
  U(uSUB, "-");
  U(uMUL, "*");
  U(uDIV, "/");
  U(uEQ, "=");
  U(uLT, "<");
  U(uLE, "<=");
  U(uGT, ">");
  U(uGE, ">=");
  U(uNOT, "not");
  pic_gc_arena_restore(pic, ai);

  /* system procedures */
  pic->pCONS = pic_invalid_value();
  pic->pCAR = pic_invalid_value();
  pic->pCDR = pic_invalid_value();
  pic->pNILP = pic_invalid_value();
  pic->pSYMBOLP = pic_invalid_value();
  pic->pPAIRP = pic_invalid_value();
  pic->pNOT = pic_invalid_value();
  pic->pADD = pic_invalid_value();
  pic->pSUB = pic_invalid_value();
  pic->pMUL = pic_invalid_value();
  pic->pDIV = pic_invalid_value();
  pic->pEQ = pic_invalid_value();
  pic->pLT = pic_invalid_value();
  pic->pLE = pic_invalid_value();
  pic->pGT = pic_invalid_value();
  pic->pGE = pic_invalid_value();

  /* root tables */
  pic->globals = pic_make_reg(pic);
  pic->macros = pic_make_reg(pic);
  pic->attrs = pic_make_reg(pic);

  /* root block */
  pic->cp = (pic_checkpoint *)pic_obj_alloc(pic, sizeof(pic_checkpoint), PIC_TT_CP);
  pic->cp->prev = NULL;
  pic->cp->depth = 0;
  pic->cp->in = pic->cp->out = NULL;

  /* reader */
  pic_reader_init(pic);

  /* parameter table */
  pic->ptable = pic_cons(pic, pic_obj_value(pic_make_reg(pic)), pic->ptable);

  /* standard libraries */
  pic->PICRIN_BASE = pic_make_library(pic, pic_read_cstr(pic, "(picrin base)"));
  pic->PICRIN_USER = pic_make_library(pic, pic_read_cstr(pic, "(picrin user)"));
  pic->lib = pic->PICRIN_USER;
  pic->prev_lib = NULL;

  pic_gc_arena_restore(pic, ai);

  /* turn on GC */
  pic->gc_enable = true;

  pic->cCONS = pic_box(pic, pic_invalid_value());
  pic->cCAR = pic_box(pic, pic_invalid_value());
  pic->cCDR = pic_box(pic, pic_invalid_value());
  pic->cNILP = pic_box(pic, pic_invalid_value());
  pic->cSYMBOLP = pic_box(pic, pic_invalid_value());
  pic->cPAIRP = pic_box(pic, pic_invalid_value());
  pic->cNOT = pic_box(pic, pic_invalid_value());
  pic->cADD = pic_box(pic, pic_invalid_value());
  pic->cSUB = pic_box(pic, pic_invalid_value());
  pic->cMUL = pic_box(pic, pic_invalid_value());
  pic->cDIV = pic_box(pic, pic_invalid_value());
  pic->cEQ = pic_box(pic, pic_invalid_value());
  pic->cLT = pic_box(pic, pic_invalid_value());
  pic->cLE = pic_box(pic, pic_invalid_value());
  pic->cGT = pic_box(pic, pic_invalid_value());
  pic->cGE = pic_box(pic, pic_invalid_value());

  pic_init_core(pic);

  pic_gc_arena_restore(pic, ai);

  return pic;

 EXIT_ARENA:
  allocf(pic->xp, 0);
 EXIT_XP:
  allocf(pic->ci, 0);
 EXIT_CI:
  allocf(pic->sp, 0);
 EXIT_SP:
  allocf(pic, 0);
 EXIT_PIC:
  return NULL;
}

void
pic_close(pic_state *pic)
{
  khash_t(s) *h = &pic->syms;
  pic_allocf allocf = pic->allocf;

  /* clear out root objects */
  pic->sp = pic->stbase;
  pic->ci = pic->cibase;
  pic->xp = pic->xpbase;
  pic->arena_idx = 0;
  pic->err = pic_invalid_value();
  pic->globals = NULL;
  pic->macros = NULL;
  pic->attrs = NULL;
  pic->features = pic_nil_value();
  pic->libs = pic_nil_value();

  /* free all heap objects */
  pic_gc_run(pic);

  /* flush all xfiles */
  xfflush(pic, NULL);

  /* free heaps */
  pic_heap_close(pic, pic->heap);

  /* free reader struct */
  pic_reader_destroy(pic);

  /* free runtime context */
  allocf(pic->stbase, 0);
  allocf(pic->cibase, 0);
  allocf(pic->xpbase, 0);

  /* free global stacks */
  kh_destroy(s, h);

  /* free GC arena */
  allocf(pic->arena, 0);

  allocf(pic, 0);
}
