/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "object.h"
#include "state.h"

void
pic_add_feature(pic_state *pic, const char *feature)
{
  pic_push(pic, pic_intern_cstr(pic, feature), pic->features);
}

static pic_value
pic_state_features(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic->features;
}

static pic_value
pic_state_global_objects(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic->globals;
}

static void
pic_init_state(pic_state *pic)
{
  pic_defun(pic, "features", pic_state_features);
  pic_defun(pic, "global-objects", pic_state_global_objects);

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
void pic_init_var(pic_state *);
void pic_init_write(pic_state *);
void pic_init_read(pic_state *);
void pic_init_dict(pic_state *);
void pic_init_record(pic_state *);
void pic_init_weak(pic_state *);
void pic_init_load(pic_state *);
void pic_init_file(pic_state *);
void pic_init_state(pic_state *);
void pic_init_eval(pic_state *);

#define DONE pic_leave(pic, ai);

static void
pic_init_core(pic_state *pic)
{
  size_t ai = pic_enter(pic);

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
  pic_init_var(pic); DONE;
  pic_init_dict(pic); DONE;
  pic_init_record(pic); DONE;
  pic_init_weak(pic); DONE;
  pic_init_state(pic); DONE;
  pic_init_load(pic); DONE;
  pic_init_read(pic); DONE;

#if PIC_USE_WRITE
  pic_init_write(pic); DONE;
#endif
#if PIC_USE_LIBC
  pic_init_file(pic); DONE;
#endif
#if PIC_USE_EVAL
  pic_init_eval(pic); DONE;
#endif
}

pic_state *
pic_open(pic_allocf allocf, void *userdata)
{
  pic_state *pic;

  pic = allocf(userdata, NULL, sizeof(pic_state));

  if (! pic) {
    goto EXIT_PIC;
  }

  /* allocator */
  pic->allocf = allocf;

  /* user data */
  pic->userdata = userdata;

  /* context */
  pic->default_cxt.ai = 0;
  pic->default_cxt.pc = NULL;
  pic->default_cxt.fp = NULL;
  pic->default_cxt.sp = NULL;
  pic->default_cxt.irep = NULL;
  pic->default_cxt.prev = NULL;
  pic->cxt = &pic->default_cxt;

  /* arena */
  pic->arena = allocf(userdata, NULL, PIC_ARENA_SIZE * sizeof(struct object *));
  pic->arena_size = PIC_ARENA_SIZE;

  if (! pic->arena) {
    goto EXIT_ARENA;
  }

  /* turn off GC */
  pic->gc_enable = false;

  /* memory heap */
  pic->heap = pic_heap_open(pic);

  /* symbol table */
  kh_init(oblist, &pic->oblist);

  /* global variables */
  pic->globals = pic_make_dict(pic);

  /* features */
  pic->features = pic_nil_value(pic);

  /* dynamic environment */
  pic->dyn_env = pic_list(pic, 1, pic_make_weak(pic));

  /* top continuation */
  {
    static const code_t halt_code[] = { 0x00, 0x01 };
    struct irep *irep;
    struct proc *proc;
    irep = (struct irep *)pic_obj_alloc(pic, PIC_TYPE_IREP);
    irep->argc = 1;
    irep->flags = IREP_CODE_STATIC;
    irep->frame_size = 1;
    irep->irepc = 0;
    irep->objc = 0;
    irep->irep = NULL;
    irep->obj = NULL;
    irep->code = halt_code;
    proc = (struct proc *)pic_obj_alloc(pic, PIC_TYPE_PROC_IREP);
    proc->u.irep = irep;
    proc->env = NULL;
    pic->halt = obj_value(pic, proc);
  }

  /* panic handler */
  pic->panicf = NULL;

  /* turn on GC */
  pic->gc_enable = true;

  pic_init_core(pic);

  pic_leave(pic, 0);            /* empty arena */

  return pic;

 EXIT_ARENA:
  allocf(userdata, pic, 0);
 EXIT_PIC:
  return NULL;
}

void
pic_close(pic_state *pic)
{
  pic_allocf allocf = pic->allocf;

  /* clear out root objects */
  pic->cxt = &pic->default_cxt;
  pic->cxt->ai = 0;
  pic->halt = pic_invalid_value(pic);
  pic->globals = pic_invalid_value(pic);
  pic->features = pic_invalid_value(pic);
  pic->dyn_env = pic_invalid_value(pic);

  assert(pic->cxt->pc == NULL);
  assert(pic->cxt->fp == NULL);
  assert(pic->cxt->sp == NULL);
  assert(pic->cxt->irep == NULL);
  assert(pic->cxt->prev == NULL);

  /* free all heap objects */
  pic_gc(pic);

  /* free heaps */
  pic_heap_close(pic, pic->heap);

  /* free global stacks */
  kh_destroy(oblist, &pic->oblist);

  /* free GC arena */
  allocf(pic->userdata, pic->arena, 0);
  allocf(pic->userdata, pic, 0);
}

pic_value
pic_global_ref(pic_state *pic, pic_value sym)
{
  pic_value val;

  if (! pic_dict_has(pic, pic->globals, sym)) {
    pic_error(pic, "undefined variable", 1, sym);
  }
  val = pic_dict_ref(pic, pic->globals, sym);
  /* FIXME */
  /* if (pic_invalid_p(pic, val)) { */
  /*   pic_error(pic, "uninitialized global variable", 1, sym); */
  /* } */
  return val;
}

void
pic_global_set(pic_state *pic, pic_value sym, pic_value value)
{
  /* FIXME */
  /* if (! pic_dict_has(pic, pic->globals, sym)) { */
  /*   pic_error(pic, "undefined variable", 1, sym); */
  /* } */
  pic_dict_set(pic, pic->globals, sym, value);
}

pic_value
pic_ref(pic_state *pic, const char *name)
{
  return pic_global_ref(pic, pic_intern_cstr(pic, name));
}

void
pic_set(pic_state *pic, const char *name, pic_value val)
{
  pic_global_set(pic, pic_intern_cstr(pic, name), val);
}

void
pic_define(pic_state *pic, const char *name, pic_value val)
{
  pic_value sym = pic_intern_cstr(pic, name);

  if (pic_dict_has(pic, pic->globals, sym)) {
    pic_warnf(pic, "redefining variable: %s", pic_str(pic, pic_sym_name(pic, sym), NULL));
  }
  pic_dict_set(pic, pic->globals, sym, val);
}

void
pic_defun(pic_state *pic, const char *name, pic_func_t f)
{
  pic_define(pic, name, pic_make_proc_func(pic, f));
}

void
pic_defvar(pic_state *pic, const char *name, pic_value init)
{
  pic_define(pic, name, pic_make_var(pic, init, pic_false_value(pic)));
}

pic_value
pic_funcall(pic_state *pic, const char *name, int n, ...)
{
  pic_value proc, r;
  va_list ap;

  proc = pic_ref(pic, name);

  TYPE_CHECK(pic, proc, proc);

  va_start(ap, n);
  r = pic_vcall(pic, proc, n, ap);
  va_end(ap);

  return r;
}
