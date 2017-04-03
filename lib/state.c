/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "object.h"
#include "state.h"

static pic_value
pic_features(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic->features;
}

static void
pic_init_features(pic_state *pic)
{
  pic_defun(pic, "features", pic_features);

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

void
pic_add_feature(pic_state *pic, const char *feature)
{
  pic_push(pic, pic_intern_cstr(pic, feature), pic->features);
}

static pic_value
pic_global_objects(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic->globals;
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
void pic_init_compile(pic_state *);
void pic_init_weak(pic_state *);

void pic_boot(pic_state *);

#define DONE pic_leave(pic, ai);

static void
pic_init_core(pic_state *pic)
{
  size_t ai = pic_enter(pic);

  pic_defun(pic, "global-objects", pic_global_objects);

  pic_init_features(pic); DONE;
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
  pic_init_read(pic); DONE;
  pic_init_dict(pic); DONE;
  pic_init_record(pic); DONE;
  pic_init_compile(pic); DONE;
  pic_init_weak(pic); DONE;

#if PIC_USE_WRITE
  pic_init_write(pic); DONE;
#endif

  pic_boot(pic); DONE;
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

  /* turn off GC */
  pic->gc_enable = false;

  /* continuation chain */
  pic->cc = NULL;

  /* prepare VM stack */
  pic->stbase = pic->sp = allocf(userdata, NULL, PIC_STACK_SIZE * sizeof(pic_value));
  pic->stend = pic->stbase + PIC_STACK_SIZE;

  if (! pic->sp) {
    goto EXIT_SP;
  }

  /* callinfo */
  pic->cibase = pic->ci = allocf(userdata, NULL, PIC_STACK_SIZE * sizeof(struct callinfo));
  pic->ciend = pic->cibase + PIC_STACK_SIZE;

  if (! pic->ci) {
    goto EXIT_CI;
  }

  /* GC arena */
  pic->arena = allocf(userdata, NULL, PIC_ARENA_SIZE * sizeof(struct object *));
  pic->arena_size = PIC_ARENA_SIZE;
  pic->arena_idx = 0;

  if (! pic->arena) {
    goto EXIT_ARENA;
  }

  /* memory heap */
  pic->heap = pic_heap_open(pic);

  /* symbol table */
  kh_init(oblist, &pic->oblist);

  /* unique symbol count */
  pic->ucnt = 0;

  /* global variables */
  pic->globals = pic_invalid_value(pic);

  /* macros */
  pic->macros = pic_invalid_value(pic);

  /* features */
  pic->features = pic_nil_value(pic);

  /* dynamic environment */
  pic->dyn_env = pic_invalid_value(pic);

  /* raised error object */
  pic->panicf = NULL;
  pic->err = pic_invalid_value(pic);

  /* root tables */
  pic->globals = pic_make_dict(pic);
  pic->macros = pic_make_dict(pic);
  pic->dyn_env = pic_list(pic, 1, pic_make_weak(pic));

  /* turn on GC */
  pic->gc_enable = true;

  pic_init_core(pic);

  pic_leave(pic, 0);            /* empty arena */

  return pic;

 EXIT_ARENA:
  allocf(userdata, pic->ci, 0);
 EXIT_CI:
  allocf(userdata, pic->sp, 0);
 EXIT_SP:
  allocf(userdata, pic, 0);
 EXIT_PIC:
  return NULL;
}

void
pic_close(pic_state *pic)
{
  pic_allocf allocf = pic->allocf;

  /* clear out root objects */
  pic->sp = pic->stbase;
  pic->ci = pic->cibase;
  pic->arena_idx = 0;
  pic->err = pic_invalid_value(pic);
  pic->globals = pic_invalid_value(pic);
  pic->macros = pic_invalid_value(pic);
  pic->features = pic_invalid_value(pic);
  pic->dyn_env = pic_invalid_value(pic);

  /* free all heap objects */
  pic_gc(pic);

  /* free heaps */
  pic_heap_close(pic, pic->heap);

  /* free runtime context */
  allocf(pic->userdata, pic->stbase, 0);
  allocf(pic->userdata, pic->cibase, 0);

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
  if (pic_invalid_p(pic, val)) {
    pic_error(pic, "uninitialized global variable", 1, sym);
  }
  return val;
}

void
pic_global_set(pic_state *pic, pic_value sym, pic_value value)
{
  if (! pic_dict_has(pic, pic->globals, sym)) {
    pic_error(pic, "undefined variable", 1, sym);
  }
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
  pic_define(pic, name, pic_make_proc(pic, f, 0, NULL));
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
