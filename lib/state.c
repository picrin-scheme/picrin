/**
 * See Copyright Notice in picrin.h
 */

#include <picrin.h>
#include <picrin/extra.h>
#include "value.h"
#include "object.h"
#include "state.h"

static pic_value pic_state_features(pic_state *);

void
pic_add_feature(pic_state *pic, const char *feature)
{
  pic_value f = pic_ref(pic, "__picrin_features__");

  pic_set(pic, "__picrin_features__", pic_cons(pic, pic_intern_cstr(pic, feature), f));
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
void pic_init_attr(pic_state *);
void pic_init_file(pic_state *);
void pic_init_state(pic_state *);
void pic_init_eval(pic_state *);

#define DONE pic_leave(pic, ai);

static void
pic_init_core(pic_state *pic)
{
  size_t ai = pic_enter(pic);

  pic_define(pic, "__picrin_features__", pic_nil_value(pic));
  pic_define(pic, "__picrin_dynenv__", pic_list(pic, 1, pic_make_attr(pic)));

  pic_init_bool(pic); DONE;
  pic_init_pair(pic); DONE;
  pic_init_number(pic); DONE;
  pic_init_proc(pic); DONE;
  pic_init_symbol(pic); DONE;
  pic_init_vector(pic); DONE;
  pic_init_blob(pic); DONE;
  pic_init_char(pic); DONE;
  pic_init_str(pic); DONE;
  pic_init_var(pic); DONE;
  pic_init_dict(pic); DONE;
  pic_init_record(pic); DONE;
  pic_init_attr(pic); DONE;
  pic_init_state(pic); DONE;

#if PIC_USE_CONT
  pic_init_cont(pic); DONE;
#endif
#if PIC_USE_PORT
  pic_init_port(pic); DONE;
#endif
#if PIC_USE_READ
  pic_init_read(pic); DONE;
#endif
#if PIC_USE_WRITE
  pic_init_write(pic); DONE;
#endif
#if PIC_USE_FILE
  pic_init_file(pic); DONE;
#endif
#if PIC_USE_EVAL
  pic_init_eval(pic); DONE;
#endif
#if PIC_USE_ERROR
  pic_init_error(pic); DONE;
#endif
}

pic_state *
pic_open(pic_allocf allocf, void *userdata, pic_panicf panicf)
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

  /* panic handler */
  pic->panicf = panicf;

  /* context */
  pic->default_cxt.ai = 0;
  pic->default_cxt.pc = NULL;
  pic->default_cxt.fp = NULL;
  pic->default_cxt.sp = NULL;
  pic->default_cxt.irep = NULL;
  pic->default_cxt.prev = NULL;
  pic->default_cxt.conts = pic_nil_value(pic);
  pic->cxt = &pic->default_cxt;

  /* arena */
  pic->arena = allocf(userdata, NULL, PIC_ARENA_SIZE * sizeof(struct object *));
  pic->arena_size = PIC_ARENA_SIZE;
  pic->ai = 0;

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

  /* top continuation */
  {
    static const code_t halt_code[] = { 0x00 };
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
    irep->codec = sizeof halt_code / sizeof halt_code[0];
    proc = (struct proc *)pic_obj_alloc(pic, PIC_TYPE_PROC_IREP);
    proc->u.irep = irep;
    proc->env = NULL;
    pic->halt = obj_value(pic, proc);
  }

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
  pic->ai = 0;
  pic->halt = pic_invalid_value(pic);
  pic->globals = pic_invalid_value(pic);

  assert(pic->cxt->ai == 0);
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

void
pic_warnf(pic_state *PIC_UNUSED(pic), const char *PIC_UNUSED(fmt), ...)
{
#if PIC_USE_FILE
  va_list ap;
  pic_value err;

  va_start(ap, fmt);
  err = pic_vstrf_value(pic, fmt, ap);
  va_end(ap);
  pic_fprintf(pic, pic_stderr(pic), "warn: %s\n", pic_cstr(pic, err, NULL));
#endif
}

pic_value
pic_global_ref(pic_state *pic, pic_value sym)
{
  if (! pic_dict_has(pic, pic->globals, sym)) {
    pic_error(pic, "undefined variable", 1, sym);
  }
  return pic_dict_ref(pic, pic->globals, sym);
}

void
pic_global_set(pic_state *pic, pic_value sym, pic_value value)
{
  pic_dict_set(pic, pic->globals, sym, value);
}

pic_value
pic_ref(pic_state *pic, const char *name)
{
  size_t ai = pic_enter(pic);
  pic_value r = pic_global_ref(pic, pic_intern_cstr(pic, name));
  pic_leave(pic, ai);
  return pic_protect(pic, r);
}

void
pic_set(pic_state *pic, const char *name, pic_value val)
{
  size_t ai = pic_enter(pic);
  pic_global_set(pic, pic_intern_cstr(pic, name), val);
  pic_leave(pic, ai);
}

void
pic_define(pic_state *pic, const char *name, pic_value val)
{
  pic_value sym = pic_intern_cstr(pic, name);

  if (pic_dict_has(pic, pic->globals, sym)) {
    pic_warnf(pic, "redefining variable: %s", name);
  }
  pic_dict_set(pic, pic->globals, sym, val);
}

void
pic_defun(pic_state *pic, const char *name, pic_func_t f)
{
  pic_define(pic, name, pic_lambda(pic, f, 0));
}

void
pic_defvar(pic_state *pic, const char *name, pic_value init)
{
  pic_define(pic, name, pic_make_var(pic, init, pic_false_value(pic)));
}

pic_value
pic_funcall(pic_state *pic, const char *name, int n, ...)
{
  size_t ai = pic_enter(pic);
  pic_value proc, r;
  va_list ap;

  proc = pic_ref(pic, name);

  TYPE_CHECK(pic, proc, proc);

  va_start(ap, n);
  r = pic_vcall(pic, proc, n, ap);
  va_end(ap);

  pic_leave(pic, ai);
  return pic_protect(pic, r);
}

#if PIC_USE_LIBC
void
pic_default_panicf(pic_state *PIC_UNUSED(pic), const char *msg, int PIC_UNUSED(n), pic_value *PIC_UNUSED(args))
{
  fprintf(stderr, "panic!: %s\n", msg);
  abort();
}
#endif

void
pic_error(pic_state *pic, const char *msg, int n, ...)
{
  va_list ap;

  va_start(ap, n);
  pic_verror(pic, msg, n, ap);
  va_end(ap);
  PIC_UNREACHABLE();
}

void
pic_verror(pic_state *pic, const char *msg, int n, va_list ap)
{
  pic_value error = pic_ref(pic, "error");
  int i;
  pic_value *args;

  args = pic_alloca(pic, sizeof(pic_value) * (n + 1));
  args[0] = pic_cstr_value(pic, msg);
  for (i = 0; i < n; ++i) {
    args[i + 1] = va_arg(ap, pic_value);
  }

  pic_apply(pic, error, n + 1, args);
  PIC_UNREACHABLE();
}

static pic_value
pic_state_features(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_ref(pic, "__picrin_features__");
}

static pic_value
pic_state_global_objects(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic->globals;
}

static pic_value
pic_state_error(pic_state *pic)
{
  const char *msg;
  int argc;
  pic_value *args;

  pic_get_args(pic, "z*", &msg, &argc, &args);

  pic->panicf(pic, msg, argc, args);
  PIC_UNREACHABLE();
}

void
pic_init_state(pic_state *pic)
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

  pic_defun(pic, "features", pic_state_features);
  pic_defun(pic, "global-objects", pic_state_global_objects);
  pic_defun(pic, "error", pic_state_error);
}
