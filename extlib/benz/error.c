/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

void
pic_panic(pic_state PIC_UNUSED(*pic), const char *msg)
{
  extern PIC_NORETURN void abort();

#if DEBUG
  fprintf(stderr, "abort: %s\n", msg);
#else
  (void)msg;
#endif
  PIC_ABORT(pic);
}

void
pic_warnf(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_value err_line;

  va_start(ap, fmt);
  err_line = pic_xvformat(pic, fmt, ap);
  va_end(ap);

  xfprintf(pic, pic_stderr(pic)->file, "warn: %s\n", pic_str_cstr(pic, pic_str_ptr(pic_car(pic, err_line))));
}

void
pic_errorf(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_value err_line, irrs;
  const char *msg;

  va_start(ap, fmt);
  err_line = pic_xvformat(pic, fmt, ap);
  va_end(ap);

  msg = pic_str_cstr(pic, pic_str_ptr(pic_car(pic, err_line)));
  irrs = pic_cdr(pic, err_line);

  pic_error(pic, msg, irrs);
}

pic_value
pic_native_exception_handler(pic_state *pic)
{
  pic_value err;
  struct pic_proc *cont;

  pic_get_args(pic, "o", &err);

  pic->err = err;

  cont = pic_proc_ptr(pic_proc_env_ref(pic, pic_get_proc(pic), "cont"));

  pic_apply1(pic, cont, pic_false_value());

  PIC_UNREACHABLE();
}

void
pic_push_handler(pic_state *pic, struct pic_proc *handler)
{
  size_t xp_len;
  ptrdiff_t xp_offset;

  if (pic->xp >= pic->xpend) {
    xp_len = (size_t)(pic->xpend - pic->xpbase) * 2;
    xp_offset = pic->xp - pic->xpbase;
    pic->xpbase = pic_realloc(pic, pic->xpbase, sizeof(struct pic_proc *) * xp_len);
    pic->xp = pic->xpbase + xp_offset;
    pic->xpend = pic->xpbase + xp_len;
  }

  *pic->xp++ = handler;
}

struct pic_proc *
pic_pop_handler(pic_state *pic)
{
  if (pic->xp == pic->xpbase) {
    pic_panic(pic, "no exception handler registered");
  }

  return *--pic->xp;
}

struct pic_error *
pic_make_error(pic_state *pic, pic_sym *type, const char *msg, pic_value irrs)
{
  struct pic_error *e;
  pic_str *stack;

  stack = pic_get_backtrace(pic);

  e = (struct pic_error *)pic_obj_alloc(pic, sizeof(struct pic_error), PIC_TT_ERROR);
  e->type = type;
  e->msg = pic_make_str_cstr(pic, msg);
  e->irrs = irrs;
  e->stack = stack;

  return e;
}

pic_value
pic_raise_continuable(pic_state *pic, pic_value err)
{
  struct pic_proc *handler;
  pic_value v;

  handler = pic_pop_handler(pic);

  pic_gc_protect(pic, pic_obj_value(handler));

  v = pic_apply1(pic, handler, err);

  pic_push_handler(pic, handler);

  return v;
}

void
pic_raise(pic_state *pic, pic_value err)
{
  pic_value val;

  val = pic_raise_continuable(pic, err);

  pic_pop_handler(pic);

  pic_errorf(pic, "error handler returned with ~s on error ~s", val, err);
}

void
pic_error(pic_state *pic, const char *msg, pic_value irrs)
{
  struct pic_error *e;

  e = pic_make_error(pic, pic_intern(pic, ""), msg, irrs);

  pic_raise(pic, pic_obj_value(e));
}

static pic_value
pic_error_with_exception_handler(pic_state *pic)
{
  struct pic_proc *handler, *thunk;
  pic_value val;

  pic_get_args(pic, "ll", &handler, &thunk);

  pic_push_handler(pic, handler);

  val = pic_apply0(pic, thunk);

  pic_pop_handler(pic);

  return val;
}

static pic_value
pic_error_raise(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  pic_raise(pic, v);
}

static pic_value
pic_error_raise_continuable(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_raise_continuable(pic, v);
}

static pic_value
pic_error_error(pic_state *pic)
{
  const char *str;
  int argc;
  pic_value *argv;

  pic_get_args(pic, "z*", &str, &argc, &argv);

  pic_error(pic, str, pic_list_by_array(pic, argc, argv));
}

static pic_value
pic_error_error_object_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_error_p(v));
}

static pic_value
pic_error_error_object_message(pic_state *pic)
{
  struct pic_error *e;

  pic_get_args(pic, "e", &e);

  return pic_obj_value(e->msg);
}

static pic_value
pic_error_error_object_irritants(pic_state *pic)
{
  struct pic_error *e;

  pic_get_args(pic, "e", &e);

  return e->irrs;
}

static pic_value
pic_error_error_object_type(pic_state *pic)
{
  struct pic_error *e;

  pic_get_args(pic, "e", &e);

  return pic_obj_value(e->type);
}

void
pic_init_error(pic_state *pic)
{
  pic_defun(pic, "with-exception-handler", pic_error_with_exception_handler);
  pic_defun(pic, "raise", pic_error_raise);
  pic_defun(pic, "raise-continuable", pic_error_raise_continuable);
  pic_defun(pic, "error", pic_error_error);
  pic_defun(pic, "error-object?", pic_error_error_object_p);
  pic_defun(pic, "error-object-message", pic_error_error_object_message);
  pic_defun(pic, "error-object-irritants", pic_error_error_object_irritants);
  pic_defun(pic, "error-object-type", pic_error_error_object_type);
}
