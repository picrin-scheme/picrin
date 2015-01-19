/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/cont.h"
#include "picrin/data.h"
#include "picrin/string.h"
#include "picrin/error.h"

void
pic_panic(pic_state *pic, const char *msg)
{
  PIC_UNUSED(pic);

  fprintf(stderr, "abort: %s\n", msg);
  abort();
}

void
pic_warnf(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_value err_line;

  va_start(ap, fmt);
  err_line = pic_xvformat(pic, fmt, ap);
  va_end(ap);

  fprintf(stderr, "warn: %s\n", pic_str_cstr(pic_str_ptr(pic_car(pic, err_line))));
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

  msg = pic_str_cstr(pic_str_ptr(pic_car(pic, err_line)));
  irrs = pic_cdr(pic, err_line);

  pic_error(pic, msg, irrs);
}

const char *
pic_errmsg(pic_state *pic)
{
  pic_str *str;

  assert(! pic_undef_p(pic->err));

  if (! pic_error_p(pic->err)) {
    str = pic_format(pic, "~s", pic->err);
  } else {
    str = pic_error_ptr(pic->err)->msg;
  }

  return pic_str_cstr(str);
}

static pic_value
native_exception_handler(pic_state *pic)
{
  pic_value err;
  struct pic_proc *cont;

  pic_get_args(pic, "o", &err);

  pic->err = err;

  cont = pic_proc_ptr(pic_attr_ref(pic, pic_obj_value(pic_get_proc(pic)), "@@escape"));

  pic_apply1(pic, cont, pic_false_value());

  PIC_UNREACHABLE();
}

void
pic_push_try(pic_state *pic, struct pic_escape *escape)
{
  struct pic_proc *cont, *handler;
  size_t xp_len;
  ptrdiff_t xp_offset;

  cont = pic_make_econt(pic, escape);

  handler = pic_make_proc(pic, native_exception_handler, "(native-exception-handler)");

  pic_attr_set(pic, pic_obj_value(handler), "@@escape", pic_obj_value(cont));

  if (pic->xp >= pic->xpend) {
    xp_len = (size_t)(pic->xpend - pic->xpbase) * 2;
    xp_offset = pic->xp - pic->xpbase;
    pic->xpbase = pic_realloc(pic, pic->xpbase, sizeof(struct pic_proc *) * xp_len);
    pic->xp = pic->xpbase + xp_offset;
    pic->xpend = pic->xpbase + xp_len;
  }

  *pic->xp++ = handler;
}

void
pic_pop_try(pic_state *pic)
{
  pic_value cont, escape;

  assert(pic->xp > pic->xpbase);

  cont = pic_attr_ref(pic, pic_obj_value(*--pic->xp), "@@escape");

  assert(pic_proc_p(cont));

  escape = pic_attr_ref(pic, cont, "@@escape");

  assert(pic_data_p(escape));

  ((struct pic_escape *)pic_data_ptr(escape)->data)->valid = false;
}

struct pic_error *
pic_make_error(pic_state *pic, pic_sym type, const char *msg, pic_value irrs)
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

  if (pic->xp == pic->xpbase) {
    pic_panic(pic, "no exception handler registered");
  }

  handler = *--pic->xp;

  pic_gc_protect(pic, pic_obj_value(handler));

  v = pic_apply1(pic, handler, err);

  *pic->xp++ = handler;

  return v;
}

void
pic_raise(pic_state *pic, pic_value err)
{
  pic_value val;

  val = pic_raise_continuable(pic, err);

  pic_pop_try(pic);

  pic_errorf(pic, "error handler returned with ~s on error ~s", val, err);
}

void
pic_throw(pic_state *pic, pic_sym type, const char *msg, pic_value irrs)
{
  struct pic_error *e;

  e = pic_make_error(pic, type, msg, irrs);

  pic_raise(pic, pic_obj_value(e));
}

void
pic_error(pic_state *pic, const char *msg, pic_value irrs)
{
  pic_throw(pic, pic_intern_cstr(pic, ""), msg, irrs);
}

static pic_value
pic_error_with_exception_handler(pic_state *pic)
{
  struct pic_proc *handler, *thunk;
  pic_value val;
  size_t xp_len;
  ptrdiff_t xp_offset;

  pic_get_args(pic, "ll", &handler, &thunk);

  if (pic->xp >= pic->xpend) {
    xp_len = (size_t)(pic->xpend - pic->xpbase) * 2;
    xp_offset = pic->xp - pic->xpbase;
    pic->xpbase = pic_realloc(pic, pic->xpbase, sizeof(struct pic_proc *) * xp_len);
    pic->xp = pic->xpbase + xp_offset;
    pic->xpend = pic->xpbase + xp_len;
  }

  *pic->xp++ = handler;

  val = pic_apply0(pic, thunk);

  --pic->xp;

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
  size_t argc;
  pic_value *argv;

  pic_get_args(pic, "z*", &str, &argc, &argv);

  pic_error(pic, str, pic_list_by_array(pic, argc, argv));
}

static pic_value
pic_error_make_error_object(pic_state *pic)
{
  struct pic_error *e;
  pic_sym type;
  pic_str *msg;
  size_t argc;
  pic_value *argv;

  pic_get_args(pic, "ms*", &type, &msg, &argc, &argv);

  e = pic_make_error(pic, type, pic_str_cstr(msg), pic_list_by_array(pic, argc, argv));

  return pic_obj_value(e);
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
  pic_defun(pic, "make-error-object", pic_error_make_error_object);
  pic_defun(pic, "error-object?", pic_error_error_object_p);
  pic_defun(pic, "error-object-message", pic_error_error_object_message);
  pic_defun(pic, "error-object-irritants", pic_error_error_object_irritants);
  pic_defun(pic, "error-object-type", pic_error_error_object_type);
}
