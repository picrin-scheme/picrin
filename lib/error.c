/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"
#include "state.h"

void
pic_panic(pic_state *pic, const char *msg)
{
  if (pic->panicf) {
    pic->panicf(pic, msg);
  }
  PIC_ABORT(pic);
  PIC_UNREACHABLE();
}

void
pic_warnf(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_value err;

  va_start(ap, fmt);
  err = pic_vstrf_value(pic, fmt, ap);
  va_end(ap);
  pic_fprintf(pic, pic_stderr(pic), "warn: %s\n", pic_str(pic, err, NULL));
}

static pic_value
native_exception_handler(pic_state *pic)
{
  pic_value err;

  pic_get_args(pic, "o", &err);

  pic->err = err;

  pic_call(pic, pic_closure_ref(pic, 0), 1, pic_false_value(pic));

  PIC_UNREACHABLE();
}

void
pic_start_try(pic_state *pic, PIC_JMPBUF *jmp)
{
  struct cont *cont;
  pic_value handler;
  pic_value var, env;

  /* call/cc */

  cont = pic_alloca_cont(pic);
  pic_save_point(pic, cont, jmp);
  handler = pic_lambda(pic, native_exception_handler, 1, pic_make_cont(pic, cont));

  /* with-exception-handler */

  var = pic_ref(pic, "picrin.base", "current-exception-handlers");
  env = pic_make_weak(pic);
  pic_weak_set(pic, env, var, pic_cons(pic, handler, pic_call(pic, var, 0)));
  pic->dyn_env = pic_cons(pic, env, pic->dyn_env);
}

void
pic_end_try(pic_state *pic)
{
  pic->dyn_env = pic_cdr(pic, pic->dyn_env);

  pic_exit_point(pic);
}

pic_value
pic_err(pic_state *pic)
{
  return pic->err;
}

pic_value
pic_make_error(pic_state *pic, const char *type, const char *msg, pic_value irrs)
{
  struct error *e;
  pic_value stack, ty = pic_intern_cstr(pic, type);

  stack = pic_get_backtrace(pic);

  e = (struct error *)pic_obj_alloc(pic, sizeof(struct error), PIC_TYPE_ERROR);
  e->type = pic_sym_ptr(pic, ty);
  e->msg = pic_str_ptr(pic, pic_cstr_value(pic, msg));
  e->irrs = irrs;
  e->stack = pic_str_ptr(pic, stack);

  return obj_value(pic, e);
}

static pic_value
with_exception_handlers(pic_state *pic, pic_value handlers, pic_value thunk)
{
  pic_value alist, var = pic_ref(pic, "picrin.base", "current-exception-handlers");
  alist = pic_list(pic, 1, pic_cons(pic, var, handlers));
  return pic_funcall(pic, "picrin.base", "with-dynamic-environment", 2, alist, thunk);
}

static pic_value
on_raise(pic_state *pic)
{
  pic_value handler, err, val;
  bool continuable;

  pic_get_args(pic, "");

  handler = pic_closure_ref(pic, 0);
  err = pic_closure_ref(pic, 1);
  continuable = pic_bool(pic, pic_closure_ref(pic, 2));

  val = pic_call(pic, handler, 1, err);
  if (! continuable) {
    pic_error(pic, "handler returned", 2, handler, err);
  }
  return val;
}

pic_value
pic_raise_continuable(pic_state *pic, pic_value err)
{
  pic_value handlers, var = pic_ref(pic, "picrin.base", "current-exception-handlers"), thunk;

  handlers = pic_call(pic, var, 0);

  if (pic_nil_p(pic, handlers)) {
    pic_panic(pic, "no exception handler");
  }
  thunk = pic_lambda(pic, on_raise, 3, pic_car(pic, handlers), err, pic_true_value(pic));
  return with_exception_handlers(pic, pic_cdr(pic, handlers), thunk);
}

void
pic_raise(pic_state *pic, pic_value err)
{
  pic_value handlers, var = pic_ref(pic, "picrin.base", "current-exception-handlers"), thunk;

  handlers = pic_call(pic, var, 0);

  if (pic_nil_p(pic, handlers)) {
    pic_panic(pic, "no exception handler");
  }
  thunk = pic_lambda(pic, on_raise, 3, pic_car(pic, handlers), err, pic_false_value(pic));
  with_exception_handlers(pic, pic_cdr(pic, handlers), thunk);
  PIC_UNREACHABLE();
}

void
pic_error(pic_state *pic, const char *msg, int n, ...)
{
  va_list ap;
  pic_value irrs;

  va_start(ap, n);
  irrs = pic_vlist(pic, n, ap);
  va_end(ap);
  pic_raise(pic, pic_make_error(pic, "", msg, irrs));
}

static pic_value
pic_error_with_exception_handler(pic_state *pic)
{
  pic_value handler, thunk;
  pic_value handlers, exc = pic_ref(pic, "picrin.base", "current-exception-handlers");

  pic_get_args(pic, "ll", &handler, &thunk);

  handlers = pic_call(pic, exc, 0);

  return with_exception_handlers(pic, pic_cons(pic, handler, handlers), thunk);
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
  const char *cstr;
  int argc;
  pic_value *argv;

  pic_get_args(pic, "z*", &cstr, &argc, &argv);

  pic_raise(pic, pic_make_error(pic, "", cstr, pic_make_list(pic, argc, argv)));
}

static pic_value
pic_error_error_object_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_error_p(pic, v));
}

static pic_value
pic_error_error_object_message(pic_state *pic)
{
  pic_value e;

  pic_get_args(pic, "o", &e);

  TYPE_CHECK(pic, e, error);

  return obj_value(pic, pic_error_ptr(pic, e)->msg);
}

static pic_value
pic_error_error_object_irritants(pic_state *pic)
{
  pic_value e;

  pic_get_args(pic, "o", &e);

  TYPE_CHECK(pic, e, error);

  return pic_error_ptr(pic, e)->irrs;
}

static pic_value
pic_error_error_object_type(pic_state *pic)
{
  pic_value e;

  pic_get_args(pic, "o", &e);

  TYPE_CHECK(pic, e, error);

  return obj_value(pic, pic_error_ptr(pic, e)->type);
}

void
pic_init_error(pic_state *pic)
{
  pic_defvar(pic, "current-exception-handlers", pic_nil_value(pic));
  pic_defun(pic, "with-exception-handler", pic_error_with_exception_handler);
  pic_defun(pic, "raise", pic_error_raise);
  pic_defun(pic, "raise-continuable", pic_error_raise_continuable);
  pic_defun(pic, "error", pic_error_error);
  pic_defun(pic, "error-object?", pic_error_error_object_p);
  pic_defun(pic, "error-object-message", pic_error_error_object_message);
  pic_defun(pic, "error-object-irritants", pic_error_error_object_irritants);
  pic_defun(pic, "error-object-type", pic_error_error_object_type);
}
