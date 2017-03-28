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

  pic_fprintf(pic, pic_stderr(pic), "warn: %s\n", pic_str(pic, err));
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

static pic_value
dynamic_set(pic_state *pic)
{
  pic_value var, val;

  pic_get_args(pic, "");

  var = pic_closure_ref(pic, 0);
  val = pic_closure_ref(pic, 1);

  pic_proc_ptr(pic, var)->locals[0] = val;

  return pic_undef_value(pic);
}

pic_value
pic_start_try(pic_state *pic, PIC_JMPBUF *jmp)
{
  struct cont *cont;
  pic_value handler;
  pic_value var, old_val, new_val;
  pic_value in, out;
  struct checkpoint *here;

  /* call/cc */

  cont = pic_alloca_cont(pic);
  pic_save_point(pic, cont, jmp);
  handler = pic_lambda(pic, native_exception_handler, 1, pic_make_cont(pic, cont));

  /* with-exception-handler */

  var = pic_ref(pic, "picrin.base", "current-exception-handlers");
  old_val = pic_call(pic, var, 0);
  new_val = pic_cons(pic, handler, old_val);

  in = pic_lambda(pic, dynamic_set, 2, var, new_val);
  out = pic_lambda(pic, dynamic_set, 2, var, old_val);

  /* dynamic-wind */

  pic_call(pic, in, 0);       /* enter */

  here = pic->cp;
  pic->cp = (struct checkpoint *)pic_obj_alloc(pic, sizeof(struct checkpoint), PIC_TYPE_CP);
  pic->cp->prev = here;
  pic->cp->depth = here->depth + 1;
  pic->cp->in = pic_proc_ptr(pic, in);
  pic->cp->out = pic_proc_ptr(pic, out);

  return pic_cons(pic, pic_obj_value(here), out);
}

void
pic_end_try(pic_state *pic, pic_value cookie)
{
  struct checkpoint *here = (struct checkpoint *)pic_obj_ptr(pic_car(pic, cookie));
  pic_value out = pic_cdr(pic, cookie);

  pic->cp = here;

  pic_call(pic, out, 0); /* exit */

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

  return pic_obj_value(e);
}

pic_value pic_raise_continuable(pic_state *, pic_value err);

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
raise_action(pic_state *pic)
{
  pic_get_args(pic, "");

  pic_call(pic, pic_closure_ref(pic, 0), 1, pic_closure_ref(pic, 1));

  pic_error(pic, "handler returned", 2, pic_closure_ref(pic, 0), pic_closure_ref(pic, 1));
}

void
pic_raise(pic_state *pic, pic_value err)
{
  pic_value stack, exc = pic_ref(pic, "picrin.base", "current-exception-handlers");

  stack = pic_call(pic, exc, 0);

  if (pic_nil_p(pic, stack)) {
    pic_panic(pic, "no exception handler");
  }

  pic_dynamic_bind(pic, exc, pic_cdr(pic, stack), pic_lambda(pic, raise_action, 2, pic_car(pic, stack), err));

  PIC_UNREACHABLE();
}

static pic_value
raise_continuable(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_call(pic, pic_closure_ref(pic, 0), 1, pic_closure_ref(pic, 1));
}

pic_value
pic_raise_continuable(pic_state *pic, pic_value err)
{
  pic_value stack, exc = pic_ref(pic, "picrin.base", "current-exception-handlers");

  stack = pic_call(pic, exc, 0);

  if (pic_nil_p(pic, stack)) {
    pic_panic(pic, "no exception handler");
  }

  return pic_dynamic_bind(pic, exc, pic_cdr(pic, stack), pic_lambda(pic, raise_continuable, 2, pic_car(pic, stack), err));
}

static pic_value
pic_error_with_exception_handler(pic_state *pic)
{
  pic_value handler, thunk;
  pic_value stack, exc = pic_ref(pic, "picrin.base", "current-exception-handlers");

  pic_get_args(pic, "ll", &handler, &thunk);

  stack = pic_call(pic, exc, 0);

  return pic_dynamic_bind(pic, exc, pic_cons(pic, handler, stack), thunk);
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

  pic_raise(pic, pic_make_error(pic, "", str, pic_make_list(pic, argc, argv)));
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

  return pic_obj_value(pic_error_ptr(pic, e)->msg);
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

  return pic_obj_value(pic_error_ptr(pic, e)->type);
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
