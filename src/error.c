/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/error.h"

const char *
pic_errmsg(pic_state *pic)
{
  assert(pic->err != NULL);

  return pic_str_cstr(pic->err->msg);
}

NORETURN static void
raise(pic_state *pic, struct pic_error *e)
{
  pic->err = e;
  if (! pic->jmp) {
    puts(pic_errmsg(pic));
    abort();
  }
  longjmp(*pic->jmp, 1);
}

NORETURN static void
error(pic_state *pic, pic_str *msg, pic_value irrs)
{
  struct pic_error *e;

  e = (struct pic_error *)pic_obj_alloc(pic, sizeof(struct pic_error), PIC_TT_ERROR);
  e->type = PIC_ERROR_OTHER;
  e->msg = msg;
  e->irrs = irrs;

  raise(pic, e);
}

void
pic_error(pic_state *pic, const char *msg)
{
  pic_errorf(pic, msg);
}

void
pic_errorf(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_value err_line;

  va_start(ap, fmt);
  err_line = pic_vformat(pic, fmt, ap);
  va_end(ap);

  error(pic, pic_str_ptr(pic_car(pic, err_line)), pic_cdr(pic, err_line));
}

void
pic_abort(pic_state *pic, const char *msg)
{
  UNUSED(pic);

  fprintf(stderr, "abort: %s\n", msg);
  fflush(stderr);
  abort();
}

void
pic_warn(pic_state *pic, const char *msg)
{
  UNUSED(pic);

  fprintf(stderr, "warn: %s\n", msg);
}

void
pic_raise(pic_state *pic, struct pic_error *e)
{
  pic_value a;
  struct pic_proc *handler;

  if (pic->ridx == 0) {
    raise(pic, e);
  }

  handler = pic->rescue[--pic->ridx];
  pic_gc_protect(pic, pic_obj_value(handler));

  a = pic_apply_argv(pic, handler, 1, pic_obj_value(e));
  /* when the handler returns */
  pic_errorf(pic, "handler returned", 2, pic_obj_value(handler), a);
}

pic_value
pic_raise_continuable(pic_state *pic, pic_value obj)
{
  struct pic_proc *handler;

  if (pic->ridx == 0) {
    pic_abort(pic, "logic flaw: no exception handler remains");
  }

  handler = pic->rescue[--pic->ridx];
  obj = pic_apply_argv(pic, handler, 1, obj);
  pic->rescue[pic->ridx++] = handler;

  return obj;
}

static pic_value
pic_error_with_exception_handler(pic_state *pic)
{
  struct pic_proc *handler, *thunk;
  pic_value v;

  pic_get_args(pic, "ll", &handler, &thunk);

  if (pic->ridx >= pic->rlen) {

#if DEBUG
    puts("rescue realloced");
#endif

    pic->rlen *= 2;
    pic->rescue = (struct pic_proc **)pic_realloc(pic, pic->rescue, sizeof(struct pic_proc *) * pic->rlen);
  }
  pic->rescue[pic->ridx++] = handler;

  v = pic_apply_argv(pic, thunk, 0);
  pic->ridx--;
  return v;
}

NORETURN static pic_value
pic_error_raise(pic_state *pic)
{
  pic_value v;
  struct pic_error *e;

  pic_get_args(pic, "o", &v);

  e = (struct pic_error *)pic_obj_alloc(pic, sizeof(struct pic_error), PIC_TT_ERROR);
  e->type = PIC_ERROR_RAISED;
  e->msg = pic_str_new_cstr(pic, "raised");
  e->irrs = pic_list(pic, 1, v);

  pic_raise(pic, e);
}

static pic_value
pic_error_raise_continuable(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_raise_continuable(pic, obj);
}

NORETURN static pic_value
pic_error_error(pic_state *pic)
{
  pic_str *str;
  size_t argc;
  pic_value *argv;
  struct pic_error *e;

  pic_get_args(pic, "s*", &str, &argc, &argv);

  e = (struct pic_error *)pic_obj_alloc(pic, sizeof(struct pic_error), PIC_TT_ERROR);
  e->type = PIC_ERROR_OTHER;
  e->msg = str;
  e->irrs = pic_list_by_array(pic, argc, argv);

  pic_raise(pic, e);
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
pic_error_read_error_p(pic_state *pic)
{
  pic_value v;
  struct pic_error *e;

  pic_get_args(pic, "o", &v);

  if (! pic_error_p(v)) {
    return pic_false_value();
  }

  e = pic_error_ptr(v);
  return pic_bool_value(e->type == PIC_ERROR_READ);
}

static pic_value
pic_error_file_error_p(pic_state *pic)
{
  pic_value v;
  struct pic_error *e;

  pic_get_args(pic, "o", &v);

  if (! pic_error_p(v)) {
    return pic_false_value();
  }

  e = pic_error_ptr(v);
  return pic_bool_value(e->type == PIC_ERROR_FILE);
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
  pic_defun(pic, "read-error?", pic_error_read_error_p);
  pic_defun(pic, "file-error?", pic_error_file_error_p);
}
