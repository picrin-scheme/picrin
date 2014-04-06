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

void
pic_abort(pic_state *pic, const char *msg)
{
  UNUSED(pic);

  fprintf(stderr, "abort: %s\n", msg);
  fflush(stderr);
  abort();
}

void
pic_warnf(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_value err_line;

  va_start(ap, fmt);
  err_line = pic_vformat(pic, fmt, ap);
  va_end(ap);

  fprintf(stderr, "warn: %s\n", pic_str_cstr(pic_str_ptr(pic_car(pic, err_line))));
}

void
pic_push_try(pic_state *pic)
{
  struct pic_jmpbuf *try_jmp;

  try_jmp = pic_alloc(pic, sizeof(struct pic_jmpbuf));

  try_jmp->prev_jmp = pic->jmp;
  pic->jmp = &try_jmp->here;

  try_jmp->prev = pic->try_jmps;
  pic->try_jmps = try_jmp;
}

void
pic_pop_try(pic_state *pic)
{
  struct pic_jmpbuf *prev;

  assert(pic->jmp == &pic->try_jmps->here);

  pic->jmp = pic->try_jmps->prev_jmp;

  prev = pic->try_jmps->prev;
  pic_free(pic, pic->try_jmps);
  pic->try_jmps = prev;
}

noreturn void
pic_throw(pic_state *pic, struct pic_error *e)
{
  pic->err = e;
  if (! pic->jmp) {
    puts(pic_errmsg(pic));
    abort();
  }
  longjmp(*pic->jmp, 1);
}

const char *
pic_errmsg(pic_state *pic)
{
  assert(pic->err != NULL);

  return pic_str_cstr(pic->err->msg);
}

void
pic_errorf(pic_state *pic, const char *fmt, ...)
{
  va_list ap;
  pic_value err_line;
  struct pic_error *e;

  va_start(ap, fmt);
  err_line = pic_vformat(pic, fmt, ap);
  va_end(ap);

  e = (struct pic_error *)pic_obj_alloc(pic, sizeof(struct pic_error), PIC_TT_ERROR);
  e->type = PIC_ERROR_OTHER;
  e->msg = pic_str_ptr(pic_car(pic, err_line));
  e->irrs = pic_cdr(pic, err_line);
  e->stack = pic_get_backtrace(pic);

  pic_throw(pic, e);
}

static pic_value
pic_error_with_exception_handler(pic_state *pic)
{
  struct pic_proc *handler, *thunk;
  pic_value v;

  pic_get_args(pic, "ll", &handler, &thunk);

  pic_try {
    v = pic_apply0(pic, thunk);
  }
  pic_catch {
    struct pic_error *e = pic->err;

    pic->err = NULL;
    v = pic_apply1(pic, handler, pic_obj_value(e));
    pic_errorf(pic, "error handler returned ~s, by error ~s", v, pic_obj_value(e));
  }
  return v;
}

noreturn static pic_value
pic_error_raise(pic_state *pic)
{
  pic_value v;
  struct pic_error *e;

  pic_get_args(pic, "o", &v);

  e = (struct pic_error *)pic_obj_alloc(pic, sizeof(struct pic_error), PIC_TT_ERROR);
  e->type = PIC_ERROR_RAISED;
  e->msg = pic_str_new_cstr(pic, "object is raised");
  e->irrs = pic_list1(pic, v);
  e->stack = pic_get_backtrace(pic);

  pic_throw(pic, e);
}

noreturn static pic_value
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
  e->stack = pic_get_backtrace(pic);

  pic_throw(pic, e);
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
  pic_defun(pic, "error", pic_error_error);
  pic_defun(pic, "error-object?", pic_error_error_object_p);
  pic_defun(pic, "error-object-message", pic_error_error_object_message);
  pic_defun(pic, "error-object-irritants", pic_error_error_object_irritants);
  pic_defun(pic, "read-error?", pic_error_read_error_p);
  pic_defun(pic, "file-error?", pic_error_file_error_p);
}
