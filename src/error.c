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
pic_push_try(pic_state *pic, struct pic_proc *handler)
{
  struct pic_jmpbuf *try_jmp;

  if (pic->try_jmp_idx >= pic->try_jmp_size) {
    pic->try_jmp_size *= 2;
    pic->try_jmps = pic_realloc(pic, pic->try_jmps, sizeof(struct pic_jmpbuf) * pic->try_jmp_size);
  }

  try_jmp = pic->try_jmps + pic->try_jmp_idx++;

  try_jmp->handler = handler;

  try_jmp->ci_offset = pic->ci - pic->cibase;
  try_jmp->sp_offset = pic->sp - pic->stbase;
  try_jmp->ip = pic->ip;

  try_jmp->prev_jmp = pic->jmp;
  pic->jmp = &try_jmp->here;
}

void
pic_pop_try(pic_state *pic)
{
  struct pic_jmpbuf *try_jmp;

  try_jmp = pic->try_jmps + --pic->try_jmp_idx;

  assert(pic->jmp == &try_jmp->here);

  pic->ci = try_jmp->ci_offset + pic->cibase;
  pic->sp = try_jmp->sp_offset + pic->stbase;
  pic->ip = try_jmp->ip;

  pic->jmp = try_jmp->prev_jmp;
}

static struct pic_error *
error_new(pic_state *pic, short type, pic_str *msg, pic_value irrs)
{
  struct pic_error *e;
  pic_str *stack;

  stack = pic_get_backtrace(pic);

  e = (struct pic_error *)pic_obj_alloc(pic, sizeof(struct pic_error), PIC_TT_ERROR);
  e->type = type;
  e->msg = msg;
  e->irrs = irrs;
  e->stack = stack;

  return e;
}

noreturn void
pic_throw_error(pic_state *pic, struct pic_error *e)
{
  void pic_vm_tear_off(pic_state *);

  pic_vm_tear_off(pic);         /* tear off */

  pic->err = e;
  if (! pic->jmp) {
    puts(pic_errmsg(pic));
    abort();
  }

  longjmp(*pic->jmp, 1);
}

noreturn void
pic_throw(pic_state *pic, short type, const char *msg, pic_value irrs)
{
  struct pic_error *e;

  e = error_new(pic, type, pic_str_new_cstr(pic, msg), irrs);

  pic_throw_error(pic, e);
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
  pic_value err_line, irrs;
  const char *msg;

  va_start(ap, fmt);
  err_line = pic_vformat(pic, fmt, ap);
  va_end(ap);

  msg = pic_str_cstr(pic_str_ptr(pic_car(pic, err_line)));
  irrs = pic_cdr(pic, err_line);

  pic_throw(pic, PIC_ERROR_OTHER, msg, irrs);
}

static pic_value
pic_error_with_exception_handler(pic_state *pic)
{
  struct pic_proc *handler, *thunk;
  pic_value v;

  pic_get_args(pic, "ll", &handler, &thunk);

  pic_try_with_handler(handler) {
    v = pic_apply0(pic, thunk);
  }
  pic_catch {
    struct pic_error *e = pic->err;

    pic->err = NULL;

    if (e->type == PIC_ERROR_RAISED) {
      v = pic_list_ref(pic, e->irrs, 0);
    } else {
      v = pic_obj_value(e);
    }
    v = pic_apply1(pic, handler, v);
    pic_errorf(pic, "error handler returned ~s, by error ~s", v, pic_obj_value(e));
  }
  return v;
}

noreturn static pic_value
pic_error_raise(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  pic_throw(pic, PIC_ERROR_RAISED, "object is raised", pic_list1(pic, v));
}

static pic_value
pic_error_raise_continuable(pic_state *pic)
{
  pic_value v;
  size_t i;

  pic_get_args(pic, "o", &v);

  if (pic->try_jmps->handler == NULL) {
    pic_errorf(pic, "uncontinuable exception handler is on top");
  }
  if ((i = pic->try_jmp_idx) == 0) {
    pic_errorf(pic, "no exception handler registered");
  }
  else {
    pic->try_jmp_idx--;
    v = pic_apply1(pic, pic->try_jmps->handler, v);
    ++pic->try_jmp_idx;
  }
  return v;
}

noreturn static pic_value
pic_error_error(pic_state *pic)
{
  const char *str;
  size_t argc;
  pic_value *argv;

  pic_get_args(pic, "z*", &str, &argc, &argv);

  pic_throw(pic, PIC_ERROR_OTHER, str, pic_list_by_array(pic, argc, argv));
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
