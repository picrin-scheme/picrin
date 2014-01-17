/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/error.h"

void
pic_error(pic_state *pic, const char *msg)
{
  pic->errmsg = msg;
  if (! pic->jmp) {
    puts(msg);
    abort();
  }
  longjmp(*pic->jmp, 1);
}

void
pic_errorf(pic_state *pic, const char *msg, size_t n, ...)
{
  pic_error(pic, msg);
}

void
pic_abort(pic_state *pic, const char *msg)
{
  fprintf(stderr, "abort: %s\n", msg);
  fflush(stderr);
  abort();
}

void
pic_warn(pic_state *pic, const char *msg)
{
  fprintf(stderr, "warn: %s\n", msg);
}

void
pic_raise(pic_state *pic, pic_value obj)
{
  pic_value a;
  struct pic_proc *handler;

  if (pic->ridx == 0) {
    pic_abort(pic, "logic flaw: no exception handler remains");
  }

  handler = pic->rescue[--pic->ridx];
  pic_gc_protect(pic, pic_obj_value(handler));

  a = pic_apply_argv(pic, handler, 1, obj);
  /* when the handler returns */
  pic_errorf(pic, "handler returned", 2, pic_obj_value(handler), a);
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

    pic->rescue = (struct pic_proc **)pic_realloc(pic, pic->rescue, pic->rlen * 2);
    pic->rlen *= 2;
  }
  pic->rescue[pic->ridx++] = handler;

  v = pic_apply_argv(pic, thunk, 0);
  pic->ridx--;
  return v;
}

static pic_value
pic_error_raise(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  pic_raise(pic, v);

  /* the function never returns */
  return pic_undef_value();
}

static pic_value
pic_error_raise_continuable(pic_state *pic)
{
  pic_value v, a;
  struct pic_proc *handler;

  pic_get_args(pic, "o", &v);

  if (pic->ridx == 0) {
    pic_abort(pic, "logic flaw: no exception handler remains");
  }

  handler = pic->rescue[--pic->ridx];
  a = pic_apply_argv(pic, handler, 1, v);
  pic->rescue[pic->ridx++] = handler;

  return a;
}

static pic_value
pic_error_error(pic_state *pic)
{
  char *str;
  int len;
  size_t argc;
  pic_value *argv;
  struct pic_error *e;

  pic_get_args(pic, "s*", &str, &len, &argc, &argv);

  e = (struct pic_error *)pic_obj_alloc(pic, sizeof(struct pic_error), PIC_TT_ERROR);
  e->type = PIC_ERROR_OTHER;
  e->msg = strdup(str);
  e->irrs = pic_list_from_array(pic, argc, argv);

  pic_raise(pic, pic_obj_value(e));

  /* never returns */
  return pic_undef_value();
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

  return pic_obj_value(pic_str_new_cstr(pic, e->msg));
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
