/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/port.h"
#include "picrin/error.h"

const char *
pic_errmsg(pic_state *pic)
{
  assert(pic->err != NULL);

  return pic->err->msg->str;
}

static pic_value
pic_vfformat(pic_state *pic, XFILE *file, const char *fmt, va_list ap)
{
  char c;
  pic_value irrs = pic_nil_value();

  while ((c = *fmt++)) {
    switch (c) {
    default:
      xfputc(c, file);
      break;
    case '%':
      c = *fmt++;
      if (! c)
        goto exit;
      switch (c) {
      default:
        xfputc(c, file);
        break;
      case '%':
        xfputc('%', file);
        break;
      case 'c':
        xfprintf(file, "%c", va_arg(ap, int));
        break;
      case 's':
        xfprintf(file, "%s", va_arg(ap, const char *));
        break;
      case 'd':
        xfprintf(file, "%d", va_arg(ap, int));
        break;
      case 'p':
        xfprintf(file, "%p", va_arg(ap, void *));
        break;
      case 'f':
        xfprintf(file, "%f", va_arg(ap, double));
        break;
      }
      break;
    case '~':
      c = *fmt++;
      if (! c)
        goto exit;
      switch (c) {
      default:
        xfputc(c, file);
        break;
      case '~':
        xfputc('~', file);
        break;
      case '%':
        xfputc('\n', file);
        break;
      case 'S':
        irrs = pic_cons(pic, pic_fdebug(pic, va_arg(ap, pic_value), file), irrs);
        break;
      }
      break;
    }
  }
 exit:

  return pic_reverse(pic, irrs);
}

static pic_value
pic_vformat(pic_state *pic, const char *fmt, va_list ap)
{
  struct pic_port *port;
  pic_value irrs;

  port = pic_open_output_string(pic);

  irrs = pic_vfformat(pic, port->file, fmt, ap);
  irrs = pic_cons(pic, pic_obj_value(pic_get_output_string(pic, port)), irrs);

  pic_close_port(pic, port);
  return irrs;
}

NORETURN static void
error(pic_state *pic, struct pic_string *msg, pic_value irrs)
{
  struct pic_error *e;

  e = (struct pic_error *)pic_obj_alloc(pic, sizeof(struct pic_error), PIC_TT_ERROR);
  e->type = PIC_ERROR_OTHER;
  e->msg = msg;
  e->irrs = irrs;

  pic->err = e;
  if (! pic->jmp) {
    puts(pic_errmsg(pic));
    abort();
  }
  longjmp(*pic->jmp, 1);
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

  pic_get_args(pic, "o", &v);

  pic_raise(pic, v);
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

NORETURN static pic_value
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
  e->msg = pic_str_new_cstr(pic, str);
  e->irrs = pic_list_by_array(pic, argc, argv);

  pic_raise(pic, pic_obj_value(e));
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
