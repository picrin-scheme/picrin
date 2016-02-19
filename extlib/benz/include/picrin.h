/**
 * Copyright (c) 2013-2016 Picrin developers.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef PICRIN_H
#define PICRIN_H

#if defined(__cplusplus)
extern "C" {
#endif

#include <stddef.h>
#include <limits.h>
#include <stdarg.h>

#include "picrin/setup.h"

typedef struct pic_state pic_state;

#if PIC_NAN_BOXING
# include <stdint.h>
typedef uint64_t pic_value;
#else
typedef struct {
  unsigned char type;
  union {
    void *data;
    double f;
    int i;
    char c;
  } u;
} pic_value;
#endif

struct pic_object;
struct pic_symbol;
struct pic_pair;
struct pic_string;
struct pic_vector;
struct pic_blob;
struct pic_proc;
struct pic_port;
struct pic_error;
struct pic_env;
struct pic_data;

typedef struct pic_symbol pic_sym;
typedef struct pic_id pic_id;
typedef struct pic_pair pic_pair;
typedef struct pic_vector pic_vec;

typedef void *(*pic_allocf)(void *userdata, void *ptr, size_t n);

pic_state *pic_open(pic_allocf f, void *userdata);
void pic_close(pic_state *);

int pic_get_args(pic_state *, const char *fmt, ...);

void *pic_malloc(pic_state *, size_t);
void *pic_realloc(pic_state *, void *, size_t);
void *pic_calloc(pic_state *, size_t, size_t);
void pic_free(pic_state *, void *);

typedef pic_value (*pic_func_t)(pic_state *);

void *pic_alloca(pic_state *, size_t);
size_t pic_enter(pic_state *);
void pic_leave(pic_state *, size_t);
pic_value pic_protect(pic_state *, pic_value);
void pic_gc(pic_state *);

void pic_add_feature(pic_state *, const char *feature);

void pic_defun(pic_state *, const char *name, pic_func_t f);
void pic_defvar(pic_state *, const char *name, pic_value v, struct pic_proc *conv);

void pic_define(pic_state *, const char *lib, const char *name, pic_value v);
pic_value pic_ref(pic_state *, const char *lib, const char *name);
void pic_set(pic_state *, const char *lib, const char *name, pic_value v);
pic_value pic_closure_ref(pic_state *, int i);
void pic_closure_set(pic_state *, int i, pic_value v);
pic_value pic_funcall(pic_state *, const char *lib, const char *name, int n, ...);

void pic_make_library(pic_state *, const char *lib);
void pic_in_library(pic_state *, const char *lib);
bool pic_find_library(pic_state *, const char *lib);
const char *pic_current_library(pic_state *);
void pic_import(pic_state *, const char *lib);
void pic_export(pic_state *, pic_sym *sym);

PIC_NORETURN void pic_panic(pic_state *, const char *msg);
PIC_NORETURN void pic_errorf(pic_state *, const char *fmt, ...);
PIC_NORETURN void pic_error(pic_state *, const char *type, const char *msg, pic_value irrs);
PIC_NORETURN void pic_raise(pic_state *, pic_value v);

struct pic_proc *pic_lambda(pic_state *, pic_func_t f, int n, ...);
struct pic_proc *pic_vlambda(pic_state *, pic_func_t f, int n, va_list);
pic_value pic_call(pic_state *, struct pic_proc *proc, int, ...);
pic_value pic_vcall(pic_state *, struct pic_proc *proc, int, va_list);
pic_value pic_apply(pic_state *, struct pic_proc *proc, int n, pic_value *argv);
pic_value pic_applyk(pic_state *, struct pic_proc *proc, int n, pic_value *argv);

PIC_INLINE int pic_int(pic_state *, pic_value);
PIC_INLINE double pic_float(pic_state *, pic_value);
PIC_INLINE char pic_char(pic_state *, pic_value);
#define pic_bool(pic,v) (! pic_false_p(pic, v))
const char *pic_str(pic_state *, struct pic_string *);
unsigned char *pic_blob(pic_state *, struct pic_blob *, int *len);
void *pic_data(pic_state *, pic_value);

typedef struct {
  const char *type_name;
  void (*dtor)(pic_state *, void *);
  void (*mark)(pic_state *, void *, void (*)(pic_state *, pic_value));
} pic_data_type;

PIC_INLINE pic_value pic_undef_value(pic_state *);
PIC_INLINE pic_value pic_int_value(pic_state *, int);
PIC_INLINE pic_value pic_float_value(pic_state *, double);
PIC_INLINE pic_value pic_char_value(pic_state *, char);
PIC_INLINE pic_value pic_true_value(pic_state *);
PIC_INLINE pic_value pic_false_value(pic_state *);
PIC_INLINE pic_value pic_bool_value(pic_state *, bool);
PIC_INLINE pic_value pic_eof_object(pic_state *);
struct pic_string *pic_str_value(pic_state *, const char *str, int len);
#define pic_cstr_value(pic, cstr) pic_str_value(pic, (cstr), strlen(cstr))
#define pic_lit_value(pic, lit) pic_str_value(pic, "" lit, -((int)sizeof lit - 1))
struct pic_string *pic_strf_value(pic_state *, const char *fmt, ...);
struct pic_string *pic_vstrf_value(pic_state *, const char *fmt, va_list ap);
struct pic_blob *pic_blob_value(pic_state *, const unsigned char *buf, int len);
struct pic_data *pic_data_value(pic_state *, void *ptr, const pic_data_type *type);

enum {
  PIC_TYPE_INVALID = 1,
  PIC_TYPE_FLOAT   = 2,
  PIC_TYPE_INT     = 3,
  PIC_TYPE_CHAR    = 4,
  PIC_TYPE_EOF     = 5,
  PIC_TYPE_UNDEF   = 6,
  PIC_TYPE_TRUE    = 8,
  PIC_TYPE_NIL     = 7,
  PIC_TYPE_FALSE   = 9,
  PIC_IVAL_END     = 10,
/* -------------------- */
  PIC_TYPE_STRING  = 16,
  PIC_TYPE_VECTOR  = 17,
  PIC_TYPE_BLOB    = 18,
  PIC_TYPE_PROC    = 19,
  PIC_TYPE_PORT    = 20,
  PIC_TYPE_ERROR   = 21,
  PIC_TYPE_ID      = 22,
  PIC_TYPE_ENV     = 23,
  PIC_TYPE_DATA    = 24,
  PIC_TYPE_DICT    = 25,
  PIC_TYPE_WEAK    = 26,
  PIC_TYPE_RECORD  = 27,
  PIC_TYPE_SYMBOL  = 28,
  PIC_TYPE_PAIR    = 29,
  PIC_TYPE_CXT     = 30,
  PIC_TYPE_CP      = 31
};

#define pic_undef_p(pic,v) (pic_type(pic,v) == PIC_TYPE_UNDEF)
#define pic_int_p(pic,v) (pic_type(pic,v) == PIC_TYPE_INT)
#define pic_float_p(pic,v) (pic_type(pic,v) == PIC_TYPE_FLOAT)
#define pic_char_p(pic,v) (pic_type(pic,v) == PIC_TYPE_CHAR)
#define pic_eof_p(pic, v) (pic_vtype(pic, v) == PIC_TYPE_EOF)
#define pic_true_p(pic,v) (pic_type(pic,v) == PIC_TYPE_TRUE)
#define pic_false_p(pic,v) (pic_type(pic,v) == PIC_TYPE_FALSE)
#define pic_str_p(pic,v) (pic_type(pic,v) == PIC_TYPE_STRING)
#define pic_blob_p(pic,v) (pic_type(pic,v) == PIC_TYPE_BLOB)
#define pic_proc_p(pic,v) (pic_type(pic,v) == PIC_TYPE_PROC)
#define pic_data_p(pic,v) (pic_type(pic,v) == PIC_TYPE_DATA)
#define pic_nil_p(pic,v) (pic_type(pic,v) == PIC_TYPE_NIL)
#define pic_pair_p(pic,v) (pic_type(pic,v) == PIC_TYPE_PAIR)
#define pic_vec_p(pic,v) (pic_type(pic,v) == PIC_TYPE_VECTOR)
#define pic_dict_p(pic,v) (pic_type(pic,v) == PIC_TYPE_DICT)
#define pic_weak_p(pic,v) (pic_type(pic,v) == PIC_TYPE_WEAK)
#define pic_port_p(pic, v) (pic_type(pic, v) == PIC_TYPE_PORT)
#define pic_sym_p(pic,v) (pic_type(pic,v) == PIC_TYPE_SYMBOL)

int pic_type(pic_state *, pic_value);
const char *pic_typename(pic_state *, int);

bool pic_eq_p(pic_state *, pic_value, pic_value);
bool pic_eqv_p(pic_state *, pic_value, pic_value);
bool pic_equal_p(pic_state *, pic_value, pic_value);

/* pair */
pic_value pic_cons(pic_state *, pic_value car, pic_value cdr);
pic_value pic_car(pic_state *, pic_value pair);
pic_value pic_cdr(pic_state *, pic_value pair);
void pic_set_car(pic_state *, pic_value pair, pic_value car);
void pic_set_cdr(pic_state *, pic_value pair, pic_value cdr);
pic_value pic_caar(pic_state *, pic_value);
pic_value pic_cadr(pic_state *, pic_value);
pic_value pic_cdar(pic_state *, pic_value);
pic_value pic_cddr(pic_state *, pic_value);

/* list */
PIC_INLINE pic_value pic_nil_value(pic_state *);
bool pic_list_p(pic_state *, pic_value);
pic_value pic_make_list(pic_state *, int n, pic_value *argv);
pic_value pic_list(pic_state *, int n, ...);
pic_value pic_vlist(pic_state *, int n, va_list);
pic_value pic_list_ref(pic_state *, pic_value list, int i);
void pic_list_set(pic_state *, pic_value list, int i, pic_value v);
pic_value pic_list_tail(pic_state *, pic_value list, int i);
int pic_length(pic_state *, pic_value list);
pic_value pic_reverse(pic_state *, pic_value list);
pic_value pic_append(pic_state *, pic_value xs, pic_value ys);

/* vector */
pic_vec *pic_make_vec(pic_state *, int);
pic_value pic_vec_ref(pic_state *, pic_vec *, int);
void pic_vec_set(pic_state *, pic_vec *, int, pic_value);
int pic_vec_len(pic_state *, pic_vec *);

/* dictionary */
struct pic_dict *pic_make_dict(pic_state *);
pic_value pic_dict_ref(pic_state *, struct pic_dict *, pic_sym *);
void pic_dict_set(pic_state *, struct pic_dict *, pic_sym *, pic_value);
void pic_dict_del(pic_state *, struct pic_dict *, pic_sym *);
bool pic_dict_has(pic_state *, struct pic_dict *, pic_sym *);
int pic_dict_size(pic_state *, struct pic_dict *);
bool pic_dict_next(pic_state *, struct pic_dict *, int *iter, pic_sym **key, pic_value *val);

/* ephemeron */
struct pic_weak *pic_make_weak(pic_state *);
pic_value pic_weak_ref(pic_state *, struct pic_weak *, void *);
void pic_weak_set(pic_state *, struct pic_weak *, void *, pic_value);
void pic_weak_del(pic_state *, struct pic_weak *, void *);
bool pic_weak_has(pic_state *, struct pic_weak *, void *);

/* symbol */
pic_sym *pic_intern(pic_state *, struct pic_string *);
#define pic_intern_str(pic,s,i) pic_intern(pic, pic_str_value(pic, (s), (i)))
#define pic_intern_cstr(pic,s) pic_intern(pic, pic_cstr_value(pic, (s)))
#define pic_intern_lit(pic,lit) pic_intern(pic, pic_lit_value(pic, lit))
struct pic_string *pic_sym_name(pic_state *, pic_sym *);

/* string */
int pic_str_len(pic_state *, struct pic_string *);
char pic_str_ref(pic_state *, struct pic_string *, int);
struct pic_string *pic_str_cat(pic_state *, struct pic_string *, struct pic_string *);
struct pic_string *pic_str_sub(pic_state *, struct pic_string *, int, int);
int pic_str_cmp(pic_state *, struct pic_string *, struct pic_string *);
int pic_str_hash(pic_state *, struct pic_string *);


/* extra stuff */


#include "picrin/type.h"
#include "picrin/state.h"

#include "picrin/cont.h"

void *pic_default_allocf(void *, void *, size_t);

#define pic_assert_type(pic, v, type)                           \
  if (! pic_##type##_p(pic, v)) {                               \
    pic_errorf(pic, "expected " #type ", but got ~s", v);       \
  }

struct pic_port *pic_make_port(pic_state *, xFILE *file);
void pic_close_port(pic_state *, struct pic_port *port);

#define pic_void(exec)                          \
  pic_void_(PIC_GENSYM(ai), exec)
#define pic_void_(ai,exec) do {                 \
    size_t ai = pic_enter(pic);     \
    exec;                                       \
    pic_leave(pic, ai);              \
  } while (0)

pic_sym *pic_add_identifier(pic_state *, pic_id *, struct pic_env *);
pic_sym *pic_put_identifier(pic_state *, pic_id *, pic_sym *, struct pic_env *);
pic_sym *pic_find_identifier(pic_state *, pic_id *, struct pic_env *);

pic_value pic_read(pic_state *, struct pic_port *);
pic_value pic_read_cstr(pic_state *, const char *);

pic_value pic_expand(pic_state *, pic_value, struct pic_env *);

pic_value pic_eval(pic_state *, pic_value, const char *);

void pic_load(pic_state *, struct pic_port *);
void pic_load_cstr(pic_state *, const char *);

struct pic_proc *pic_make_var(pic_state *, pic_value, struct pic_proc *);

bool pic_data_type_p(pic_state *, pic_value, const pic_data_type *);

#define pic_deflibrary(pic, lib) do {           \
    if (! pic_find_library(pic, lib)) {         \
      pic_make_library(pic, lib);               \
    }                                           \
    pic_in_library(pic, lib);                   \
  } while (0)

/* do not return from try block! */

#define pic_try                                 \
  pic_try_(PIC_GENSYM(cont), PIC_GENSYM(handler))
#define pic_catch                               \
  pic_catch_(PIC_GENSYM(label))
#define pic_try_(cont, handler)                                         \
  do {                                                                  \
    extern void pic_push_handler(pic_state *, struct pic_proc *);       \
    extern struct pic_proc *pic_pop_handler(pic_state *);               \
    extern pic_value pic_native_exception_handler(pic_state *);         \
    struct pic_cont cont;                                               \
    pic_save_point(pic, &cont);                                         \
    if (PIC_SETJMP(pic, cont.jmp) == 0) {                               \
      struct pic_proc *handler;                                         \
      handler = pic_lambda(pic, pic_native_exception_handler, 1, pic_obj_value(pic_make_cont(pic, &cont))); \
      do {                                                              \
        pic_push_handler(pic, handler);
#define pic_catch_(label)                                 \
        pic_pop_handler(pic);                             \
      } while (0);                                        \
      pic->cc = pic->cc->prev;                            \
    } else {                                              \
      goto label;                                         \
    }                                                     \
  } while (0);                                            \
  if (0)                                                  \
  label:

#define pic_for_each(var, list, it)                                     \
  for (it = (list); ! pic_nil_p(pic, it); it = pic_cdr(pic, it))        \
    if ((var = pic_car(pic, it)), true)

#define pic_push(pic, item, place) (place = pic_cons(pic, item, place))
#define pic_pop(pic, place) (place = pic_cdr(pic, place))

void pic_warnf(pic_state *, const char *, ...);
struct pic_string *pic_get_backtrace(pic_state *);
void pic_print_backtrace(pic_state *, xFILE *);

#define pic_stdin(pic) pic_port_ptr(pic_funcall(pic, "picrin.base", "current-input-port", 0))
#define pic_stdout(pic) pic_port_ptr(pic_funcall(pic, "picrin.base", "current-output-port", 0))
#define pic_stderr(pic) pic_port_ptr(pic_funcall(pic, "picrin.base", "current-error-port", 0))

pic_value pic_write(pic_state *, pic_value); /* returns given obj */
pic_value pic_fwrite(pic_state *, pic_value, xFILE *);
void pic_printf(pic_state *, const char *, ...);
void pic_fprintf(pic_state *, struct pic_port *, const char *, ...);
pic_value pic_display(pic_state *, pic_value);
pic_value pic_fdisplay(pic_state *, pic_value, xFILE *);

struct pic_env *pic_library_environment(pic_state *, const char *);

#if DEBUG
# define pic_debug(pic,obj) pic_fwrite(pic,obj,xstderr)
# define pic_fdebug(pic,obj,file) pic_fwrite(pic,obj,file)
#endif

#if defined(__cplusplus)
}
#endif

#endif
