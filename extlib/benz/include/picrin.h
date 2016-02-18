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

#include "picrin/type.h"

typedef void *(*pic_allocf)(void *, void *, size_t);

pic_state *pic_open(pic_allocf, void *);
void pic_close(pic_state *);

int pic_get_args(pic_state *, const char *, ...);

void *pic_malloc(pic_state *, size_t);
void *pic_realloc(pic_state *, void *, size_t);
void *pic_calloc(pic_state *, size_t, size_t);
void pic_free(pic_state *, void *);

typedef pic_value (*pic_func_t)(pic_state *);

void *pic_alloca(pic_state *, size_t);
pic_value pic_gc_protect(pic_state *, pic_value);
size_t pic_gc_arena_preserve(pic_state *);
void pic_gc_arena_restore(pic_state *, size_t);
void pic_gc(pic_state *);

void pic_add_feature(pic_state *, const char *);

void pic_defun(pic_state *, const char *, pic_func_t);
void pic_defvar(pic_state *, const char *, pic_value, struct pic_proc *);

void pic_define(pic_state *, const char *, const char *, pic_value);
pic_value pic_ref(pic_state *, const char *, const char *);
void pic_set(pic_state *, const char *, const char *, pic_value);
pic_value pic_closure_ref(pic_state *, int);
void pic_closure_set(pic_state *, int, pic_value);
pic_value pic_funcall(pic_state *pic, const char *, const char *, int, ...);

void pic_make_library(pic_state *, const char *);
void pic_in_library(pic_state *, const char *);
bool pic_find_library(pic_state *, const char *);
const char *pic_current_library(pic_state *);
void pic_import(pic_state *, const char *);
void pic_export(pic_state *, pic_sym *);

PIC_NORETURN void pic_panic(pic_state *, const char *);
PIC_NORETURN void pic_errorf(pic_state *, const char *, ...);

struct pic_proc *pic_lambda(pic_state *, pic_func_t, int, ...);
struct pic_proc *pic_vlambda(pic_state *, pic_func_t, int, va_list);
pic_value pic_call(pic_state *, struct pic_proc *, int, ...);
pic_value pic_vcall(pic_state *, struct pic_proc *, int, va_list);
pic_value pic_apply(pic_state *, struct pic_proc *, int, pic_value *);
pic_value pic_applyk(pic_state *, struct pic_proc *, int, pic_value *);

int pic_int(pic_value);
double pic_float(pic_value);
char pic_char(pic_value);
bool pic_bool(pic_value);
/* const char *pic_str(pic_state *, pic_value); */
/* unsigned char *pic_blob(pic_state *, pic_value, int *len); */
/* void *pic_data(pic_state *, pic_value); */

pic_value pic_undef_value();
pic_value pic_int_value(int);
pic_value pic_float_value(double);
pic_value pic_char_value(char c);
pic_value pic_true_value();
pic_value pic_false_value();
pic_value pic_bool_value(bool);

#define pic_undef_p(v) (pic_vtype(v) == PIC_VTYPE_UNDEF)
#define pic_int_p(v) (pic_vtype(v) == PIC_VTYPE_INT)
#define pic_float_p(v) (pic_vtype(v) == PIC_VTYPE_FLOAT)
#define pic_char_p(v) (pic_vtype(v) == PIC_VTYPE_CHAR)
#define pic_true_p(v) (pic_vtype(v) == PIC_VTYPE_TRUE)
#define pic_false_p(v) (pic_vtype(v) == PIC_VTYPE_FALSE)
#define pic_str_p(v) (pic_type(v) == PIC_TT_STRING)
#define pic_blob_p(v) (pic_type(v) == PIC_TT_BLOB)
#define pic_proc_p(o) (pic_type(o) == PIC_TT_PROC)
#define pic_data_p(o) (pic_type(o) == PIC_TT_DATA)
#define pic_nil_p(v) (pic_vtype(v) == PIC_VTYPE_NIL)
#define pic_pair_p(v) (pic_type(v) == PIC_TT_PAIR)
#define pic_vec_p(v) (pic_type(v) == PIC_TT_VECTOR)
#define pic_dict_p(v) (pic_type(v) == PIC_TT_DICT)
#define pic_weak_p(v) (pic_type(v) == PIC_TT_WEAK)
#define pic_sym_p(v) (pic_type(v) == PIC_TT_SYMBOL)

enum pic_tt pic_type(pic_value);
const char *pic_type_repr(enum pic_tt);

bool pic_eq_p(pic_value, pic_value);
bool pic_eqv_p(pic_value, pic_value);
bool pic_equal_p(pic_state *, pic_value, pic_value);

/* list */
pic_value pic_nil_value();
pic_value pic_cons(pic_state *, pic_value, pic_value);
PIC_INLINE pic_value pic_car(pic_state *, pic_value);
PIC_INLINE pic_value pic_cdr(pic_state *, pic_value);
void pic_set_car(pic_state *, pic_value, pic_value);
void pic_set_cdr(pic_state *, pic_value, pic_value);
bool pic_list_p(pic_value);
pic_value pic_list(pic_state *, int n, ...);
pic_value pic_vlist(pic_state *, int n, va_list);
pic_value pic_list_ref(pic_state *, pic_value, int);
pic_value pic_list_tail(pic_state *, pic_value, int);
void pic_list_set(pic_state *, pic_value, int, pic_value);
int pic_length(pic_state *, pic_value);

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

/* ephemeron */
struct pic_weak *pic_make_weak(pic_state *);
pic_value pic_weak_ref(pic_state *, struct pic_weak *, void *);
void pic_weak_set(pic_state *, struct pic_weak *, void *, pic_value);
void pic_weak_del(pic_state *, struct pic_weak *, void *);
bool pic_weak_has(pic_state *, struct pic_weak *, void *);

/* symbol */
pic_sym *pic_intern(pic_state *, struct pic_string *);
#define pic_intern_str(pic,s,i) pic_intern(pic, pic_make_str(pic, (s), (i)))
#define pic_intern_cstr(pic,s) pic_intern(pic, pic_make_cstr(pic, (s)))
#define pic_intern_lit(pic,lit) pic_intern(pic, pic_make_lit(pic, lit))
const char *pic_symbol_name(pic_state *, pic_sym *);

/* string */
int pic_str_len(struct pic_string *);
char pic_str_ref(pic_state *, struct pic_string *, int);
struct pic_string *pic_str_cat(pic_state *, struct pic_string *, struct pic_string *);
struct pic_string *pic_str_sub(pic_state *, struct pic_string *, int, int);
int pic_str_cmp(pic_state *, struct pic_string *, struct pic_string *);
int pic_str_hash(pic_state *, struct pic_string *);

/* extra stuff */

#include "picrin/state.h"

#include "picrin/blob.h"
#include "picrin/cont.h"
#include "picrin/data.h"
#include "picrin/dict.h"
#include "picrin/error.h"
#include "picrin/macro.h"
#include "picrin/pair.h"
#include "picrin/port.h"
#include "picrin/proc.h"
#include "picrin/record.h"
#include "picrin/string.h"
#include "picrin/symbol.h"
#include "picrin/vector.h"
#include "picrin/weak.h"

void *pic_default_allocf(void *, void *, size_t);

struct pic_object *pic_obj_alloc(pic_state *, size_t, enum pic_tt);

#define pic_void(exec)                          \
  pic_void_(PIC_GENSYM(ai), exec)
#define pic_void_(ai,exec) do {                 \
    size_t ai = pic_gc_arena_preserve(pic);     \
    exec;                                       \
    pic_gc_arena_restore(pic, ai);              \
  } while (0)

pic_value pic_read(pic_state *, struct pic_port *);
pic_value pic_read_cstr(pic_state *, const char *);

void pic_load(pic_state *, struct pic_port *);
void pic_load_cstr(pic_state *, const char *);

pic_value pic_eval(pic_state *, pic_value, const char *);

struct pic_proc *pic_make_var(pic_state *, pic_value, struct pic_proc *);

#define pic_deflibrary(pic, lib) do {           \
    if (! pic_find_library(pic, lib)) {         \
      pic_make_library(pic, lib);               \
    }                                           \
    pic_in_library(pic, lib);                   \
  } while (0)

void pic_warnf(pic_state *, const char *, ...);
struct pic_string *pic_get_backtrace(pic_state *);
void pic_print_backtrace(pic_state *, xFILE *);

struct pic_port *pic_stdin(pic_state *);
struct pic_port *pic_stdout(pic_state *);
struct pic_port *pic_stderr(pic_state *);

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
