/**
 * Copyright (c) 2013-2017 Picrin developers.
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

#include <picrin/setup.h>

typedef struct pic_state pic_state;

typedef struct value {
#if PIC_NAN_BOXING
  uint64_t v;
#else
  union {
    void *data;
    double f;
    int i;
    char c;
  } u;
  unsigned char type;
#endif
} pic_value;


/*
 * state manipulation
 */

typedef void *(*pic_allocf)(void *userdata, void *ptr, size_t n);
typedef void (*pic_panicf)(pic_state *, const char *msg, int n, pic_value *args);
pic_state *pic_open(pic_allocf allocf, void *userdata, pic_panicf panicf);
void pic_close(pic_state *);


/*
 * memory management
 */

void *pic_malloc(pic_state *, size_t);
void *pic_realloc(pic_state *, void *, size_t);
void *pic_calloc(pic_state *, size_t, size_t);
void pic_free(pic_state *, void *);
/* for managed area: */
size_t pic_enter(pic_state *);
void pic_leave(pic_state *, size_t);
pic_value pic_protect(pic_state *, pic_value);
void *pic_alloca(pic_state *, size_t);
void pic_gc(pic_state *);


/*
 * comparison
 */

bool pic_eq_p(pic_state *, pic_value, pic_value);
bool pic_eqv_p(pic_state *, pic_value, pic_value);
bool pic_equal_p(pic_state *, pic_value, pic_value);


/*
 * number, boolean, character, and userdata
 */

typedef struct {
  const char *type_name;
  void (*dtor)(pic_state *, void *);
} pic_data_type;

bool pic_int_p(pic_state *, pic_value);
bool pic_float_p(pic_state *, pic_value);
bool pic_char_p(pic_state *, pic_value);
bool pic_true_p(pic_state *, pic_value);
bool pic_false_p(pic_state *, pic_value);
bool pic_bool_p(pic_state *, pic_value);
bool pic_data_p(pic_state *, pic_value, const pic_data_type *);
pic_value pic_undef_value(pic_state *);
pic_value pic_int_value(pic_state *, int);
pic_value pic_float_value(pic_state *, double);
pic_value pic_char_value(pic_state *, char);
pic_value pic_bool_value(pic_state *, bool);
pic_value pic_true_value(pic_state *);
pic_value pic_false_value(pic_state *);
pic_value pic_data_value(pic_state *, void *ptr, const pic_data_type *type);
int pic_int(pic_state *, pic_value i);
double pic_float(pic_state *, pic_value f);
char pic_char(pic_state *, pic_value c);
#define pic_bool(pic,b) (! pic_false_p(pic, (b)))
void *pic_data(pic_state *, pic_value data);


/*
 * bytevector
 */

bool pic_blob_p(pic_state *, pic_value);
pic_value pic_blob_value(pic_state *, const unsigned char *buf, int len);
unsigned char *pic_blob(pic_state *, pic_value blob, int *len);
pic_value pic_serialize(pic_state *pic, pic_value obj);
pic_value pic_deserialize(pic_state *pic, pic_value blob);


/*
 * string
 */

bool pic_str_p(pic_state *, pic_value);
pic_value pic_str_value(pic_state *, const char *str, int len);
pic_value pic_cstr_value(pic_state *, const char *str);
#define pic_lit_value(pic, lit) pic_str_value(pic, "" lit, -((int)sizeof lit - 1))
pic_value pic_strf_value(pic_state *, const char *fmt, ...);
pic_value pic_vstrf_value(pic_state *, const char *fmt, va_list ap);
const char *pic_str(pic_state *, pic_value str, int *len);
const char *pic_cstr(pic_state *, pic_value str, int *len);
int pic_str_len(pic_state *, pic_value str);
pic_value pic_str_cat(pic_state *, pic_value str1, pic_value str2);
pic_value pic_str_sub(pic_state *, pic_value str, int i, int j);


/*
 * symbol
 */

bool pic_sym_p(pic_state *, pic_value);
pic_value pic_intern(pic_state *, pic_value str);
#define pic_intern_str(pic,s,i) pic_intern(pic, pic_str_value(pic, (s), (i)))
#define pic_intern_cstr(pic,s) pic_intern(pic, pic_cstr_value(pic, (s)))
#define pic_intern_lit(pic,lit) pic_intern(pic, pic_lit_value(pic, lit))
pic_value pic_sym_name(pic_state *, pic_value sym);


/*
 * pair
 */

bool pic_pair_p(pic_state *, pic_value);
pic_value pic_cons(pic_state *, pic_value car, pic_value cdr);
pic_value pic_car(pic_state *, pic_value pair);
pic_value pic_cdr(pic_state *, pic_value pair);
void pic_set_car(pic_state *, pic_value pair, pic_value car);
void pic_set_cdr(pic_state *, pic_value pair, pic_value cdr);
pic_value pic_caar(pic_state *, pic_value);
pic_value pic_cadr(pic_state *, pic_value);
pic_value pic_cdar(pic_state *, pic_value);
pic_value pic_cddr(pic_state *, pic_value);


/*
 * list
 */

bool pic_nil_p(pic_state *, pic_value);
bool pic_list_p(pic_state *, pic_value);
pic_value pic_nil_value(pic_state *);
pic_value pic_make_list(pic_state *, int n, pic_value *argv);
pic_value pic_list(pic_state *, int n, ...);
pic_value pic_vlist(pic_state *, int n, va_list);
pic_value pic_list_ref(pic_state *, pic_value list, int i);
void pic_list_set(pic_state *, pic_value list, int i, pic_value v);
pic_value pic_list_tail(pic_state *, pic_value list, int i);
int pic_length(pic_state *, pic_value list);
pic_value pic_reverse(pic_state *, pic_value list);
pic_value pic_append(pic_state *, pic_value xs, pic_value ys);


/*
 * vector
 */

bool pic_vec_p(pic_state *, pic_value);
pic_value pic_make_vec(pic_state *, int n, pic_value *argv);
pic_value pic_vec_ref(pic_state *, pic_value vec, int i);
void pic_vec_set(pic_state *, pic_value vec, int i, pic_value v);
int pic_vec_len(pic_state *, pic_value vec);


/*
 * dictionary
 */

bool pic_dict_p(pic_state *, pic_value);
pic_value pic_make_dict(pic_state *);
pic_value pic_dict_ref(pic_state *, pic_value dict, pic_value key);
void pic_dict_set(pic_state *, pic_value dict, pic_value key, pic_value);
void pic_dict_del(pic_state *, pic_value dict, pic_value key);
bool pic_dict_has(pic_state *, pic_value dict, pic_value key);
int pic_dict_size(pic_state *, pic_value dict);
bool pic_dict_next(pic_state *, pic_value dict, int *iter, pic_value *key, pic_value *val);


/*
 * attribute
 */

pic_value pic_make_attr(pic_state *);
pic_value pic_attr_ref(pic_state *, pic_value attr, pic_value key);
void pic_attr_set(pic_state *, pic_value attr, pic_value key, pic_value val);
void pic_attr_del(pic_state *, pic_value attr, pic_value key);
bool pic_attr_has(pic_state *, pic_value attr, pic_value key);


/*
 * procedure
 */

typedef pic_value (*pic_func_t)(pic_state *);
bool pic_proc_p(pic_state *, pic_value);
pic_value pic_lambda(pic_state *, pic_func_t f, int n, ...);
pic_value pic_vlambda(pic_state *, pic_func_t f, int n, va_list);
int pic_get_args(pic_state *, const char *fmt, ...);
pic_value pic_closure_ref(pic_state *, int i);
void pic_closure_set(pic_state *, int i, pic_value v);
pic_value pic_call(pic_state *, pic_value proc, int, ...);
pic_value pic_callk(pic_state *, pic_value proc, int, ...);
pic_value pic_vcall(pic_state *, pic_value proc, int, va_list);
pic_value pic_vcallk(pic_state *, pic_value proc, int, va_list);
pic_value pic_apply(pic_state *, pic_value proc, int n, pic_value *argv);
pic_value pic_applyk(pic_state *, pic_value proc, int n, pic_value *argv);


/*
 * core language features
 */

void pic_add_feature(pic_state *, const char *feature);
void pic_define(pic_state *, const char *name, pic_value v);
pic_value pic_ref(pic_state *, const char *name);
void pic_set(pic_state *, const char *name, pic_value v);
pic_value pic_make_var(pic_state *, pic_value init, pic_value conv);
void pic_defun(pic_state *, const char *name, pic_func_t f);
void pic_defvar(pic_state *, const char *name, pic_value v);
pic_value pic_funcall(pic_state *, const char *name, int n, ...);
pic_value pic_values(pic_state *, int n, ...);
pic_value pic_vvalues(pic_state *, int n, va_list);
PIC_NORETURN void pic_error(pic_state *, const char *msg, int n, ...);
PIC_NORETURN void pic_verror(pic_state *pic, const char *msg, int n, va_list ap);


/*
 * utility macros
 */

#define pic_for_each(var, list, it)                                     \
  for (it = (list); ! pic_nil_p(pic, it); it = pic_cdr(pic, it))        \
    if ((var = pic_car(pic, it)), true)

#define pic_push(pic, item, place) (place = pic_cons(pic, item, place))
#define pic_pop(pic, place) (place = pic_cdr(pic, place))

#define pic_void(pic, exec) pic_void_(pic, PIC_GENSYM(ai), exec)
#define pic_void_(pic,ai,exec) do {             \
    size_t ai = pic_enter(pic);                 \
    exec;                                       \
    pic_leave(pic, ai);                         \
  } while (0)


#if defined(__cplusplus)
}
#endif

#endif
