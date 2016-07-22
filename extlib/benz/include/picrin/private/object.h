/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_OBJECT_H
#define PICRIN_OBJECT_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "picrin/private/file.h"
#include "picrin/private/khash.h"
#include "picrin/private/gc.h"

typedef struct identifier symbol;

KHASH_DECLARE(env, struct identifier *, symbol *)
KHASH_DECLARE(dict, symbol *, pic_value)
KHASH_DECLARE(weak, struct object *, pic_value)

struct object;              /* defined in gc.c */

struct basic {
  OBJECT_HEADER
};

struct identifier {
  OBJECT_HEADER
  union {
    struct string *str;
    struct identifier *id;
  } u;
  struct env *env;
};

struct env {
  OBJECT_HEADER
  khash_t(env) map;
  struct env *up;
  struct string *lib;
};

struct pair {
  OBJECT_HEADER
  pic_value car;
  pic_value cdr;
};

struct blob {
  OBJECT_HEADER
  unsigned char *data;
  int len;
};

struct string {
  OBJECT_HEADER
  struct rope *rope;
};

struct dict {
  OBJECT_HEADER
  khash_t(dict) hash;
};

struct weak {
  OBJECT_HEADER
  khash_t(weak) hash;
  struct weak *prev;         /* for GC */
};

struct vector {
  OBJECT_HEADER
  pic_value *data;
  int len;
};

struct data {
  OBJECT_HEADER
  const pic_data_type *type;
  void *data;
};

struct context {
  OBJECT_HEADER
  pic_value *regs;
  int regc;
  struct context *up;
  pic_value storage[1];
};

struct proc {
  OBJECT_HEADER
  union {
    struct {
      pic_func_t func;
      int localc;
    } f;
    struct {
      struct irep *irep;
      struct context *cxt;
    } i;
  } u;
  pic_value locals[1];
};

struct record {
  OBJECT_HEADER
  pic_value type;
  pic_value datum;
};

struct error {
  OBJECT_HEADER
  symbol *type;
  struct string *msg;
  pic_value irrs;
  struct string *stack;
};

struct port {
  OBJECT_HEADER
  struct file file;
};

struct checkpoint {
  OBJECT_HEADER
  struct proc *in;
  struct proc *out;
  int depth;
  struct checkpoint *prev;
};

struct object *pic_obj_ptr(pic_value);

#define pic_id_ptr(pic, o) (assert(pic_id_p(pic, o)), (struct identifier *)pic_obj_ptr(o))
#define pic_sym_ptr(pic, o) (assert(pic_sym_p(pic, o)), (symbol *)pic_obj_ptr(o))
#define pic_str_ptr(pic, o) (assert(pic_str_p(pic, o)), (struct string *)pic_obj_ptr(o))
#define pic_blob_ptr(pic, o) (assert(pic_blob_p(pic, o)), (struct blob *)pic_obj_ptr(o))
#define pic_pair_ptr(pic, o) (assert(pic_pair_p(pic, o)), (struct pair *)pic_obj_ptr(o))
#define pic_vec_ptr(pic, o) (assert(pic_vec_p(pic, o)), (struct vector *)pic_obj_ptr(o))
#define pic_dict_ptr(pic, o) (assert(pic_dict_p(pic, o)), (struct dict *)pic_obj_ptr(o))
#define pic_weak_ptr(pic, o) (assert(pic_weak_p(pic, o)), (struct weak *)pic_obj_ptr(o))
#define pic_data_ptr(pic, o) (assert(pic_data_p(pic, o, NULL)), (struct data *)pic_obj_ptr(o))
#define pic_proc_ptr(pic, o) (assert(pic_proc_p(pic, o)), (struct proc *)pic_obj_ptr(o))
#define pic_env_ptr(pic, o) (assert(pic_env_p(pic, o)), (struct env *)pic_obj_ptr(o))
#define pic_port_ptr(pic, o) (assert(pic_port_p(pic, o)), (struct port *)pic_obj_ptr(o))
#define pic_error_ptr(pic, o) (assert(pic_error_p(pic, o)), (struct error *)pic_obj_ptr(o))
#define pic_rec_ptr(pic, o) (assert(pic_rec_p(pic, o)), (struct record *)pic_obj_ptr(o))

#define pic_obj_p(pic,v) (pic_type(pic,v) > PIC_IVAL_END)
#define pic_env_p(pic, v) (pic_type(pic, v) == PIC_TYPE_ENV)
#define pic_error_p(pic, v) (pic_type(pic, v) == PIC_TYPE_ERROR)
#define pic_rec_p(pic, v) (pic_type(pic, v) == PIC_TYPE_RECORD)

pic_value pic_obj_value(void *ptr);
struct object *pic_obj_alloc(pic_state *, size_t, int type);

#define TYPENAME_int   "integer"
#define TYPENAME_blob  "bytevector"
#define TYPENAME_char  "character"
#define TYPENAME_sym   "symbol"
#define TYPENAME_error "error"
#define TYPENAME_proc  "procedure"
#define TYPENAME_str   "string"
#define TYPENAME_id    "identifier"
#define TYPENAME_env   "environment"
#define TYPENAME_vec   "vector"

#define TYPE_CHECK(pic, v, type) do {                           \
    if (! pic_##type##_p(pic, v))                               \
      pic_error(pic, TYPENAME_##type " required", 1, v);        \
  } while (0)

#define VALID_INDEX(pic, len, i) do {                                   \
    if (i < 0 || len <= i) pic_error(pic, "index out of range", 1, pic_int_value(pic, i)); \
  } while (0)
#define VALID_RANGE(pic, len, s, e) do {                                \
    if (s < 0 || len < s) pic_error(pic, "invalid start index", 1, pic_int_value(pic, s)); \
    if (e < s || len < e) pic_error(pic, "invalid end index", 1, pic_int_value(pic, e)); \
  } while (0)
#define VALID_ATRANGE(pic, tolen, at, fromlen, s, e) do {               \
    VALID_INDEX(pic, tolen, at);                                        \
    VALID_RANGE(pic, fromlen, s, e);                                    \
    if (tolen - at < e - s) pic_error(pic, "invalid range", 0);        \
  } while (0)

pic_value pic_make_identifier(pic_state *, pic_value id, pic_value env);
pic_value pic_make_proc(pic_state *, pic_func_t, int, pic_value *);
pic_value pic_make_proc_irep(pic_state *, struct irep *, struct context *);
pic_value pic_make_env(pic_state *, pic_value env);
pic_value pic_make_rec(pic_state *, pic_value type, pic_value datum);

pic_value pic_add_identifier(pic_state *, pic_value id, pic_value env);
void pic_put_identifier(pic_state *, pic_value id, pic_value uid, pic_value env);
pic_value pic_find_identifier(pic_state *, pic_value id, pic_value env);
pic_value pic_id_name(pic_state *, pic_value id);

struct rope *pic_rope_incref(struct rope *);
void pic_rope_decref(pic_state *, struct rope *);

#define pic_func_p(pic, proc) (pic_type(pic, proc) == PIC_TYPE_FUNC)
#define pic_irep_p(pic, proc) (pic_type(pic, proc) == PIC_TYPE_IREP)

struct cont *pic_alloca_cont(pic_state *);
pic_value pic_make_cont(pic_state *, struct cont *);
void pic_save_point(pic_state *, struct cont *, PIC_JMPBUF *);
void pic_exit_point(pic_state *);
void pic_wind(pic_state *, struct checkpoint *, struct checkpoint *);
pic_value pic_dynamic_wind(pic_state *, pic_value in, pic_value thunk, pic_value out);

pic_value pic_dynamic_bind(pic_state *, pic_value var, pic_value val, pic_value thunk);

pic_value pic_library_environment(pic_state *, const char *);

#if defined(__cplusplus)
}
#endif

#endif
