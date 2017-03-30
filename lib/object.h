/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_OBJECT_H
#define PICRIN_OBJECT_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "khash.h"

#if PIC_BITMAP_GC
# define OBJECT_HEADER                           \
  unsigned char tt;
#else
# define OBJECT_HEADER                           \
  unsigned char tt;                              \
  char gc_mark;
#endif

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

typedef struct identifier symbol;

KHASH_DECLARE(env, struct identifier *, symbol *)

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

KHASH_DECLARE(dict, symbol *, pic_value)

struct dict {
  OBJECT_HEADER
  khash_t(dict) hash;
};

KHASH_DECLARE(weak, struct object *, pic_value)

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

struct code {
  int insn;
  int a;
  int b;
};

struct irep {
  OBJECT_HEADER
  int argc, localc, capturec;
  bool varg;
  struct code *code;
  struct irep **irep;
  int *ints;
  double *nums;
  struct object **pool;
  size_t ncode, nirep, nints, nnums, npool;
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

enum {
  FILE_READ  = 01,
  FILE_WRITE = 02,
  FILE_UNBUF = 04,
  FILE_EOF   = 010,
  FILE_ERR   = 020,
  FILE_LNBUF = 040
};

struct port {
  OBJECT_HEADER
  struct file {
    /* buffer */
    char buf[1];                  /* fallback buffer */
    long cnt;                     /* characters left */
    char *ptr;                    /* next character position */
    char *base;                   /* location of the buffer */
    /* operators */
    void *cookie;
    const pic_port_type *vtable;
    int flag;                     /* mode of the file access */
  } file;
};

struct checkpoint {
  OBJECT_HEADER
  struct proc *in;
  struct proc *out;
  int depth;
  struct checkpoint *prev;
};

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

PIC_STATIC_INLINE int
obj_tt(pic_state *PIC_UNUSED(pic), void *ptr)
{
  return ((struct basic *)ptr)->tt;
}

#if !PIC_NAN_BOXING

PIC_STATIC_INLINE struct object *
obj_ptr(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return (struct object *)(v.u.data);
}

PIC_STATIC_INLINE bool
obj_p(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v.type > PIC_IVAL_END;
}

PIC_STATIC_INLINE pic_value
obj_value(pic_state *PIC_UNUSED(pic), void *ptr)
{
  pic_value v = pic_make_value(obj_tt(pic, ptr));
  v.u.data = ptr;
  return v;
}

#else  /* NAN_BOXING */

PIC_STATIC_INLINE struct object *
obj_ptr(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return (struct object *)((0x3ffffffffffful & v.v) << 2);
}

PIC_STATIC_INLINE bool
obj_p(pic_state *PIC_UNUSED(pic), pic_value v)
{
  return v.v > ((0x3ffC0ul + (0x3f & PIC_IVAL_END)) << 46);
}

PIC_STATIC_INLINE pic_value
obj_value(pic_state *PIC_UNUSED(pic), void *ptr)
{
  pic_value v = pic_make_value(obj_tt(pic, ptr));
  v.v |= 0x3ffffffffffful & ((uint64_t)ptr >> 2);
  return v;
}

#endif  /* NAN_BOXING */

#define DEFPTR(name,type)                                               \
  PIC_STATIC_INLINE type *name(pic_state *PIC_UNUSED(pic), pic_value o) { \
    return (type *) obj_ptr(pic, o);                                    \
  }

DEFPTR(pic_id_ptr, struct identifier)
DEFPTR(pic_sym_ptr, symbol)
DEFPTR(pic_str_ptr, struct string)
DEFPTR(pic_blob_ptr, struct blob)
DEFPTR(pic_pair_ptr, struct pair)
DEFPTR(pic_vec_ptr, struct vector)
DEFPTR(pic_dict_ptr, struct dict)
DEFPTR(pic_weak_ptr, struct weak)
DEFPTR(pic_data_ptr, struct data)
DEFPTR(pic_proc_ptr, struct proc)
DEFPTR(pic_env_ptr, struct env)
DEFPTR(pic_port_ptr, struct port)
DEFPTR(pic_error_ptr, struct error)
DEFPTR(pic_rec_ptr, struct record)
DEFPTR(pic_cp_ptr, struct checkpoint)
DEFPTR(pic_irep_ptr, struct irep)

struct object *pic_obj_alloc(pic_state *, size_t, int type);

pic_value pic_make_identifier(pic_state *, pic_value id, pic_value env);
pic_value pic_make_proc(pic_state *, pic_func_t, int, pic_value *);
pic_value pic_make_proc_irep(pic_state *, struct irep *, struct context *);
pic_value pic_make_env(pic_state *, pic_value env);
pic_value pic_make_record(pic_state *, pic_value type, pic_value datum);

pic_value pic_add_identifier(pic_state *, pic_value id, pic_value env);
void pic_put_identifier(pic_state *, pic_value id, pic_value uid, pic_value env);
pic_value pic_find_identifier(pic_state *, pic_value id, pic_value env);
pic_value pic_id_name(pic_state *, pic_value id);

struct rope *pic_rope_incref(struct rope *);
void pic_rope_decref(pic_state *, struct rope *);

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
