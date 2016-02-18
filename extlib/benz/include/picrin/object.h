/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_OBJECT_H
#define PICRIN_OBJECT_H

#if defined(__cplusplus)
extern "C" {
#endif


/* symbol & identifier */

struct pic_id {
  union {
    struct pic_symbol {
      PIC_OBJECT_HEADER
      struct pic_string *str;
    } sym;
    struct {
      PIC_OBJECT_HEADER
      struct pic_id *id;
      struct pic_env *env;
    } id;
  } u;
};

#define pic_sym_ptr(v) ((pic_sym *)pic_obj_ptr(v))

#define pic_id_p(pic, v) (pic_type(pic, v) == PIC_TYPE_ID || pic_type(pic, v) == PIC_TYPE_SYMBOL)
#define pic_id_ptr(v) ((pic_id *)pic_obj_ptr(v))

pic_id *pic_make_identifier(pic_state *, pic_id *, struct pic_env *);

struct pic_string *pic_id_name(pic_state *, pic_id *);


/* pair */

struct pic_pair {
  PIC_OBJECT_HEADER
  pic_value car;
  pic_value cdr;
};

#define pic_pair_ptr(o) ((struct pic_pair *)pic_obj_ptr(o))


/* blob */

struct pic_blob {
  PIC_OBJECT_HEADER
  unsigned char *data;
  int len;
};

#define pic_blob_ptr(v) ((struct pic_blob *)pic_obj_ptr(v))

/* string */

struct pic_string {
  PIC_OBJECT_HEADER
  struct pic_rope *rope;
};

void pic_rope_incref(pic_state *, struct pic_rope *);
void pic_rope_decref(pic_state *, struct pic_rope *);

#define pic_str_ptr(o) ((struct pic_string *)pic_obj_ptr(o))


/* vector */

struct pic_vector {
  PIC_OBJECT_HEADER
  pic_value *data;
  int len;
};

#define pic_vec_ptr(o) ((struct pic_vector *)pic_obj_ptr(o))


/* dictionary */

KHASH_DECLARE(dict, pic_sym *, pic_value)

struct pic_dict {
  PIC_OBJECT_HEADER
  khash_t(dict) hash;
};

#define pic_dict_ptr(v) ((struct pic_dict *)pic_obj_ptr(v))


/* weak */

KHASH_DECLARE(weak, void *, pic_value)

struct pic_weak {
  PIC_OBJECT_HEADER
  khash_t(weak) hash;
  struct pic_weak *prev;         /* for GC */
};

#define pic_weak_ptr(v) ((struct pic_weak *)pic_obj_ptr(v))


/* data */

struct pic_data {
  PIC_OBJECT_HEADER
  const pic_data_type *type;
  void *data;
};

#define pic_data_ptr(o) ((struct pic_data *)pic_obj_ptr(o))


/* context */

struct pic_context {
  PIC_OBJECT_HEADER
  pic_value *regs;
  int regc;
  struct pic_context *up;
  pic_value storage[1];
};

#define pic_context_ptr(o) ((struct pic_context *)pic_obj_ptr(o))


/* procedure */

struct pic_proc {
  PIC_OBJECT_HEADER
  enum {
    PIC_PROC_TAG_IREP,
    PIC_PROC_TAG_FUNC
  } tag;
  union {
    struct {
      pic_func_t func;
      int localc;
    } f;
    struct {
      struct pic_irep *irep;
      struct pic_context *cxt;
    } i;
  } u;
  pic_value locals[1];
};

#define pic_proc_ptr(o) ((struct pic_proc *)pic_obj_ptr(o))

#define pic_proc_func_p(proc) ((proc)->tag == PIC_PROC_TAG_FUNC)
#define pic_proc_irep_p(proc) ((proc)->tag == PIC_PROC_TAG_IREP)

struct pic_proc *pic_make_proc(pic_state *, pic_func_t, int, pic_value *);
struct pic_proc *pic_make_proc_irep(pic_state *, struct pic_irep *, struct pic_context *);


/* record */

struct pic_record {
  PIC_OBJECT_HEADER
  pic_value type;
  pic_value datum;
};

#define pic_rec_p(pic, v) (pic_type(pic, v) == PIC_TYPE_RECORD)
#define pic_rec_ptr(v) ((struct pic_record *)pic_obj_ptr(v))

struct pic_record *pic_make_rec(pic_state *, pic_value, pic_value);


/* error */

struct pic_error {
  PIC_OBJECT_HEADER
  pic_sym *type;
  struct pic_string *msg;
  pic_value irrs;
  struct pic_string *stack;
};

#define pic_error_p(pic, v) (pic_type(pic, v) == PIC_TYPE_ERROR)
#define pic_error_ptr(v) ((struct pic_error *)pic_obj_ptr(v))

struct pic_error *pic_make_error(pic_state *, const char *, const char *, pic_value);


/* port */

struct pic_port {
  PIC_OBJECT_HEADER
  xFILE *file;
};

#define pic_port_ptr(v) ((struct pic_port *)pic_obj_ptr(v))


#if defined(__cplusplus)
}
#endif

#endif
