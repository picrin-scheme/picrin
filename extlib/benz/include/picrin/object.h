/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_OBJECT_H
#define PICRIN_OBJECT_H

#if defined(__cplusplus)
extern "C" {
#endif


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

#if defined(__cplusplus)
}
#endif

#endif
