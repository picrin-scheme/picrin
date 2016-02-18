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


#if defined(__cplusplus)
}
#endif

#endif
