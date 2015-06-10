/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_DATA_H
#define PICRIN_DATA_H

#if defined(__cplusplus)
extern "C" {
#endif

typedef struct {
  const char *type_name;
  void (*dtor)(pic_state *, void *);
  void (*mark)(pic_state *, void *, void (*)(pic_state *, pic_value));
} pic_data_type;

struct pic_data {
  PIC_OBJECT_HEADER
  const pic_data_type *type;
  xhash storage;                /* const char * to pic_value table */
  void *data;
};

#define pic_data_p(o) (pic_type(o) == PIC_TT_DATA)
#define pic_data_ptr(o) ((struct pic_data *)pic_ptr(o))

PIC_INLINE bool pic_data_type_p(const pic_value obj, const pic_data_type *type) {
  return pic_data_p(obj) && pic_data_ptr(obj)->type == type;
}

struct pic_data *pic_data_alloc(pic_state *, const pic_data_type *, void *);

#if defined(__cplusplus)
}
#endif

#endif
