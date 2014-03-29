/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_DATA_H__
#define PICRIN_DATA_H__

#if defined(__cplusplus)
extern "C" {
#endif

typedef struct {
  const char *type_name;
  void (*dtor)(pic_state *, void *);
} pic_data_type;

struct pic_data {
  PIC_OBJECT_HEADER;
  const pic_data_type *type;
  xhash storage;                /* const char * to pic_value table */
  void *data;
};

#define pic_data_p(o) (pic_type(o) == PIC_TT_DATA)
#define pic_data_ptr(o) ((struct pic_data *)pic_ptr(o))

struct pic_data *pic_data_alloc(pic_state *, const pic_data_type *, void *);

#if defined(__cplusplus)
}
#endif

#endif
