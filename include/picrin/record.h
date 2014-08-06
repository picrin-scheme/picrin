/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_RECORD_H
#define PICRIN_RECORD_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_record {
  PIC_OBJECT_HEADER
  xhash     hash;
};

#define pic_record_p(v) (pic_type(v) == PIC_TT_RECORD)
#define pic_record_ptr(v) ((struct pic_record *)pic_ptr(v))

struct pic_record *pic_record_new(pic_state *, pic_value);

pic_value pic_record_type(pic_state *, struct pic_record *);
pic_value pic_record_ref(pic_state *, struct pic_record *, pic_sym);
void pic_record_set(pic_state *, struct pic_record *, pic_sym, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
