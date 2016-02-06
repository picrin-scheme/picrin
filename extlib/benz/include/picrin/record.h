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
  pic_value type;
  pic_value datum;
};

#define pic_rec_p(v) (pic_type(v) == PIC_TT_RECORD)
#define pic_rec_ptr(v) ((struct pic_record *)pic_ptr(v))

struct pic_record *pic_make_rec(pic_state *, pic_value, pic_value);

pic_value pic_rec_type(pic_state *, struct pic_record *);
pic_value pic_rec_datum(pic_state *, struct pic_record *);

#if defined(__cplusplus)
}
#endif

#endif
