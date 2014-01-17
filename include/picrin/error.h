/**
 * See Copyright Notice in picrin.h
 */

#ifndef ERROR_H__
#define ERROR_H__

struct pic_error {
  PIC_OBJECT_HEADER
  enum pic_error_kind {
    PIC_ERROR_OTHER,
    PIC_ERROR_FILE,
    PIC_ERROR_READ
  } type;
  char *msg;
  pic_value irrs;
};

#define pic_error_p(v) (pic_type(v) == PIC_TT_ERROR)
#define pic_error_ptr(v) ((struct pic_error *)pic_ptr(v))

#endif
