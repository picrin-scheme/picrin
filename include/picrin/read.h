/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_READ_H__
#define PICRIN_READ_H__

#if defined(__cplusplus)
extern "C" {
#endif

enum pic_typecase {
  PIC_CASE_DEFAULT,
  PIC_CASE_FOLD,
};

struct pic_reader {
  short typecase;
  xhash labels;
};

#if defined(__cplusplus)
}
#endif

#endif
