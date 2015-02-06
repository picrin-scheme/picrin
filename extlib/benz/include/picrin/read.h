/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_READ_H
#define PICRIN_READ_H

#if defined(__cplusplus)
extern "C" {
#endif

typedef pic_value (*pic_reader_t)(pic_state *, struct pic_port *port, int c);

struct pic_reader {
  enum pic_typecase {
    PIC_CASE_DEFAULT,
    PIC_CASE_FOLD
  } typecase;
  xhash labels;
  pic_reader_t table[256];
  pic_reader_t dispatch[256];
};

struct pic_reader *pic_reader_open(pic_state *);
void pic_reader_close(pic_state *, struct pic_reader *);

#if defined(__cplusplus)
}
#endif

#endif
