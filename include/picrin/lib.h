#ifndef LIB_H__
#define LIB_H__

struct pic_lib {
  PIC_OBJECT_HEADER
  struct pic_senv *senv;
  struct xhash *exports;
};

#endif
