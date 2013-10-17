#ifndef PORT_H__
#define PORT_H__

#include "picrin.h"

enum pic_port_flag {
  PIC_PORT_IN,
  PIC_PORT_OUT,
  PIC_PORT_TEXT,
  PIC_PORT_BINARY
};

enum pic_port_status {
  PIC_PORT_OPEN,
  PIC_PORT_CLOSE,
};

struct pic_port {
  PIC_OBJECT_HEADER
  FILE *file;
  short flags;
  char status;
};

#define pic_port_ptr(v) ((struct pic_port_t *)v.u.data)

#endif
