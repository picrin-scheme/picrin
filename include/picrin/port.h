#ifndef PORT_H__
#define PORT_H__

#include <stdio.h>

enum pic_port_flag {
  PIC_PORT_IN = 1,
  PIC_PORT_OUT = 2,
  PIC_PORT_TEXT = 4,
  PIC_PORT_BINARY = 8
};

enum pic_port_status {
  PIC_PORT_OPEN,
  PIC_PORT_CLOSE,
};

struct pic_port {
  PIC_OBJECT_HEADER
  FILE *file;
  short flags;
  enum pic_port_status status;
};

#define pic_port_p(v) (pic_type(v) == PIC_TT_PORT)
#define pic_port_ptr(v) ((struct pic_port *)(v).u.data)

#endif
