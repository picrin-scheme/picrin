/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_PORT_H
#define PICRIN_PORT_H

#if defined(__cplusplus)
extern "C" {
#endif

enum pic_port_flag {
  PIC_PORT_IN = 1,
  PIC_PORT_OUT = 2,
  PIC_PORT_TEXT = 4,
  PIC_PORT_BINARY = 8,
  PIC_PORT_OPEN = 16
};

struct pic_port {
  PIC_OBJECT_HEADER
  xFILE *file;
  int flags;
};

#define pic_port_p(v) (pic_type(v) == PIC_TT_PORT)
#define pic_port_ptr(v) ((struct pic_port *)pic_ptr(v))

pic_value pic_eof_object();

struct pic_port *pic_open_input_string(pic_state *, const char *);
struct pic_port *pic_open_output_string(pic_state *);
struct pic_string *pic_get_output_string(pic_state *, struct pic_port *);

struct pic_port *pic_open_file(pic_state *, const char *, int);
void pic_close_port(pic_state *pic, struct pic_port *);

#if defined(__cplusplus)
}
#endif

#endif
