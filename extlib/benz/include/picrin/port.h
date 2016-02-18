/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_PORT_H
#define PICRIN_PORT_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_port {
  PIC_OBJECT_HEADER
  xFILE *file;
};

#define pic_port_ptr(v) ((struct pic_port *)pic_obj_ptr(v))

struct pic_port *pic_make_port(pic_state *, xFILE *file);
void pic_close_port(pic_state *, struct pic_port *port);

#if defined(__cplusplus)
}
#endif

#endif
