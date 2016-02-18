/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_STRING_H
#define PICRIN_STRING_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_string {
  PIC_OBJECT_HEADER
  struct pic_rope *rope;
};

void pic_rope_incref(pic_state *, struct pic_rope *);
void pic_rope_decref(pic_state *, struct pic_rope *);

#define pic_str_ptr(o) ((struct pic_string *)pic_obj_ptr(o))

#if defined(__cplusplus)
}
#endif

#endif
