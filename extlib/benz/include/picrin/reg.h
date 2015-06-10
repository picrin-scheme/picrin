/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_REG_H
#define PICRIN_REG_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_reg {
  PIC_OBJECT_HEADER
  xhash hash;
  struct pic_reg *prev;         /* for GC */
};

#define pic_reg_p(v) (pic_type(v) == PIC_TT_REG)
#define pic_reg_ptr(v) ((struct pic_reg *)pic_ptr(v))

struct pic_reg *pic_make_reg(pic_state *);

pic_value pic_reg_ref(pic_state *, struct pic_reg *, void *);
void pic_reg_set(pic_state *, struct pic_reg *, void *, pic_value);
void pic_reg_del(pic_state *, struct pic_reg *, void *);
bool pic_reg_has(pic_state *, struct pic_reg *, void *);

#if defined(__cplusplus)
}
#endif

#endif
