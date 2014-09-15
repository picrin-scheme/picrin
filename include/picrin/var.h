/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_VAR_H
#define PICRIN_VAR_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_var {
  PIC_OBJECT_HEADER
  pic_value stack;
  struct pic_proc *conv;
};

#define pic_var_p(o) (pic_type(o) == PIC_TT_VAR)
#define pic_var_ptr(o) ((struct pic_var *)pic_ptr(o))

struct pic_var *pic_make_var(pic_state *, pic_value, struct pic_proc * /* = NULL */);

pic_value pic_var_ref(pic_state *, struct pic_var *);
void pic_var_set(pic_state *, struct pic_var *, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
