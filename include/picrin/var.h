#ifndef VAR_H__
#define VAR_H__

#include "picrin.h"
#include "picrin/proc.h"

struct pic_var {
  PIC_OBJECT_HEADER
  pic_value value;
  struct pic_proc *conv;
};

#define pic_var_p(o) (pic_type(o) == PIC_TT_VAR)
#define pic_var_ptr(o) ((struct pic_var *)pic_ptr(o))

struct pic_var *pic_var_new(pic_state *, pic_value, struct pic_proc *);

pic_value pic_var_ref(pic_state *, struct pic_var *);
void pic_var_set(pic_state *, struct pic_var *, pic_value);
void pic_var_set_force(pic_state *, struct pic_var *, pic_value);

#endif
