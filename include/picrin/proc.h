/**
 * See Copyright Notice in picrin.h
 */

#ifndef PROC_H__
#define PROC_H__

struct pic_env {
  PIC_OBJECT_HEADER
  pic_value *values;
  int valuec;
  struct pic_env *up;
};

struct pic_proc {
  PIC_OBJECT_HEADER
  bool cfunc_p;
  union {
    pic_func_t cfunc;
    struct pic_irep *irep;
  } u;
  struct pic_env *env;
};

#define pic_proc_p(o) (pic_type(o) == PIC_TT_PROC)
#define pic_env_p(o) (pic_type(o) == PIC_TT_ENV)

#define pic_proc_ptr(o) ((struct pic_proc *)pic_ptr(o))
#define pic_env_ptr(o) ((struct pic_env *)pic_ptr(o))

#define pic_proc_cfunc_p(o) (pic_proc_ptr(o)->cfunc_p)

struct pic_proc *pic_proc_new(pic_state *, pic_func_t);
struct pic_proc *pic_proc_new_irep(pic_state *, struct pic_irep *, struct pic_env *);

/* closed variables accessor */
void pic_proc_cv_init(pic_state *, struct pic_proc *, size_t);
int pic_proc_cv_size(pic_state *, struct pic_proc *);
pic_value pic_proc_cv_ref(pic_state *, struct pic_proc *, size_t);
void pic_proc_cv_set(pic_state *, struct pic_proc *, size_t, pic_value);

#endif
