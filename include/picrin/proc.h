#ifndef PROC_H__
#define PROC_H__

struct pic_env {
  PIC_OBJECT_HEADER
  pic_value *values;
  int num_val;
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

#define pic_proc_ptr(o) ((struct pic_proc *)(o).u.data)
#define pic_env_ptr(o) ((struct pic_env *)(o).u.data)

#define pic_proc_cfunc_p(o) (pic_proc_ptr(o)->cfunc_p)

struct pic_proc *pic_proc_new(pic_state *, struct pic_irep *, struct pic_env *);
struct pic_proc *pic_proc_new_cfunc(pic_state *, pic_func_t);

#endif
