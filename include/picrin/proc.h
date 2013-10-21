#ifndef PROC_H__
#define PROC_H__

struct pic_env {
  pic_value assoc;
  struct pic_env *parent;
};

struct pic_proc {
  PIC_OBJECT_HEADER
  bool cfunc_p;
  union {
    pic_func_t cfunc;
    struct pic_irep *irep;
  } u;
  pic_value aux;
};

#define pic_proc_ptr(o) ((struct pic_proc *)o.u.data)

#define pic_proc_cfunc_p(o) (pic_proc_ptr(o)->cfunc_p)

struct pic_proc *pic_proc_new(pic_state *, struct pic_irep *irep);
struct pic_proc *pic_proc_new_cfunc(pic_state *, pic_func_t cfunc, pic_value aux);

#endif
