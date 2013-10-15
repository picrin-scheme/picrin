#ifndef PROC_H__
#define PROC_H__

struct pic_proc {
  PIC_OBJECT_HEADER
  union {
    pic_value (*cfunc)(pic_state *);
    struct pic_irep *irep;
  } u;
};

#define pic_proc_ptr(o) ((struct pic_proc *)o.u.data)

#endif
