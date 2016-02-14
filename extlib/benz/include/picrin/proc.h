/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_PROC_H
#define PICRIN_PROC_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_context {
  PIC_OBJECT_HEADER
  pic_value *regs;
  int regc;
  struct pic_context *up;
  pic_value storage[1];
};

struct pic_proc {
  PIC_OBJECT_HEADER
  enum {
    PIC_PROC_TAG_IREP,
    PIC_PROC_TAG_FUNC
  } tag;
  union {
    struct {
      pic_func_t func;
      int localc;
    } f;
    struct {
      struct pic_irep *irep;
      struct pic_context *cxt;
    } i;
  } u;
  pic_value locals[1];
};

#define pic_proc_func_p(proc) ((proc)->tag == PIC_PROC_TAG_FUNC)
#define pic_proc_irep_p(proc) ((proc)->tag == PIC_PROC_TAG_IREP)

#define pic_proc_ptr(o) ((struct pic_proc *)pic_ptr(o))

#define pic_context_p(o) (pic_type(o) == PIC_TT_CXT)
#define pic_context_ptr(o) ((struct pic_context *)pic_ptr(o))

struct pic_proc *pic_make_proc(pic_state *, pic_func_t, int, pic_value *);
struct pic_proc *pic_make_proc_irep(pic_state *, struct pic_irep *, struct pic_context *);

#if defined(__cplusplus)
}
#endif

#endif
