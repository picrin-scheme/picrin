/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_ERROR_H
#define PICRIN_ERROR_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_error {
  PIC_OBJECT_HEADER
  pic_sym *type;
  pic_str *msg;
  pic_value irrs;
  pic_str *stack;
};

#define pic_error_p(v) (pic_type(v) == PIC_TT_ERROR)
#define pic_error_ptr(v) ((struct pic_error *)pic_ptr(v))

struct pic_error *pic_make_error(pic_state *, pic_sym *, const char *, pic_list);

/* do not return from try block! */

#define pic_try                                 \
  pic_try_(PIC_GENSYM(cont), PIC_GENSYM(handler))
#define pic_catch                               \
  pic_catch_(PIC_GENSYM(label))
#define pic_try_(cont, handler)                                         \
  do {                                                                  \
    struct pic_cont cont;                                               \
    pic_save_point(pic, &cont);                                         \
    if (PIC_SETJMP(pic, cont.jmp) == 0) {                               \
      extern pic_value pic_native_exception_handler(pic_state *);       \
      struct pic_proc *handler;                                         \
      handler = pic_make_proc(pic, pic_native_exception_handler, "(native-exception-handler)"); \
      pic_proc_env_set(pic, handler, "cont", pic_obj_value(pic_make_cont(pic, &cont))); \
      do {                                                              \
        pic_push_handler(pic, handler);
#define pic_catch_(label)                                 \
  pic_pop_handler(pic);                                   \
      } while (0);                                        \
      pic->cc = pic->cc->prev;                            \
    } else {                                              \
      pic->cc = pic->cc->prev;                            \
      goto label;                                         \
    }                                                     \
  } while (0);                                            \
  if (0)                                                  \
  label:

void pic_push_handler(pic_state *, struct pic_proc *);
struct pic_proc *pic_pop_handler(pic_state *);

pic_value pic_raise_continuable(pic_state *, pic_value);
PIC_NORETURN void pic_raise(pic_state *, pic_value);
PIC_NORETURN void pic_error(pic_state *, const char *, pic_list);

#if defined(__cplusplus)
}
#endif

#endif
