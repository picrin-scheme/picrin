/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_ERROR_H
#define PICRIN_ERROR_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_jmpbuf {
  jmp_buf here;
  struct pic_proc *handler;
  ptrdiff_t ci_offset;
  ptrdiff_t sp_offset;
  pic_code *ip;
  jmp_buf *prev_jmp;
};

/* do not return from try block! */

#define pic_try                                 \
  pic_try_with_handler(NULL)
#define pic_try_with_handler(handler)           \
  pic_push_try(pic, handler);                   \
  if (setjmp(*pic->jmp) == 0)                   \
    do
#define pic_catch                               \
    while (pic_pop_try(pic), 0);                \
  else                                          \
    if (pic_pop_try(pic), 1)

void pic_push_try(pic_state *, struct pic_proc *);
void pic_pop_try(pic_state *);

noreturn void pic_throw(pic_state *, short, const char *, pic_value);
noreturn void pic_throw_error(pic_state *, struct pic_error *);

struct pic_error {
  PIC_OBJECT_HEADER
  enum pic_error_kind {
    PIC_ERROR_OTHER,
    PIC_ERROR_FILE,
    PIC_ERROR_READ,
    PIC_ERROR_RAISED
  } type;
  struct pic_string *msg;
  pic_value irrs;
  pic_str *stack;
};

#define pic_error_p(v) (pic_type(v) == PIC_TT_ERROR)
#define pic_error_ptr(v) ((struct pic_error *)pic_ptr(v))

#if defined(__cplusplus)
}
#endif

#endif
