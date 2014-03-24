/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_ERROR_H__
#define PICRIN_ERROR_H__

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_jmpbuf {
  jmp_buf here;
  jmp_buf *prev_jmp;
  struct pic_jmpbuf *prev;
};

#define pic_try                                 \
  pic_push_try(pic);                            \
  if (setjmp(*pic->jmp) == 0)                   \
    do
#define pic_catch                               \
    while (pic_pop_try(pic), 0);                \
  else                                          \
    if (pic_pop_try(pic), 1)

void pic_push_try(pic_state *);
void pic_pop_try(pic_state *);

noreturn void pic_throw(pic_state *, struct pic_error *);

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
};

#define pic_error_p(v) (pic_type(v) == PIC_TT_ERROR)
#define pic_error_ptr(v) ((struct pic_error *)pic_ptr(v))

pic_value pic_raise_continuable(pic_state *, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
