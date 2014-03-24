/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_ERROR_H__
#define PICRIN_ERROR_H__

#if defined(__cplusplus)
extern "C" {
#endif

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

#define pic_try                                                 \
  pic_try_helper__(GENSYM(i), GENSYM(here), GENSYM(prev_jmp))
#define pic_try_helper__(i, here, prev_jmp)                             \
  for (int i = 0; ! i; )                                                \
    for (jmp_buf here, *prev_jmp = pic->jmp; ! i; )                     \
      for (pic->jmp = &here; ! i++; pic->jmp = prev_jmp)                \
        if (setjmp(here) == 0)
#define pic_catch else

pic_value pic_raise_continuable(pic_state *, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
