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
  pic_sym type;
  pic_str *msg;
  pic_value irrs;
  pic_str *stack;
};

#define pic_error_p(v) (pic_type(v) == PIC_TT_ERROR)
#define pic_error_ptr(v) ((struct pic_error *)pic_ptr(v))

struct pic_error *pic_make_error(pic_state *, pic_sym, const char *, pic_list);

/* do not return from try block! */

#define pic_try                                 \
  if (pic_push_try(pic))                        \
    do
#define pic_catch                               \
    while (pic_pop_try(pic), 0);                \
  else

bool pic_push_try(pic_state *);
void pic_pop_try(pic_state *);

pic_value pic_raise_continuable(pic_state *, pic_value);
noreturn void pic_raise(pic_state *, pic_value);
noreturn void pic_throw(pic_state *, pic_sym, const char *, pic_list);
noreturn void pic_error(pic_state *, const char *, pic_list);

#if defined(__cplusplus)
}
#endif

#endif
