/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_EXTRA_H
#define PICRIN_EXTRA_H

#if defined(__cplusplus)
extern "C" {
#endif

#if PIC_USE_LIBC
void *pic_default_allocf(void *, void *, size_t);
void pic_default_panicf(pic_state *, const char *, int, pic_value *);
#endif

#if PIC_USE_FILE
pic_value pic_fopen(pic_state *, FILE *, const char *mode);
#endif

#if PIC_USE_ERROR
# define pic_try pic_try_(PIC_GENSYM(jmp))
# define pic_try_(jmp)                                                  \
  do {                                                                  \
    extern PIC_JMPBUF *pic_prepare_try(pic_state *);                    \
    extern void pic_enter_try(pic_state *);                             \
    extern void pic_exit_try(pic_state *);                              \
    extern pic_value pic_abort_try(pic_state *);                        \
    PIC_JMPBUF *jmp = pic_prepare_try(pic);                             \
    if (PIC_SETJMP(*jmp) == 0) {                                        \
      pic_enter_try(pic);
# define pic_catch(e) pic_catch_(e, PIC_GENSYM(label))
# define pic_catch_(e, label)                             \
      pic_exit_try(pic);                                  \
    } else {                                              \
      e = pic_abort_try(pic);                             \
      goto label;                                         \
    }                                                     \
  } while (0);                                            \
  if (0)                                                  \
  label:
#endif

#if defined(__cplusplus)
}
#endif

#endif
