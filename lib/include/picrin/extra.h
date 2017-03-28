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
#endif

pic_value pic_read(pic_state *, pic_value port);
pic_value pic_read_cstr(pic_state *, const char *);

pic_value pic_expand(pic_state *, pic_value program, pic_value env);
pic_value pic_eval(pic_state *, pic_value program, const char *lib);

void pic_load(pic_state *, pic_value port);
void pic_load_cstr(pic_state *, const char *);

#if PIC_USE_STDIO
pic_value pic_fopen(pic_state *, FILE *, const char *mode);
#endif

#define pic_deflibrary(pic, lib) do {           \
    if (! pic_find_library(pic, lib)) {         \
      pic_make_library(pic, lib);               \
    }                                           \
    pic_in_library(pic, lib);                   \
  } while (0)

#define pic_try pic_try_(PIC_GENSYM(cont), PIC_GENSYM(jmp))
#define pic_try_(cont, jmp)                                             \
  do {                                                                  \
    extern pic_value pic_start_try(pic_state *, PIC_JMPBUF *);          \
    extern void pic_end_try(pic_state *, pic_value);                    \
    extern pic_value pic_err(pic_state *);                              \
    PIC_JMPBUF jmp;                                                     \
    if (PIC_SETJMP(pic, jmp) == 0) {                                    \
      pic_value pic_try_cookie_ = pic_start_try(pic, &jmp);
#define pic_catch(e) pic_catch_(e, PIC_GENSYM(label))
#define pic_catch_(e, label)                              \
      pic_end_try(pic, pic_try_cookie_);                  \
    } else {                                              \
      e = pic_err(pic);                                   \
      goto label;                                         \
    }                                                     \
  } while (0);                                            \
  if (0)                                                  \
  label:

/* for debug */

#if PIC_USE_WRITE
void pic_print_error(pic_state *, pic_value port, pic_value err);
#endif

#if defined(__cplusplus)
}
#endif

#endif
