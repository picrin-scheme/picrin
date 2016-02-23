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

#if PIC_USE_WRITE
void pic_printf(pic_state *, const char *fmt, ...);
void pic_fprintf(pic_state *, pic_value port, const char *fmt, ...);
void pic_vfprintf(pic_state *, pic_value port, const char *fmt, va_list ap);
#endif

/* extra xfile methods */

xFILE *xfile_xstdin(pic_state *);
xFILE *xfile_xstdout(pic_state *);
xFILE *xfile_xstderr(pic_state *);
#define xstdin  (xfile_xstdin(pic))
#define xstdout (xfile_xstdout(pic))
#define xstderr (xfile_xstderr(pic))
#if PIC_USE_STDIO
xFILE *xfopen_file(pic_state *, FILE *, const char *mode);
#endif
xFILE *xfopen_buf(pic_state *, const char *buf, int len, const char *mode);
int xfget_buf(pic_state *, xFILE *file, const char **buf, int *len);
xFILE *xfopen_null(pic_state *, const char *mode);

/* port manipulation */

#define pic_stdin(pic) pic_funcall(pic, "picrin.base", "current-input-port", 0)
#define pic_stdout(pic) pic_funcall(pic, "picrin.base", "current-output-port", 0)
#define pic_stderr(pic) pic_funcall(pic, "picrin.base", "current-error-port", 0)

/* utility macros */

#define pic_for_each(var, list, it)                                     \
  for (it = (list); ! pic_nil_p(pic, it); it = pic_cdr(pic, it))        \
    if ((var = pic_car(pic, it)), true)

#define pic_push(pic, item, place) (place = pic_cons(pic, item, place))
#define pic_pop(pic, place) (place = pic_cdr(pic, place))

#define pic_assert_type(pic, v, type) do {      \
    if (! pic_##type##_p(pic, v))               \
      pic_error(pic, #type " required", 1, v);  \
  } while (0)

#define pic_void(exec) pic_void_(PIC_GENSYM(ai), exec)
#define pic_void_(ai,exec) do {                 \
    size_t ai = pic_enter(pic);                 \
    exec;                                       \
    pic_leave(pic, ai);                         \
  } while (0)

#define pic_deflibrary(pic, lib) do {           \
    if (! pic_find_library(pic, lib)) {         \
      pic_make_library(pic, lib);               \
    }                                           \
    pic_in_library(pic, lib);                   \
  } while (0)

/* for pic_try & pic_catch macros */
struct pic_cont *pic_alloca_cont(pic_state *);
pic_value pic_make_cont(pic_state *, struct pic_cont *);
void pic_push_native_handler(pic_state *, struct pic_cont *);
pic_value pic_pop_handler(pic_state *);
void pic_save_point(pic_state *, struct pic_cont *, PIC_JMPBUF *);
void pic_exit_point(pic_state *);

#define pic_try pic_try_(PIC_GENSYM(cont), PIC_GENSYM(jmp))
#define pic_try_(cont, jmp)                                             \
  do {                                                                  \
    PIC_JMPBUF jmp;                                                     \
    struct pic_cont *cont = pic_alloca_cont(pic);                       \
    if (PIC_SETJMP(pic, jmp) == 0) {                                    \
      pic_save_point(pic, cont, &jmp);                                  \
      pic_push_native_handler(pic, cont);
#define pic_catch pic_catch_(PIC_GENSYM(label))
#define pic_catch_(label)                                 \
      pic_pop_handler(pic);                               \
      pic_exit_point(pic);                                \
    } else {                                              \
      goto label;                                         \
    }                                                     \
  } while (0);                                            \
  if (0)                                                  \
  label:

pic_value pic_err(pic_state *);

/* for debug */

void pic_warnf(pic_state *, const char *, ...);
pic_value pic_get_backtrace(pic_state *);
#if PIC_USE_WRITE
void pic_print_error(pic_state *, xFILE *);
#endif

pic_value pic_library_environment(pic_state *, const char *);

#if defined(__cplusplus)
}
#endif

#endif
