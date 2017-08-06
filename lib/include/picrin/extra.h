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

#if PIC_USE_PORT
typedef struct {
  int (*read)(pic_state *, void *, char *, int);
  int (*write)(pic_state *, void *, const char *, int);
  long (*seek)(pic_state *, void *, long, int);
  int (*close)(pic_state *, void *);
} pic_port_type;

#ifndef EOF
# define EOF (-1)
#endif

#define PIC_SEEK_CUR 0
#define PIC_SEEK_END 1
#define PIC_SEEK_SET 2

#define PIC_IONBF 0
#define PIC_IOLBF 1
#define PIC_IOFBF 2

#define pic_stdin(pic) pic_funcall(pic, "current-input-port", 0)
#define pic_stdout(pic) pic_funcall(pic, "current-output-port", 0)
#define pic_stderr(pic) pic_funcall(pic, "current-error-port", 0)
bool pic_eof_p(pic_state *, pic_value);
pic_value pic_eof_object(pic_state *);
bool pic_port_p(pic_state *, pic_value, const pic_port_type *type);
/* basic methods */
pic_value pic_funopen(pic_state *, void *cookie, const pic_port_type *type);
pic_value pic_fmemopen(pic_state *pic, const char *data, int size, const char *mode);
size_t pic_fread(pic_state *, void *ptr, size_t size, size_t count, pic_value port);
size_t pic_fwrite(pic_state *, const void *ptr, size_t size, size_t count, pic_value port);
long pic_fseek(pic_state *, pic_value port, long offset, int whence);
int pic_fclose(pic_state *, pic_value port);
/* error handling */
void pic_clearerr(pic_state *, pic_value port);
int pic_feof(pic_state *, pic_value port);
int pic_ferror(pic_state *, pic_value port);
/* character I/O */
int pic_fputc(pic_state *, int c, pic_value port);
int pic_fgetc(pic_state *, pic_value port);
int pic_fputs(pic_state *, const char *s, pic_value port);
char *pic_fgets(pic_state *, char *s, int size, pic_value port);
int pic_ungetc(pic_state *, int c, pic_value port);
int pic_fflush(pic_state *, pic_value port);
int pic_setvbuf(pic_state *, pic_value port, char *buf, int mode, size_t size);
/* formatted output */
int pic_printf(pic_state *, const char *fmt, ...);
int pic_fprintf(pic_state *, pic_value port, const char *fmt, ...);
int pic_vfprintf(pic_state *, pic_value port, const char *fmt, va_list ap);
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
