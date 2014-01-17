#ifndef PORT_H__
#define PORT_H__

#include <stdio.h>

#define PIC_UBUFSIZ 3

enum pic_file_flags {
  PIC_FILE_EOF = 1,
};

typedef struct {
  short flags;
  /* buffered IO */
  char *buf;
  int mode;
  int bufsiz;
  char *s, *c, *e;
  /* ungetc buf */
  char ub[PIC_UBUFSIZ];
  int us;
  int ur;
  /* operators */
  struct {
    void *cookie;
    int (*read)(void *, char *, int);
    int (*write)(void *, const char *, int);
    long (*seek)(void *, long, int);
    int (*close)(void *);
  } vtable;
} pic_file;

enum pic_port_flag {
  PIC_PORT_IN = 1,
  PIC_PORT_OUT = 2,
  PIC_PORT_TEXT = 4,
  PIC_PORT_BINARY = 8,
};

enum pic_port_status {
  PIC_PORT_OPEN,
  PIC_PORT_CLOSE,
};

struct pic_port {
  PIC_OBJECT_HEADER
  pic_file *file;
  int flags;
  int status;
};

#define pic_port_p(v) (pic_type(v) == PIC_TT_PORT)
#define pic_port_ptr(v) ((struct pic_port *)pic_ptr(v))

pic_value pic_eof_object();

struct pic_port *pic_stdin(pic_state *);
struct pic_port *pic_stdout(pic_state *);
struct pic_port *pic_stderr(pic_state *);

/* generic file constructor */
pic_file *pic_funopen(void *cookie, int (*read)(void *, char *, int), int (*write)(void *, const char *, int), long (*seek)(void *, long, int), int (*close)(void *));

/* buffering */
int pic_setvbuf(pic_file *, char *, int, size_t);
int pic_fflush(pic_file *);
int pic_ffill(pic_file *);

/* resource aquisition */
pic_file *pic_fopen(const char *, const char *);
pic_file *pic_mopen(const char *, size_t, const char *);
int pic_fclose(pic_file *);

/* direct IO with buffering */
size_t pic_fread(void *, size_t, size_t, pic_file *);
size_t pic_fwrite(const void *, size_t, size_t, pic_file *);

/* character IO */
int pic_fgetc(pic_file *);
int pic_ungetc(int, pic_file *);
int pic_fputc(int, pic_file *);
int pic_fputs(const char *, pic_file *);

#endif
