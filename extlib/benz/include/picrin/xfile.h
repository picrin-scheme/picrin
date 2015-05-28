#ifndef XFILE_H
#define XFILE_H

#if defined(__cplusplus)
extern "C" {
#endif

#include <stdio.h>

#ifndef NULL
# define NULL 0
#endif

#ifndef EOF
# define EOF (-1)
#endif

#define XBUFSIZ 1024
#define XOPEN_MAX 1024

typedef struct {
  /* buffer */
  char buf[1];                  /* fallback buffer */
  long cnt;                     /* characters left */
  char *ptr;                    /* next character position */
  char *base;                   /* location of the buffer */
  /* operators */
  struct {
    void *cookie;
    int (*read)(void *, char *, int);
    int (*write)(void *, const char *, int);
    long (*seek)(void *, long, int);
    int (*close)(void *);
  } vtable;
  int flag;                     /* mode of the file access */
} xFILE;

extern xFILE x_iob[XOPEN_MAX];

#define xstdin  (x_iob[0].vtable.cookie || (x_iob[0].vtable.cookie = stdin ), &x_iob[0])
#define xstdout (x_iob[1].vtable.cookie || (x_iob[1].vtable.cookie = stdout), &x_iob[1])
#define xstderr (x_iob[2].vtable.cookie || (x_iob[2].vtable.cookie = stderr), &x_iob[2])

enum _flags {
  X_READ  = 01,
  X_WRITE = 02,
  X_UNBUF = 04,
  X_EOF   = 010,
  X_ERR   = 020,
  X_LNBUF = 040
};

#define xclearerr(p) ((p)->flag &= ~(X_EOF | X_ERR))
#define xfeof(p)     (((p)->flag & X_EOF) != 0)
#define xferror(p)   (((p)->flag & X_ERR) != 0)
#define xfileno(p)   ((p)->fd)

#define xgetc(p)                                                \
  ((--(p)->cnt >= 0)                                            \
   ? (unsigned char) *(p)->ptr++                                \
   : x_fillbuf(p))
#define xputc(x, p)                                             \
  ((--(p)->cnt >= 0 && !(((p)->flag & X_LNBUF) && (x) == '\n')) \
   ? *(p)->ptr++ = (x)                                          \
   : x_flushbuf(x, (p)))
#define xgetchar()   xgetc(xstdin)
#define xputchar(x)  xputc((x), xstdout)

/* resource aquisition */
xFILE *xfunopen(void *cookie, int (*read)(void *, char *, int), int (*write)(void *, const char *, int), long (*seek)(void *, long, int), int (*close)(void *));
xFILE *xfopen(const char *, const char *);
int xfclose(xFILE *);

/* buffer management */
int x_fillbuf(xFILE *);
int x_flushbuf(int, xFILE *);
int xfflush(xFILE *);

/* direct IO */
size_t xfread(void *, size_t, size_t, xFILE *);
size_t xfwrite(const void *, size_t, size_t, xFILE *);

enum {
  XSEEK_CUR,
  XSEEK_END,
  XSEEK_SET
};

/* indicator positioning */
long xfseek(xFILE *, long, int);
long xftell(xFILE *);
void xrewind(xFILE *);

/* character IO */
int xfputc(int, xFILE *);
int xfgetc(xFILE *);
int xfputs(const char *, xFILE *);
char *xfgets(char *, int, xFILE *);
int xputs(const char *);
int xungetc(int, xFILE *);

/* formatted I/O */
int xprintf(const char *, ...);
int xfprintf(xFILE *, const char *, ...);
int xvfprintf(xFILE *, const char *, va_list);

#if defined(__cplusplus)
}
#endif

#endif
