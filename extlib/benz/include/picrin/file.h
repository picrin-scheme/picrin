#ifndef PICRIN_FILE_H
#define PICRIN_FILE_H

#if defined(__cplusplus)
extern "C" {
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
    int (*read)(pic_state *, void *, char *, int);
    int (*write)(pic_state *, void *, const char *, int);
    long (*seek)(pic_state *, void *, long, int);
    int (*close)(pic_state *, void *);
  } vtable;
  int flag;                     /* mode of the file access */
} xFILE;

#define xstdin  (&pic->files[0])
#define xstdout (&pic->files[1])
#define xstderr (&pic->files[2])

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

#define xgetc(pic, p)                                           \
  ((--(p)->cnt >= 0)                                            \
   ? (unsigned char) *(p)->ptr++                                \
   : x_fillbuf((pic), p))
#define xputc(pic, x, p)                                        \
  ((--(p)->cnt >= 0 && !(((p)->flag & X_LNBUF) && (x) == '\n')) \
   ? *(p)->ptr++ = (x)                                          \
   : x_flushbuf((pic), (x), (p)))
#define xgetchar(pic)   xgetc((pic), xstdin)
#define xputchar(pic, x)  xputc((pic), (x), xstdout)

/* resource aquisition */
xFILE *xfunopen(pic_state *, void *cookie, int (*read)(pic_state *, void *, char *, int), int (*write)(pic_state *, void *, const char *, int), long (*seek)(pic_state *, void *, long, int), int (*close)(pic_state *, void *));
int xfclose(pic_state *, xFILE *);

#if PIC_ENABLE_STDIO
xFILE *xfopen_file(pic_state *, FILE *, const char *mode);
#endif
xFILE *xfopen_buf(pic_state *, const char *buf, int len, const char *mode);
int xfget_buf(pic_state *, xFILE *file, const char **buf, int *len);
xFILE *xfopen_null(pic_state *, const char *mode);

/* buffer management */
int x_fillbuf(pic_state *, xFILE *);
int x_flushbuf(pic_state *, int, xFILE *);
int xfflush(pic_state *, xFILE *);

/* direct IO */
size_t xfread(pic_state *, void *, size_t, size_t, xFILE *);
size_t xfwrite(pic_state *, const void *, size_t, size_t, xFILE *);

enum {
  XSEEK_CUR,
  XSEEK_END,
  XSEEK_SET
};

/* indicator positioning */
long xfseek(pic_state *, xFILE *, long, int);
long xftell(pic_state *, xFILE *);
void xrewind(pic_state *, xFILE *);

/* character IO */
int xfputc(pic_state *, int, xFILE *);
int xfgetc(pic_state *, xFILE *);
int xfputs(pic_state *, const char *, xFILE *);
char *xfgets(pic_state *, char *, int, xFILE *);
int xputs(pic_state *, const char *);
int xungetc(int, xFILE *);

/* formatted I/O */
int xprintf(pic_state *, const char *, ...);
int xfprintf(pic_state *, xFILE *, const char *, ...);
int xvfprintf(pic_state *, xFILE *, const char *, va_list);

#if defined(__cplusplus)
}
#endif

#endif
