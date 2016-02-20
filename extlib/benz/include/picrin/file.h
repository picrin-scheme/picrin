#ifndef PICRIN_FILE_H
#define PICRIN_FILE_H

#if defined(__cplusplus)
extern "C" {
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

enum {
  X_READ  = 01,
  X_WRITE = 02,
  X_UNBUF = 04,
  X_EOF   = 010,
  X_ERR   = 020,
  X_LNBUF = 040
};

#define xstdin  (&pic->files[0])
#define xstdout (&pic->files[1])
#define xstderr (&pic->files[2])

xFILE *xfunopen(pic_state *, void *cookie, int (*read)(pic_state *, void *, char *, int), int (*write)(pic_state *, void *, const char *, int), long (*seek)(pic_state *, void *, long, int), int (*close)(pic_state *, void *));
size_t xfread(pic_state *, void *ptr, size_t size, size_t count, xFILE *fp);
size_t xfwrite(pic_state *, const void *ptr, size_t size, size_t count, xFILE *fp);
long xfseek(pic_state *, xFILE *fp, long offset, int whence); /* 0:cur, 1:end, 2:set */
int xfclose(pic_state *, xFILE *fp);

void xclearerr(pic_state *, xFILE *fp);
int xfeof(pic_state *, xFILE *fp);
int xferror(pic_state *, xFILE *fp);

int xfputc(pic_state *, int c, xFILE *fp);
int xfgetc(pic_state *, xFILE *fp);
int xfputs(pic_state *, const char *s, xFILE *fp);
char *xfgets(pic_state *, char *s, int size, xFILE *fp);
int xungetc(pic_state *, int c, xFILE *fp);
int xfflush(pic_state *, xFILE *fp);
int xfprintf(pic_state *, xFILE *fp, const char *fmt, ...);
int xvfprintf(pic_state *, xFILE *fp, const char *fmt, va_list);

#if PIC_ENABLE_STDIO
xFILE *xfopen_file(pic_state *, FILE *, const char *mode);
#endif
xFILE *xfopen_buf(pic_state *, const char *buf, int len, const char *mode);
int xfget_buf(pic_state *, xFILE *file, const char **buf, int *len);
xFILE *xfopen_null(pic_state *, const char *mode);

#if defined(__cplusplus)
}
#endif

#endif
