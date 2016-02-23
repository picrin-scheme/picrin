#ifndef PICRIN_FILE_H
#define PICRIN_FILE_H

#if defined(__cplusplus)
extern "C" {
#endif

#define XBUFSIZ 1024
#define XOPEN_MAX 1024

struct xFILE {
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
};

enum {
  X_READ  = 01,
  X_WRITE = 02,
  X_UNBUF = 04,
  X_EOF   = 010,
  X_ERR   = 020,
  X_LNBUF = 040
};


#if defined(__cplusplus)
}
#endif

#endif
