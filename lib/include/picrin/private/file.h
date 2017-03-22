#ifndef PICRIN_FILE_H
#define PICRIN_FILE_H

#if defined(__cplusplus)
extern "C" {
#endif

struct file {
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
  FILE_READ  = 01,
  FILE_WRITE = 02,
  FILE_UNBUF = 04,
  FILE_EOF   = 010,
  FILE_ERR   = 020,
  FILE_LNBUF = 040
};


#if defined(__cplusplus)
}
#endif

#endif
