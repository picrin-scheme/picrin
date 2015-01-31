#ifndef XFILE_H
#define XFILE_H

#if defined(__cplusplus)
extern "C" {
#endif

#define EOF (-1)

typedef struct {
  int ungot;
  int err;
  /* operators */
  struct {
    void *cookie;
    int (*read)(void *, char *, int);
    int (*write)(void *, const char *, int);
    long (*seek)(void *, long, int);
    int (*flush)(void *);
    int (*close)(void *);
  } vtable;
} xFILE;

enum {
  XF_SEEK_SET,
  XF_SEEK_CUR,
  XF_SEEK_END
};

#define XFOPEN_MAX 1024

/* resource aquisition */
xFILE *xfunopen(void *cookie, int (*read)(void *, char *, int), int (*write)(void *, const char *, int), long (*seek)(void *, long, int), int (*flush)(void *), int (*close)(void *));
int xfclose(xFILE *);

/* buffer management */
int xfflush(xFILE *);

/* direct IO with buffering */
size_t xfread(void *, size_t, size_t, xFILE *);
size_t xfwrite(const void *, size_t, size_t, xFILE *);

/* indicator positioning */
long xfseek(xFILE *, long offset, int whence);
long xftell(xFILE *);
void xrewind(xFILE *);

/* stream status */
void xclearerr(xFILE *);
int xfeof(xFILE *);
int xferror(xFILE *);

/* character IO */
int xfgetc(xFILE *);
char *xfgets(char *, int, xFILE *);
int xfputc(int, xFILE *);
int xfputs(const char *, xFILE *);
int xgetc(xFILE *);
int xputc(int, xFILE *);
int xungetc(int, xFILE *);

/* formatted I/O */
int xfprintf(xFILE *, const char *, ...);
int xvfprintf(xFILE *, const char *, va_list);

#if defined(__cplusplus)
}
#endif

#endif
