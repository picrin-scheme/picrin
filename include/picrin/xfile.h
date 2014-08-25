#ifndef XFILE_H__
#define XFILE_H__

#if defined(__cplusplus)
extern "C" {
#endif

#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>

typedef struct {
  int ungot;
  int flags;
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

/* generic file constructor */
xFILE *xfunopen(void *cookie, int (*read)(void *, char *, int), int (*write)(void *, const char *, int), long (*seek)(void *, long, int), int (*flush)(void *), int (*close)(void *));

/* resource aquisition */
xFILE *xfopen(const char *, const char *);
xFILE *xfpopen(FILE *);
xFILE *xmopen();
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
char xgetc(xFILE *);
int xgetchar(void);
int xputc(int, xFILE *);
int xputchar(int);
int xputs(char *);
int xungetc(int, xFILE *);

/* formatted I/O */
int xprintf(const char *, ...);
int xfprintf(xFILE *, const char *, ...);
int xvfprintf(xFILE *, const char *, va_list);

/* standard I/O */
extern xFILE *xstdin;
extern xFILE *xstdout;
extern xFILE *xstderr;

#if defined(__cplusplus)
}
#endif

#endif
