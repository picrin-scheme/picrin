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

void pic_load(pic_state *, pic_value port);
void pic_load_cstr(pic_state *, const char *);

#if PIC_USE_STDIO
pic_value pic_fopen(pic_state *, FILE *, const char *mode);
#endif


/*
 * library
 */

void pic_deflibrary(pic_state *, const char *lib);
void pic_in_library(pic_state *, const char *lib);
void pic_export(pic_state *, int n, ...);


/* for debug */

#if PIC_USE_WRITE
void pic_print_error(pic_state *, pic_value port, pic_value err);
#endif

#if defined(__cplusplus)
}
#endif

#endif
