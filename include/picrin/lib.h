/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_LIB_H
#define PICRIN_LIB_H

#if defined(__cplusplus)
extern "C" {
#endif

/*
 * library
 */

void pic_deflibrary(pic_state *, const char *lib);
void pic_in_library(pic_state *, const char *lib);
void pic_export(pic_state *, int n, ...);

#if defined(__cplusplus)
}
#endif

#endif
