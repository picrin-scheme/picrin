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

#if PIC_USE_FILE
pic_value pic_fopen(pic_state *, FILE *, const char *mode);
#endif

#if defined(__cplusplus)
}
#endif

#endif
