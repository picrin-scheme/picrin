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

pic_value pic_load(pic_state *, pic_value irep); /* TODO */
void pic_load_native(pic_state *pic, const char *); /* TODO */
pic_value pic_assemble(pic_state *pic, pic_value as);
pic_value pic_execute(pic_state *pic, pic_value irep);
void pic_serialize(pic_state *pic, const char *name, pic_value irep);
pic_value pic_deserialize(pic_state *pic, const unsigned char *bin);

/* for debug */

#if PIC_USE_WRITE
void pic_print_error(pic_state *, pic_value port, pic_value err);
#endif

#if defined(__cplusplus)
}
#endif

#endif
