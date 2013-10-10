#ifndef PICRIN_H__
#define PICRIN_H__

#include <stddef.h>

#include "picrin/value.h"

typedef struct {
} pic_state;

void *pic_alloc(pic_state *, size_t);
struct pic_object *pic_gc_alloc(pic_state *, size_t, enum pic_tt);
void pic_free(pic_state *, void *);

pic_state *pic_open();
void pic_close(pic_state *);

pic_value pic_cons(pic_state *, pic_value, pic_value);
pic_value pic_car(pic_state *, pic_value);
pic_value pic_cdr(pic_state *, pic_value);

pic_value pic_intern_cstr(pic_state *, const char *);

void pic_debug(pic_state *, pic_value);

#endif
