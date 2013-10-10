#ifndef PICRIN_H__
#define PICRIN_H__

#include "picrin/value.h"

typedef struct {
} pic_state;

pic_state *pic_open();
void pic_close(pic_state *);

#endif
