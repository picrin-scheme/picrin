/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_SYMBOL_H
#define PICRIN_SYMBOL_H

#include "picrin/macro.h"

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_symbol {
  PIC_OBJECT_HEADER
  pic_str *str;
};

#define pic_sym_p(v) (pic_type(v) == PIC_TT_SYMBOL)
#define pic_sym_ptr(v) ((struct pic_symbol *)pic_ptr(v))

pic_sym *pic_make_identifier(pic_state *, pic_sym *, struct pic_senv *);

#if defined(__cplusplus)
}
#endif

#endif
