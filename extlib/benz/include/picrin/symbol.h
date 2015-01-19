/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_SYMBOL_H
#define PICRIN_SYMBOL_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_symbol {
  PIC_OBJECT_HEADER
  pic_str *str;
};

#define pic_sym(v) pic_ptr(v)
#define pic_sym_p(v) (pic_type(v) == PIC_TT_SYMBOL)

#if defined(__cplusplus)
}
#endif

#endif
