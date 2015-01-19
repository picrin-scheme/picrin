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

#if defined(__cplusplus)
}
#endif

#endif
