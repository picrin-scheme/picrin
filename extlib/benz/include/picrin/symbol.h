/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_SYMBOL_H
#define PICRIN_SYMBOL_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_id {
  union {
    struct pic_symbol {
      PIC_OBJECT_HEADER
      struct pic_string *str;
    } sym;
    struct {
      PIC_OBJECT_HEADER
      struct pic_id *id;
      struct pic_env *env;
    } id;
  } u;
};

#define pic_sym_ptr(v) ((pic_sym *)pic_ptr(v))

#define pic_id_p(v) (pic_type(v) == PIC_TT_ID || pic_type(v) == PIC_TT_SYMBOL)
#define pic_id_ptr(v) ((pic_id *)pic_ptr(v))

pic_id *pic_make_identifier(pic_state *, pic_id *, struct pic_env *);

const char *pic_identifier_name(pic_state *, pic_id *);

#if defined(__cplusplus)
}
#endif

#endif
