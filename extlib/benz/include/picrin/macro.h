/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_MACRO_H
#define PICRIN_MACRO_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_senv {
  PIC_OBJECT_HEADER
  xhash map;
  pic_value defer;
  struct pic_senv *up;
};

#define pic_senv_p(v) (pic_type(v) == PIC_TT_SENV)
#define pic_senv_ptr(v) ((struct pic_senv *)pic_ptr(v))

struct pic_senv *pic_null_syntactic_environment(pic_state *);

bool pic_identifier_p(pic_state *pic, pic_value obj);
bool pic_identifier_eq_p(pic_state *, struct pic_senv *, pic_sym, struct pic_senv *, pic_sym);

struct pic_senv *pic_make_senv(pic_state *, struct pic_senv *);

pic_sym pic_add_rename(pic_state *, struct pic_senv *, pic_sym);
bool pic_find_rename(pic_state *, struct pic_senv *, pic_sym, pic_sym * /* = NULL */);
void pic_put_rename(pic_state *, struct pic_senv *, pic_sym, pic_sym);

void pic_define_syntactic_keyword(pic_state *, struct pic_senv *, pic_sym, pic_sym);

#if defined(__cplusplus)
}
#endif

#endif
