/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_MACRO_H
#define PICRIN_MACRO_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_env {
  PIC_OBJECT_HEADER
  struct pic_dict *map;
  pic_value defer;
  struct pic_env *up;
};

#define pic_env_p(v) (pic_type(v) == PIC_TT_ENV)
#define pic_env_ptr(v) ((struct pic_env *)pic_ptr(v))

bool pic_identifier_p(pic_state *pic, pic_value obj);
bool pic_identifier_eq_p(pic_state *, struct pic_env *, pic_sym *, struct pic_env *, pic_sym *);

struct pic_env *pic_make_env(pic_state *, struct pic_env *);

pic_sym *pic_add_rename(pic_state *, struct pic_env *, pic_sym *);
pic_sym *pic_find_rename(pic_state *, struct pic_env *, pic_sym *);
void pic_put_rename(pic_state *, struct pic_env *, pic_sym *, pic_sym *);

#if defined(__cplusplus)
}
#endif

#endif
