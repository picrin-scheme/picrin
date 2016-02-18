/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_MACRO_H
#define PICRIN_MACRO_H

#if defined(__cplusplus)
extern "C" {
#endif

KHASH_DECLARE(env, pic_id *, pic_sym *)

struct pic_env {
  PIC_OBJECT_HEADER
  khash_t(env) map;
  struct pic_env *up;
  struct pic_string *lib;
};

#define pic_env_p(pic, v) (pic_type(pic, v) == PIC_TT_ENV)
#define pic_env_ptr(v) ((struct pic_env *)pic_ptr(v))

struct pic_env *pic_make_topenv(pic_state *, struct pic_string *);
struct pic_env *pic_make_env(pic_state *, struct pic_env *);

pic_sym *pic_add_identifier(pic_state *, pic_id *, struct pic_env *);
pic_sym *pic_put_identifier(pic_state *, pic_id *, pic_sym *, struct pic_env *);
pic_sym *pic_find_identifier(pic_state *, pic_id *, struct pic_env *);

pic_value pic_expand(pic_state *, pic_value, struct pic_env *);

#if defined(__cplusplus)
}
#endif

#endif
