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
  pic_str *prefix;
};

#define pic_env_p(v) (pic_type(v) == PIC_TT_ENV)
#define pic_env_ptr(v) ((struct pic_env *)pic_ptr(v))

struct pic_env *pic_make_topenv(pic_state *, pic_str *);
struct pic_env *pic_make_env(pic_state *, struct pic_env *);

pic_sym *pic_add_identifier(pic_state *, pic_id *, struct pic_env *);
pic_sym *pic_put_identifier(pic_state *, pic_id *, pic_sym *, struct pic_env *);
pic_sym *pic_find_identifier(pic_state *, pic_id *, struct pic_env *);
pic_sym *pic_lookup_identifier(pic_state *, pic_id *, struct pic_env *);

#if defined(__cplusplus)
}
#endif

#endif
