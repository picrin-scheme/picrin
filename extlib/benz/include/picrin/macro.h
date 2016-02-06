/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_MACRO_H
#define PICRIN_MACRO_H

#if defined(__cplusplus)
extern "C" {
#endif

KHASH_DECLARE(env, void *, pic_sym *)

struct pic_id {
  PIC_OBJECT_HEADER
  pic_value var;
  struct pic_env *env;
};

struct pic_env {
  PIC_OBJECT_HEADER
  khash_t(env) map;
  struct pic_env *up;
  pic_str *prefix;
};

#define pic_id_p(v) (pic_type(v) == PIC_TT_ID)
#define pic_id_ptr(v) ((struct pic_id *)pic_ptr(v))

#define pic_env_p(v) (pic_type(v) == PIC_TT_ENV)
#define pic_env_ptr(v) ((struct pic_env *)pic_ptr(v))

struct pic_id *pic_make_id(pic_state *, pic_value, struct pic_env *);
struct pic_env *pic_make_topenv(pic_state *, pic_str *);
struct pic_env *pic_make_env(pic_state *, struct pic_env *);

pic_sym *pic_add_variable(pic_state *, struct pic_env *, pic_value);
pic_sym *pic_put_variable(pic_state *, struct pic_env *, pic_value, pic_sym *);
pic_sym *pic_find_variable(pic_state *, struct pic_env *, pic_value);
pic_sym *pic_resolve_variable(pic_state *, struct pic_env *, pic_value);

bool pic_var_p(pic_value);
pic_sym *pic_var_name(pic_state *, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
