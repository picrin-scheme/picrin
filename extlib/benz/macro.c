/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

KHASH_DEFINE(env, pic_id *, pic_sym *, kh_ptr_hash_func, kh_ptr_hash_equal)

struct pic_env *
pic_make_env(pic_state *pic, struct pic_env *up)
{
  struct pic_env *env;

  assert(up != NULL);

  env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TT_ENV);
  env->up = up;
  env->prefix = NULL;
  kh_init(env, &env->map);
  return env;
}

struct pic_env *
pic_make_topenv(pic_state *pic, pic_str *prefix)
{
  struct pic_env *env;

  env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TT_ENV);
  env->up = NULL;
  env->prefix = prefix;
  kh_init(env, &env->map);
  return env;
}

pic_sym *
pic_add_identifier(pic_state *pic, pic_id *id, struct pic_env *env)
{
  const char *name;
  pic_sym *uid;
  pic_str *str;

  name = pic_identifier_name(pic, id);

  if (env->up == NULL && pic_sym_p(pic_obj_value(id))) { /* toplevel & public */
    str = pic_format(pic, "%s/%s", pic_str_cstr(pic, env->prefix), name);
  } else {
    str = pic_format(pic, ".%s.%d", name, pic->ucnt++);
  }
  uid = pic_intern_str(pic, str);

  return pic_put_identifier(pic, id, uid, env);
}

pic_sym *
pic_put_identifier(pic_state *pic, pic_id *id, pic_sym *uid, struct pic_env *env)
{
  khiter_t it;
  int ret;

  it = kh_put(env, &env->map, id, &ret);
  kh_val(&env->map, it) = uid;

  return uid;
}

pic_sym *
pic_find_identifier(pic_state PIC_UNUSED(*pic), pic_id *id, struct pic_env *env)
{
  khiter_t it;

  it = kh_get(env, &env->map, id);
  if (it == kh_end(&env->map)) {
    return NULL;
  }
  return kh_val(&env->map, it);
}

static pic_sym *
lookup(pic_state *pic, pic_id *id, struct pic_env *env)
{
  pic_sym *uid = NULL;

  while (env != NULL) {
    uid = pic_find_identifier(pic, id, env);
    if (uid != NULL) {
      break;
    }
    env = env->up;
  }
  return uid;
}

pic_sym *
pic_lookup_identifier(pic_state *pic, pic_id *id, struct pic_env *env)
{
  pic_sym *uid;

  while ((uid = lookup(pic, id, env)) == NULL) {
    if (pic_sym_p(pic_obj_value(id))) {
      break;
    }
    env = id->u.id.env;         /* do not overwrite id first */
    id = id->u.id.id;
  }
  if (uid == NULL) {
    while (env->up != NULL) {
      env = env->up;
    }
    uid = pic_add_identifier(pic, id, env);
  }
  return uid;
}
