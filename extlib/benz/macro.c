/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

KHASH_DEFINE(env, void *, pic_sym *, kh_ptr_hash_func, kh_ptr_hash_equal)

bool
pic_var_p(pic_value obj)
{
  return pic_sym_p(obj) || pic_id_p(obj);
}

struct pic_id *
pic_make_id(pic_state *pic, pic_value var, struct pic_env *env)
{
  struct pic_id *id;

  assert(pic_var_p(var));

  id = (struct pic_id *)pic_obj_alloc(pic, sizeof(struct pic_id), PIC_TT_ID);
  id->var = var;
  id->env = env;
  return id;
}

struct pic_env *
pic_make_env(pic_state *pic, struct pic_env *up)
{
  struct pic_env *env;

  env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TT_ENV);
  env->up = up;
  kh_init(env, &env->map);
  return env;
}

pic_sym *
pic_var_name(pic_state PIC_UNUSED(*pic), pic_value var)
{
  assert(pic_var_p(var));

  while (pic_id_p(var)) {
    var = pic_id_ptr(var)->var;
  }
  return pic_sym_ptr(var);
}

pic_sym *
pic_uniq(pic_state *pic, pic_value var)
{
  pic_str *str;

  assert(pic_var_p(var));

  str = pic_format(pic, "%s.%d", pic_symbol_name(pic, pic_var_name(pic, var)), pic->ucnt++);

  return pic_intern_str(pic, str);
}

pic_sym *
pic_add_variable(pic_state *pic, struct pic_env *env, pic_value var)
{
  pic_sym *uid;

  assert(pic_var_p(var));

  uid = pic_uniq(pic, var);

  pic_put_variable(pic, env, var, uid);

  return uid;
}

void
pic_put_variable(pic_state PIC_UNUSED(*pic), struct pic_env *env, pic_value var, pic_sym *uid)
{
  khiter_t it;
  int ret;

  assert(pic_var_p(var));

  it = kh_put(env, &env->map, pic_ptr(var), &ret);
  kh_val(&env->map, it) = uid;
}

pic_sym *
pic_find_variable(pic_state PIC_UNUSED(*pic), struct pic_env *env, pic_value var)
{
  khiter_t it;

  assert(pic_var_p(var));

  it = kh_get(env, &env->map, pic_ptr(var));
  if (it == kh_end(&env->map)) {
    return NULL;
  }
  return kh_val(&env->map, it);
}

static pic_sym *
lookup(void *var, struct pic_env *env)
{
  khiter_t it;

  while (env != NULL) {
    it = kh_get(env, &env->map, var);
    if (it != kh_end(&env->map)) {
      return kh_val(&env->map, it);
    }
    env = env->up;
  }
  return NULL;
}

pic_sym *
pic_resolve(pic_state *pic, pic_value var, struct pic_env *env)
{
  pic_sym *uid;

  assert(env != NULL);

  pic_assert_type(pic, var, var);

  while ((uid = lookup(pic_ptr(var), env)) == NULL) {
    if (pic_sym_p(var)) {
      break;
    }
    env = pic_id_ptr(var)->env;
    var = pic_id_ptr(var)->var;
  }
  if (uid == NULL) {
    while (env->up != NULL) {
      env = env->up;
    }
    uid = pic_add_variable(pic, env, var);
  }
  return uid;
}

static pic_value
pic_macro_identifier_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic_id_p(obj));
}

static pic_value
pic_macro_make_identifier(pic_state *pic)
{
  pic_value var, env;

  pic_get_args(pic, "oo", &var, &env);

  pic_assert_type(pic, var, var);
  pic_assert_type(pic, env, env);

  return pic_obj_value(pic_make_id(pic, var, pic_env_ptr(env)));
}

static pic_value
pic_macro_identifier_variable(pic_state *pic)
{
  pic_value id;

  pic_get_args(pic, "o", &id);

  pic_assert_type(pic, id, id);

  return pic_id_ptr(id)->var;
}

static pic_value
pic_macro_identifier_environment(pic_state *pic)
{
  pic_value id;

  pic_get_args(pic, "o", &id);

  pic_assert_type(pic, id, id);

  return pic_obj_value(pic_id_ptr(id)->env);
}

static pic_value
pic_macro_variable_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic_var_p(obj));
}

static pic_value
pic_macro_variable_eq_p(pic_state *pic)
{
  int argc, i;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    if (! pic_var_p(argv[i])) {
      return pic_false_value();
    }
    if (! pic_equal_p(pic, argv[i], argv[0])) {
      return pic_false_value();
    }
  }
  return pic_true_value();
}

void
pic_init_macro(pic_state *pic)
{
  pic_defun(pic, "make-identifier", pic_macro_make_identifier);
  pic_defun(pic, "identifier?", pic_macro_identifier_p);
  pic_defun(pic, "identifier-variable", pic_macro_identifier_variable);
  pic_defun(pic, "identifier-environment", pic_macro_identifier_environment);

  pic_defun(pic, "variable?", pic_macro_variable_p);
  pic_defun(pic, "variable=?", pic_macro_variable_eq_p);
}
