/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "../object.h"
#include "../state.h"

KHASH_DEFINE(env, struct identifier *, symbol *, kh_ptr_hash_func, kh_ptr_hash_equal)

pic_value
pic_make_env(pic_state *pic, pic_value prefix)
{
  struct env *env;

  env = (struct env *)pic_obj_alloc(pic, sizeof(struct env), PIC_TYPE_ENV);
  env->up = NULL;
  env->prefix = pic_str_ptr(pic, prefix);
  kh_init(env, &env->map);

  return obj_value(pic, env);
}

static pic_value
default_env(pic_state *pic)
{
  return pic_ref(pic, "default-environment");
}

static pic_value
extend_env(pic_state *pic, pic_value up)
{
  struct env *env;

  env = (struct env *)pic_obj_alloc(pic, sizeof(struct env), PIC_TYPE_ENV);
  env->up = pic_env_ptr(pic, up);
  env->prefix = NULL;
  kh_init(env, &env->map);

  return obj_value(pic, env);
}

static bool
search_scope(pic_state *pic, pic_value id, pic_value env, pic_value *uid)
{
  int it;

  it = kh_get(env, &pic_env_ptr(pic, env)->map, pic_id_ptr(pic, id));
  if (it == kh_end(&pic_env_ptr(pic, env)->map)) {
    return false;
  }
  *uid = obj_value(pic, kh_val(&pic_env_ptr(pic, env)->map, it));
  return true;
}

static bool
search(pic_state *pic, pic_value id, pic_value env, pic_value *uid)
{
  struct env *e;

  while (1) {
    if (search_scope(pic, id, env, uid))
      return true;
    e = pic_env_ptr(pic, env)->up;
    if (e == NULL)
      break;
    env = obj_value(pic, e);
  }
  return false;
}

pic_value
pic_find_identifier(pic_state *pic, pic_value id, pic_value env)
{
  struct env *e;
  pic_value uid;

  while (! search(pic, id, env, &uid)) {
    if (pic_sym_p(pic, id)) {
      while (1) {
        e = pic_env_ptr(pic, env);
        if (e->up == NULL)
          break;
        env = obj_value(pic, e->up);
      }
      return pic_add_identifier(pic, id, env);
    }
    env = obj_value(pic, pic_id_ptr(pic, id)->env); /* do not overwrite id first */
    id = obj_value(pic, pic_id_ptr(pic, id)->u.id);
  }
  return uid;
}

pic_value
pic_add_identifier(pic_state *pic, pic_value id, pic_value env)
{
  const char *name, *prefix;
  pic_value uid, str;

  if (search_scope(pic, id, env, &uid)) {
    return uid;
  }

  name = pic_str(pic, pic_id_name(pic, id), NULL);

  if (pic_env_ptr(pic, env)->up == NULL && pic_sym_p(pic, id)) {
    prefix = pic_str(pic, obj_value(pic, pic_env_ptr(pic, env)->prefix), NULL);
    str = pic_strf_value(pic, "%s%s", prefix, name);
  } else {
    str = pic_strf_value(pic, ".%s.%d", name, pic->ucnt++);
  }
  uid = pic_intern(pic, str);

  pic_set_identifier(pic, id, uid, env);

  return uid;
}

void
pic_set_identifier(pic_state *pic, pic_value id, pic_value uid, pic_value env)
{
  int it, ret;
  it = kh_put(env, &pic_env_ptr(pic, env)->map, pic_id_ptr(pic, id), &ret);
  kh_val(&pic_env_ptr(pic, env)->map, it) = pic_sym_ptr(pic, uid);
}

#define EQ(sym, lit) (strcmp(pic_sym(pic, sym), lit) == 0)
#define S(lit) (pic_intern_lit(pic, lit))

#define pic_sym(pic,sym) pic_str(pic, pic_sym_name(pic, (sym)), NULL)

static void
define_macro(pic_state *pic, pic_value uid, pic_value mac)
{
  if (pic_weak_has(pic, pic->macros, uid)) {
    pic_warnf(pic, "redefining syntax variable: %s", pic_sym(pic, uid));
  }
  pic_weak_set(pic, pic->macros, uid, mac);
}

static bool
find_macro(pic_state *pic, pic_value uid, pic_value *mac)
{
  if (! pic_weak_has(pic, pic->macros, uid)) {
    return false;
  }
  *mac = pic_weak_ref(pic, pic->macros, uid);
  return true;
}

static void
shadow_macro(pic_state *pic, pic_value uid)
{
  if (pic_weak_has(pic, pic->macros, uid)) {
    pic_weak_del(pic, pic->macros, uid);
  }
}

static pic_value expand(pic_state *, pic_value expr, pic_value env, pic_value deferred);
static pic_value expand_lambda(pic_state *, pic_value expr, pic_value env);

static pic_value
expand_var(pic_state *pic, pic_value id, pic_value env, pic_value deferred)
{
  pic_value mac, functor;

  functor = pic_find_identifier(pic, id, env);

  if (find_macro(pic, functor, &mac)) {
    return expand(pic, pic_call(pic, mac, 2, id, env), env, deferred);
  }
  return functor;
}

static pic_value
expand_quote(pic_state *pic, pic_value expr)
{
  return pic_cons(pic, S("core#quote"), pic_cdr(pic, expr));
}

static pic_value
expand_list(pic_state *pic, pic_value obj, pic_value env, pic_value deferred)
{
  size_t ai = pic_enter(pic);
  pic_value x, head, tail;

  if (pic_pair_p(pic, obj)) {
    head = expand(pic, pic_car(pic, obj), env, deferred);
    tail = expand_list(pic, pic_cdr(pic, obj), env, deferred);
    x = pic_cons(pic, head, tail);
  } else {
    x = expand(pic, obj, env, deferred);
  }

  pic_leave(pic, ai);
  pic_protect(pic, x);
  return x;
}

static pic_value
expand_defer(pic_state *pic, pic_value expr, pic_value deferred)
{
  pic_value skel = pic_cons(pic, pic_invalid_value(pic), pic_invalid_value(pic));

  pic_set_car(pic, deferred, pic_cons(pic, pic_cons(pic, expr, skel), pic_car(pic, deferred)));

  return skel;
}

static void
expand_deferred(pic_state *pic, pic_value deferred, pic_value env)
{
  pic_value defer, val, src, dst, it;

  deferred = pic_car(pic, deferred);

  pic_for_each (defer, pic_reverse(pic, deferred), it) {
    src = pic_car(pic, defer);
    dst = pic_cdr(pic, defer);

    val = expand_lambda(pic, src, env);

    /* copy */
    pic_set_car(pic, dst, pic_car(pic, val));
    pic_set_cdr(pic, dst, pic_cdr(pic, val));
  }
}

static pic_value
expand_lambda(pic_state *pic, pic_value expr, pic_value env)
{
  pic_value formal, body;
  pic_value in;
  pic_value a, deferred;

  in = extend_env(pic, env);

  for (a = pic_cadr(pic, expr); pic_pair_p(pic, a); a = pic_cdr(pic, a)) {
    pic_add_identifier(pic, pic_car(pic, a), in);
  }
  if (pic_id_p(pic, a)) {
    pic_add_identifier(pic, a, in);
  }

  deferred = pic_list(pic, 1, pic_nil_value(pic));

  formal = expand_list(pic, pic_list_ref(pic, expr, 1), in, deferred);
  body = expand(pic, pic_list_ref(pic, expr, 2), in, deferred);

  expand_deferred(pic, deferred, in);

  return pic_list(pic, 3, S("core#lambda"), formal, body);
}

static pic_value
expand_define(pic_state *pic, pic_value expr, pic_value env, pic_value deferred)
{
  pic_value uid, val;

  uid = pic_add_identifier(pic, pic_list_ref(pic, expr, 1), env);

  shadow_macro(pic, uid);

  val = expand(pic, pic_list_ref(pic, expr, 2), env, deferred);

  return pic_list(pic, 3, S("core#define"), uid, val);
}

static pic_value
expand_defmacro(pic_state *pic, pic_value expr, pic_value env)
{
  pic_value uid, val;

  uid = pic_add_identifier(pic, pic_list_ref(pic, expr, 1), env);

  val = pic_load(pic, pic_compile(pic, pic_list_ref(pic, expr, 2), env));
  if (! pic_proc_p(pic, val)) {
    pic_error(pic, "macro definition evaluates to non-procedure object", 1, pic_list_ref(pic, expr, 1));
  }

  define_macro(pic, uid, val);

  return pic_undef_value(pic);
}

static pic_value
expand_node(pic_state *pic, pic_value expr, pic_value env, pic_value deferred)
{
  switch (pic_type(pic, expr)) {
  case PIC_TYPE_ID:
  case PIC_TYPE_SYMBOL: {
    return expand_var(pic, expr, env, deferred);
  }
  case PIC_TYPE_PAIR: {
    pic_value mac;

    if (! pic_list_p(pic, expr)) {
      pic_error(pic, "cannot expand improper list", 1, expr);
    }

    if (pic_id_p(pic, pic_car(pic, expr))) {
      pic_value functor;

      functor = pic_find_identifier(pic, pic_car(pic, expr), env);

      if (EQ(functor, "core#define-macro")) {
        return expand_defmacro(pic, expr, env);
      }
      else if (EQ(functor, "core#lambda")) {
        return expand_defer(pic, expr, deferred);
      }
      else if (EQ(functor, "core#define")) {
        return expand_define(pic, expr, env, deferred);
      }
      else if (EQ(functor, "core#quote")) {
        return expand_quote(pic, expr);
      }

      if (find_macro(pic, functor, &mac)) {
        return expand(pic, pic_call(pic, mac, 2, expr, env), env, deferred);
      }
    }
    return expand_list(pic, expr, env, deferred);
  }
  default:
    return expr;
  }
}

static pic_value
expand(pic_state *pic, pic_value expr, pic_value env, pic_value deferred)
{
  size_t ai = pic_enter(pic);
  pic_value v;

  v = expand_node(pic, expr, env, deferred);

  pic_leave(pic, ai);
  pic_protect(pic, v);
  return v;
}

pic_value
pic_compile(pic_state *pic, pic_value expr, pic_value env)
{
  pic_value v, deferred;

  deferred = pic_list(pic, 1, pic_nil_value(pic));

  v = expand(pic, expr, env, deferred);

  expand_deferred(pic, deferred, env);

  return v;
}

static pic_value
pic_compile_make_environment(pic_state *pic)
{
  pic_value name;

  pic_get_args(pic, "m", &name);

  return pic_make_env(pic, pic_sym_name(pic, name));
}

static pic_value
pic_compile_set_identifier(pic_state *pic)
{
  pic_value id, uid, env;

  pic_get_args(pic, "omo", &id, &uid, &env);

  TYPE_CHECK(pic, id, id);
  TYPE_CHECK(pic, env, env);

  pic_set_identifier(pic, id, uid, env);
  return pic_undef_value(pic);
}

static pic_value
pic_compile_find_identifier(pic_state *pic)
{
  pic_value id, env;

  pic_get_args(pic, "oo", &id, &env);

  TYPE_CHECK(pic, id, id);
  TYPE_CHECK(pic, env, env);

  return pic_find_identifier(pic, id, env);
}

static pic_value
pic_compile_add_macro(pic_state *pic)
{
  pic_value id, mac, uid;

  pic_get_args(pic, "ol", &id, &mac);

  TYPE_CHECK(pic, id, id);

  uid = pic_find_identifier(pic, id, default_env(pic));
  define_macro(pic, uid, mac);
  return pic_undef_value(pic);
}

static pic_value
pic_compile_compile(pic_state *pic)
{
  pic_value program, env = default_env(pic);

  pic_get_args(pic, "o|o", &program, &env);

  TYPE_CHECK(pic, env, env);

  return pic_compile(pic, program, env);
}

static pic_value
pic_compile_eval(pic_state *pic)
{
  pic_value program, env = default_env(pic);

  pic_get_args(pic, "o|o", &program, &env);

  TYPE_CHECK(pic, env, env);

  return pic_load(pic, pic_compile(pic, program, env));
}

#define add_keyword(name) do {                  \
    pic_value var;                              \
    var = pic_intern_lit(pic, name);            \
    pic_set_identifier(pic, var, var, env);     \
  } while (0)

void
pic_init_compile(pic_state *pic)
{
  pic_value env = pic_make_env(pic, pic_lit_value(pic, ""));
  add_keyword("core#define");
  add_keyword("core#set!");
  add_keyword("core#quote");
  add_keyword("core#lambda");
  add_keyword("core#if");
  add_keyword("core#begin");
  add_keyword("core#define-macro");
  pic_define(pic, "default-environment", env);
  pic_defun(pic, "make-environment", pic_compile_make_environment);
  pic_defun(pic, "find-identifier", pic_compile_find_identifier);
  pic_defun(pic, "set-identifier!", pic_compile_set_identifier);
  pic_defun(pic, "add-macro!", pic_compile_add_macro);
  pic_defun(pic, "compile", pic_compile_compile);
  pic_defun(pic, "eval", pic_compile_eval);
}
