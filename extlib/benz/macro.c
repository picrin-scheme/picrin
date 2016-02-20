/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/object.h"

KHASH_DEFINE(env, pic_id *, pic_sym *, kh_ptr_hash_func, kh_ptr_hash_equal)

pic_value
pic_make_env(pic_state *pic, pic_value up)
{
  struct pic_env *env;

  env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TYPE_ENV);
  env->up = pic_env_ptr(pic, up);
  env->lib = NULL;
  kh_init(env, &env->map);

  return pic_obj_value(env);
}

pic_value
pic_add_identifier(pic_state *pic, pic_value id, pic_value env)
{
  const char *name;
  pic_value uid, str;

  name = pic_str(pic, pic_id_name(pic, id));

  if (pic_env_ptr(pic, env)->up == NULL && pic_sym_p(pic, id)) { /* toplevel & public */
    str = pic_strf_value(pic, "~a/%s", pic_obj_value(pic_env_ptr(pic, env)->lib), name);
  } else {
    str = pic_strf_value(pic, ".%s.%d", name, pic->ucnt++);
  }
  uid = pic_intern(pic, str);

  return pic_put_identifier(pic, id, uid, env);
}

pic_value
pic_put_identifier(pic_state *pic, pic_value id, pic_value uid, pic_value env)
{
  khiter_t it;
  int ret;

  it = kh_put(env, &pic_env_ptr(pic, env)->map, pic_id_ptr(pic, id), &ret);
  kh_val(&pic_env_ptr(pic, env)->map, it) = pic_sym_ptr(pic, uid);

  return uid;
}

static bool
search_scope(pic_state *pic, pic_value id, pic_value env, pic_value *uid)
{
  khiter_t it;

  it = kh_get(env, &pic_env_ptr(pic, env)->map, pic_id_ptr(pic, id));
  if (it == kh_end(&pic_env_ptr(pic, env)->map)) {
    return false;
  }
  *uid = pic_obj_value(kh_val(&pic_env_ptr(pic, env)->map, it));
  return true;
}

static bool
search(pic_state *pic, pic_value id, pic_value env, pic_value *uid)
{
  struct pic_env *e;

  while (1) {
    if (search_scope(pic, id, env, uid))
      return true;
    e = pic_env_ptr(pic, env)->up;
    if (e == NULL)
      break;
    env = pic_obj_value(e);
  }
  return false;
}

pic_value
pic_find_identifier(pic_state *pic, pic_value id, pic_value env)
{
  struct pic_env *e;
  pic_value uid;

  while (! search(pic, id, env, &uid)) {
    if (pic_sym_p(pic, id)) {
      while (1) {
        e = pic_env_ptr(pic, env);
        if (e->up == NULL)
          break;
        env = pic_obj_value(e->up);
      }
      return pic_add_identifier(pic, id, env);
    }
    env = pic_obj_value(pic_id_ptr(pic, id)->env); /* do not overwrite id first */
    id = pic_obj_value(pic_id_ptr(pic, id)->u.id);
  }
  return uid;
}


/**
 * macro expander
 */


static void
define_macro(pic_state *pic, pic_value uid, pic_value mac)
{
  if (pic_weak_has(pic, pic->macros, uid)) {
    pic_warnf(pic, "redefining syntax variable: ~s", uid);
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
  return pic_cons(pic, pic->sQUOTE, pic_cdr(pic, expr));
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

  in = pic_make_env(pic, env);

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

  return pic_list(pic, 3, pic->sLAMBDA, formal, body);
}

static pic_value
expand_define(pic_state *pic, pic_value expr, pic_value env, pic_value deferred)
{
  pic_value id, uid, val;

  id = pic_cadr(pic, expr);
  if (! search_scope(pic, id, env, &uid)) {
    uid = pic_add_identifier(pic, id, env);
  } else {
    shadow_macro(pic, uid);
  }
  val = expand(pic, pic_list_ref(pic, expr, 2), env, deferred);

  return pic_list(pic, 3, pic->sDEFINE, uid, val);
}

static pic_value
expand_defmacro(pic_state *pic, pic_value expr, pic_value env)
{
  pic_value pic_compile(pic_state *, pic_value);
  pic_value id, uid, val;

  id = pic_cadr(pic, expr);
  if (! search_scope(pic, id, env, &uid)) {
    uid = pic_add_identifier(pic, id, env);
  }

  val = pic_call(pic, pic_compile(pic, pic_expand(pic, pic_list_ref(pic, expr, 2), env)), 0);
  if (! pic_proc_p(pic, val)) {
    pic_errorf(pic, "macro definition \"~s\" evaluates to non-procedure object", id);
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
      pic_errorf(pic, "cannot expand improper list: ~s", expr);
    }

    if (pic_id_p(pic, pic_car(pic, expr))) {
      pic_value functor;

      functor = pic_find_identifier(pic, pic_car(pic, expr), env);

      if (pic_eq_p(pic, functor, pic->sDEFINE_MACRO)) {
        return expand_defmacro(pic, expr, env);
      }
      else if (pic_eq_p(pic, functor, pic->sLAMBDA)) {
        return expand_defer(pic, expr, deferred);
      }
      else if (pic_eq_p(pic, functor, pic->sDEFINE)) {
        return expand_define(pic, expr, env, deferred);
      }
      else if (pic_eq_p(pic, functor, pic->sQUOTE)) {
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
pic_expand(pic_state *pic, pic_value expr, pic_value env)
{
  pic_value v, deferred;

#if DEBUG
  puts("before expand:");
  pic_debug(pic, expr);
  puts("");
#endif

  deferred = pic_list(pic, 1, pic_nil_value(pic));

  v = expand(pic, expr, env, deferred);

  expand_deferred(pic, deferred, env);

#if DEBUG
  puts("after expand:");
  pic_debug(pic, v);
  puts("");
#endif

  return v;
}
