/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/object.h"

KHASH_DEFINE(env, pic_id *, pic_sym *, kh_ptr_hash_func, kh_ptr_hash_equal)

struct pic_env *
pic_make_env(pic_state *pic, struct pic_env *up)
{
  struct pic_env *env;

  assert(up != NULL);

  env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TYPE_ENV);
  env->up = up;
  env->lib = NULL;
  kh_init(env, &env->map);
  return env;
}

struct pic_env *
pic_make_topenv(pic_state *pic, struct pic_string *lib)
{
  struct pic_env *env;

  env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TYPE_ENV);
  env->up = NULL;
  env->lib = lib;
  kh_init(env, &env->map);
  return env;
}

pic_sym *
pic_add_identifier(pic_state *pic, pic_id *id, struct pic_env *env)
{
  const char *name;
  pic_sym *uid;
  struct pic_string *str;

  name = pic_str(pic, pic_id_name(pic, id));

  if (env->up == NULL && pic_sym_p(pic, pic_obj_value(id))) { /* toplevel & public */
    str = pic_strf_value(pic, "%s/%s", pic_str(pic, env->lib), name);
  } else {
    str = pic_strf_value(pic, ".%s.%d", name, pic->ucnt++);
  }
  uid = pic_intern(pic, str);

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
search_scope(pic_state *pic, pic_id *id, struct pic_env *env)
{
  khiter_t it;

  it = kh_get(env, &env->map, id);
  if (it == kh_end(&env->map)) {
    return NULL;
  }
  return kh_val(&env->map, it);
}

static pic_sym *
search(pic_state *pic, pic_id *id, struct pic_env *env)
{
  pic_sym *uid = NULL;

  while (env != NULL) {
    uid = search_scope(pic, id, env);
    if (uid != NULL) {
      break;
    }
    env = env->up;
  }
  return uid;
}

pic_sym *
pic_find_identifier(pic_state *pic, pic_id *id, struct pic_env *env)
{
  pic_sym *uid;

  while ((uid = search(pic, id, env)) == NULL) {
    if (pic_sym_p(pic, pic_obj_value(id))) {
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


/**
 * macro expander
 */


static void
define_macro(pic_state *pic, pic_sym *uid, struct pic_proc *mac)
{
  if (pic_weak_has(pic, pic->macros, uid)) {
    pic_warnf(pic, "redefining syntax variable: ~s", pic_obj_value(uid));
  }
  pic_weak_set(pic, pic->macros, uid, pic_obj_value(mac));
}

static struct pic_proc *
find_macro(pic_state *pic, pic_sym *uid)
{
  if (! pic_weak_has(pic, pic->macros, uid)) {
    return NULL;
  }
  return pic_proc_ptr(pic_weak_ref(pic, pic->macros, uid));
}

static void
shadow_macro(pic_state *pic, pic_sym *uid)
{
  if (pic_weak_has(pic, pic->macros, uid)) {
    pic_weak_del(pic, pic->macros, uid);
  }
}

static pic_value expand(pic_state *, pic_value, struct pic_env *, pic_value);
static pic_value expand_lambda(pic_state *, pic_value, struct pic_env *);

static pic_value
expand_var(pic_state *pic, pic_id *id, struct pic_env *env, pic_value deferred)
{
  struct pic_proc *mac;
  pic_sym *functor;

  functor = pic_find_identifier(pic, id, env);

  if ((mac = find_macro(pic, functor)) != NULL) {
    return expand(pic, pic_call(pic, mac, 2, pic_obj_value(id), pic_obj_value(env)), env, deferred);
  }
  return pic_obj_value(functor);
}

static pic_value
expand_quote(pic_state *pic, pic_value expr)
{
  return pic_cons(pic, pic_obj_value(pic->sQUOTE), pic_cdr(pic, expr));
}

static pic_value
expand_list(pic_state *pic, pic_value obj, struct pic_env *env, pic_value deferred)
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
  pic_value skel = pic_cons(pic, pic_invalid_value(), pic_invalid_value());

  pic_set_car(pic, deferred, pic_cons(pic, pic_cons(pic, expr, skel), pic_car(pic, deferred)));

  return skel;
}

static void
expand_deferred(pic_state *pic, pic_value deferred, struct pic_env *env)
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
expand_lambda(pic_state *pic, pic_value expr, struct pic_env *env)
{
  pic_value formal, body;
  struct pic_env *in;
  pic_value a, deferred;

  in = pic_make_env(pic, env);

  for (a = pic_cadr(pic, expr); pic_pair_p(pic, a); a = pic_cdr(pic, a)) {
    pic_add_identifier(pic, pic_id_ptr(pic_car(pic, a)), in);
  }
  if (pic_id_p(pic, a)) {
    pic_add_identifier(pic, pic_id_ptr(a), in);
  }

  deferred = pic_list(pic, 1, pic_nil_value(pic));

  formal = expand_list(pic, pic_list_ref(pic, expr, 1), in, deferred);
  body = expand(pic, pic_list_ref(pic, expr, 2), in, deferred);

  expand_deferred(pic, deferred, in);

  return pic_list(pic, 3, pic_obj_value(pic->sLAMBDA), formal, body);
}

static pic_value
expand_define(pic_state *pic, pic_value expr, struct pic_env *env, pic_value deferred)
{
  pic_sym *uid;
  pic_id *id;
  pic_value val;

  id = pic_id_ptr(pic_cadr(pic, expr));
  if ((uid = search_scope(pic, id, env)) == NULL) {
    uid = pic_add_identifier(pic, id, env);
  } else {
    shadow_macro(pic, uid);
  }
  val = expand(pic, pic_list_ref(pic, expr, 2), env, deferred);

  return pic_list(pic, 3, pic_obj_value(pic->sDEFINE), pic_obj_value(uid), val);
}

static pic_value
expand_defmacro(pic_state *pic, pic_value expr, struct pic_env *env)
{
  struct pic_proc *pic_compile(pic_state *, pic_value);
  pic_id *id;
  pic_value val;
  pic_sym *uid;

  id = pic_id_ptr(pic_cadr(pic, expr));
  if ((uid = search_scope(pic, id, env)) == NULL) {
    uid = pic_add_identifier(pic, id, env);
  }

  val = pic_call(pic, pic_compile(pic, pic_expand(pic, pic_list_ref(pic, expr, 2), env)), 0);
  if (! pic_proc_p(pic, val)) {
    pic_errorf(pic, "macro definition \"%s\" evaluates to non-procedure object", pic_str(pic, pic_id_name(pic, id)));
  }

  define_macro(pic, uid, pic_proc_ptr(val));

  return pic_undef_value(pic);
}

static pic_value
expand_node(pic_state *pic, pic_value expr, struct pic_env *env, pic_value deferred)
{
  switch (pic_type(pic, expr)) {
  case PIC_TYPE_ID:
  case PIC_TYPE_SYMBOL: {
    return expand_var(pic, pic_id_ptr(expr), env, deferred);
  }
  case PIC_TYPE_PAIR: {
    struct pic_proc *mac;

    if (! pic_list_p(pic, expr)) {
      pic_errorf(pic, "cannot expand improper list: ~s", expr);
    }

    if (pic_id_p(pic, pic_car(pic, expr))) {
      pic_sym *functor;

      functor = pic_find_identifier(pic, pic_id_ptr(pic_car(pic, expr)), env);

      if (functor == pic->sDEFINE_MACRO) {
        return expand_defmacro(pic, expr, env);
      }
      else if (functor == pic->sLAMBDA) {
        return expand_defer(pic, expr, deferred);
      }
      else if (functor == pic->sDEFINE) {
        return expand_define(pic, expr, env, deferred);
      }
      else if (functor == pic->sQUOTE) {
        return expand_quote(pic, expr);
      }

      if ((mac = find_macro(pic, functor)) != NULL) {
        return expand(pic, pic_call(pic, mac, 2, expr, pic_obj_value(env)), env, deferred);
      }
    }
    return expand_list(pic, expr, env, deferred);
  }
  default:
    return expr;
  }
}

static pic_value
expand(pic_state *pic, pic_value expr, struct pic_env *env, pic_value deferred)
{
  size_t ai = pic_enter(pic);
  pic_value v;

  v = expand_node(pic, expr, env, deferred);

  pic_leave(pic, ai);
  pic_protect(pic, v);
  return v;
}

pic_value
pic_expand(pic_state *pic, pic_value expr, struct pic_env *env)
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
