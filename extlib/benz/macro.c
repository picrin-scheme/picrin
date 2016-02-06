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


/**
 * macro expander
 */


static void
define_macro(pic_state *pic, pic_sym *uid, struct pic_proc *mac)
{
  if (pic_reg_has(pic, pic->macros, uid)) {
    pic_warnf(pic, "redefining syntax variable: ~s", pic_obj_value(uid));
  }
  pic_reg_set(pic, pic->macros, uid, pic_obj_value(mac));
}

static struct pic_proc *
find_macro(pic_state *pic, pic_sym *uid)
{
  if (! pic_reg_has(pic, pic->macros, uid)) {
    return NULL;
  }
  return pic_proc_ptr(pic_reg_ref(pic, pic->macros, uid));
}

static void
shadow_macro(pic_state *pic, pic_sym *uid)
{
  if (pic_reg_has(pic, pic->macros, uid)) {
    pic_reg_del(pic, pic->macros, uid);
  }
}

static pic_value expand(pic_state *, pic_value, struct pic_env *, pic_value);
static pic_value expand_lambda(pic_state *, pic_value, struct pic_env *);

static pic_value
expand_var(pic_state *pic, pic_id *id, struct pic_env *env, pic_value deferred)
{
  struct pic_proc *mac;
  pic_sym *functor;

  functor = pic_lookup_identifier(pic, id, env);

  if ((mac = find_macro(pic, functor)) != NULL) {
    return expand(pic, pic_apply2(pic, mac, pic_obj_value(id), pic_obj_value(env)), env, deferred);
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
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value x, head, tail;

  if (pic_pair_p(obj)) {
    head = expand(pic, pic_car(pic, obj), env, deferred);
    tail = expand_list(pic, pic_cdr(pic, obj), env, deferred);
    x = pic_cons(pic, head, tail);
  } else {
    x = expand(pic, obj, env, deferred);
  }

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, x);
  return x;
}

static pic_value
expand_defer(pic_state *pic, pic_value expr, pic_value deferred)
{
  pic_value skel = pic_cons(pic, pic_invalid_value(), pic_invalid_value());

  pic_set_car(pic, deferred, pic_acons(pic, expr, skel, pic_car(pic, deferred)));

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

  for (a = pic_cadr(pic, expr); pic_pair_p(a); a = pic_cdr(pic, a)) {
    pic_add_identifier(pic, pic_id_ptr(pic_car(pic, a)), in);
  }
  if (pic_id_p(a)) {
    pic_add_identifier(pic, pic_id_ptr(a), in);
  }

  deferred = pic_list1(pic, pic_nil_value());

  formal = expand_list(pic, pic_list_ref(pic, expr, 1), in, deferred);
  body = expand(pic, pic_list_ref(pic, expr, 2), in, deferred);

  expand_deferred(pic, deferred, in);

  return pic_list3(pic, pic_obj_value(pic->sLAMBDA), formal, body);
}

static pic_value
expand_define(pic_state *pic, pic_value expr, struct pic_env *env, pic_value deferred)
{
  pic_sym *uid;
  pic_id *id;
  pic_value val;

  id = pic_id_ptr(pic_cadr(pic, expr));
  if ((uid = pic_find_identifier(pic, id, env)) == NULL) {
    uid = pic_add_identifier(pic, id, env);
  } else {
    shadow_macro(pic, uid);
  }
  val = expand(pic, pic_list_ref(pic, expr, 2), env, deferred);

  return pic_list3(pic, pic_obj_value(pic->sDEFINE), pic_obj_value(uid), val);
}

static pic_value
expand_defmacro(pic_state *pic, pic_value expr, struct pic_env *env)
{
  struct pic_proc *pic_compile(pic_state *, pic_value);
  pic_id *id;
  pic_value val;
  pic_sym *uid;

  id = pic_id_ptr(pic_cadr(pic, expr));
  if ((uid = pic_find_identifier(pic, id, env)) == NULL) {
    uid = pic_add_identifier(pic, id, env);
  }

  val = pic_apply0(pic, pic_compile(pic, pic_expand(pic, pic_list_ref(pic, expr, 2), env)));
  if (! pic_proc_p(val)) {
    pic_errorf(pic, "macro definition \"~s\" evaluates to non-procedure object", pic_identifier_name(pic, id));
  }

  define_macro(pic, uid, pic_proc_ptr(val));

  return pic_undef_value();
}

static pic_value
expand_node(pic_state *pic, pic_value expr, struct pic_env *env, pic_value deferred)
{
  switch (pic_type(expr)) {
  case PIC_TT_ID:
  case PIC_TT_SYMBOL: {
    return expand_var(pic, pic_id_ptr(expr), env, deferred);
  }
  case PIC_TT_PAIR: {
    struct pic_proc *mac;

    if (! pic_list_p(expr)) {
      pic_errorf(pic, "cannot expand improper list: ~s", expr);
    }

    if (pic_id_p(pic_car(pic, expr))) {
      pic_sym *functor;

      functor = pic_lookup_identifier(pic, pic_id_ptr(pic_car(pic, expr)), env);

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
        return expand(pic, pic_apply2(pic, mac, expr, pic_obj_value(env)), env, deferred);
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
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value v;

  v = expand_node(pic, expr, env, deferred);

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, v);
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

  deferred = pic_list1(pic, pic_nil_value());

  v = expand(pic, expr, env, deferred);

  expand_deferred(pic, deferred, env);

#if DEBUG
  puts("after expand:");
  pic_debug(pic, v);
  puts("");
#endif

  return v;
}
