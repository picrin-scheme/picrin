/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

/**
 * macro expander
 */

static pic_sym *
lookup(pic_state PIC_UNUSED(*pic), pic_value var, struct pic_env *env)
{
  khiter_t it;

  pic_assert_type(pic, var, var);

  while (env != NULL) {
    it = kh_get(env, &env->map, pic_ptr(var));
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

  while ((uid = lookup(pic, var, env)) == NULL) {
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

static void
define_macro(pic_state *pic, pic_sym *uid, struct pic_proc *mac)
{
  if (pic_dict_has(pic, pic->macros, uid)) {
    pic_warnf(pic, "redefining syntax variable: ~s", pic_obj_value(uid));
  }
  pic_dict_set(pic, pic->macros, uid, pic_obj_value(mac));
}

static struct pic_proc *
find_macro(pic_state *pic, pic_sym *uid)
{
  if (! pic_dict_has(pic, pic->macros, uid)) {
    return NULL;
  }
  return pic_proc_ptr(pic_dict_ref(pic, pic->macros, uid));
}

static void
shadow_macro(pic_state *pic, pic_sym *uid)
{
  if (pic_dict_has(pic, pic->macros, uid)) {
    pic_dict_del(pic, pic->macros, uid);
  }
}

static pic_value expand(pic_state *, pic_value, struct pic_env *, pic_value);
static pic_value expand_lambda(pic_state *, pic_value, struct pic_env *);

static pic_value
expand_var(pic_state *pic, pic_value var, struct pic_env *env, pic_value deferred)
{
  struct pic_proc *mac;
  pic_sym *functor;

  functor = pic_resolve(pic, var, env);

  if ((mac = find_macro(pic, functor)) != NULL) {
    return expand(pic, pic_apply2(pic, mac, var, pic_obj_value(env)), env, deferred);
  }
  return pic_obj_value(functor);
}

static pic_value
expand_quote(pic_state *pic, pic_value expr)
{
  return pic_cons(pic, pic_obj_value(pic->uQUOTE), pic_cdr(pic, expr));
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
  pic_value skel = pic_list1(pic, pic_invalid_value()); /* (#<invalid>) */

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
    pic_add_variable(pic, in, pic_car(pic, a));
  }
  if (pic_var_p(a)) {
    pic_add_variable(pic, in, a);
  }

  deferred = pic_list1(pic, pic_nil_value());

  formal = expand_list(pic, pic_list_ref(pic, expr, 1), in, deferred);
  body = expand(pic, pic_list_ref(pic, expr, 2), in, deferred);

  expand_deferred(pic, deferred, in);

  return pic_list3(pic, pic_obj_value(pic->uLAMBDA), formal, body);
}

static pic_value
expand_define(pic_state *pic, pic_value expr, struct pic_env *env, pic_value deferred)
{
  pic_sym *uid;
  pic_value var, val;

  var = pic_cadr(pic, expr);
  if ((uid = pic_find_variable(pic, env, var)) == NULL) {
    uid = pic_add_variable(pic, env, var);
  } else {
    shadow_macro(pic, uid);
  }
  val = expand(pic, pic_list_ref(pic, expr, 2), env, deferred);

  return pic_list3(pic, pic_obj_value(pic->uDEFINE), pic_obj_value(uid), val);
}

static pic_value
expand_defmacro(pic_state *pic, pic_value expr, struct pic_env *env)
{
  pic_value var, val;
  pic_sym *uid;

  var = pic_cadr(pic, expr);
  if ((uid = pic_find_variable(pic, env, var)) == NULL) {
    uid = pic_add_variable(pic, env, var);
  }

  val = pic_eval(pic, pic_list_ref(pic, expr, 2), env);
  if (! pic_proc_p(val)) {
    pic_errorf(pic, "macro definition \"~s\" evaluates to non-procedure object", var);
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
    return expand_var(pic, expr, env, deferred);
  }
  case PIC_TT_PAIR: {
    struct pic_proc *mac;

    if (! pic_list_p(expr)) {
      pic_errorf(pic, "cannot expand improper list: ~s", expr);
    }

    if (pic_var_p(pic_car(pic, expr))) {
      pic_sym *functor;

      functor = pic_resolve(pic, pic_car(pic, expr), env);

      if (functor == pic->uDEFINE_MACRO) {
        return expand_defmacro(pic, expr, env);
      }
      else if (functor == pic->uLAMBDA) {
        return expand_defer(pic, expr, deferred);
      }
      else if (functor == pic->uDEFINE) {
        return expand_define(pic, expr, env, deferred);
      }
      else if (functor == pic->uQUOTE) {
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

KHASH_DECLARE(a, pic_sym *, int)
KHASH_DEFINE2(a, pic_sym *, int, 0, kh_ptr_hash_func, kh_ptr_hash_equal)

typedef struct analyze_scope {
  int depth;
  pic_sym *rest;                     /* Nullable */
  khash_t(a) args, locals, captures; /* rest args variable is counted as a local */
  pic_value defer;
  struct analyze_scope *up;
} analyze_scope;

static void
analyzer_scope_init(pic_state *pic, analyze_scope *scope, pic_value formal, analyze_scope *up)
{
  int ret;

  kh_init(a, &scope->args);
  kh_init(a, &scope->locals);
  kh_init(a, &scope->captures);

  /* analyze formal */
  for (; pic_pair_p(formal); formal = pic_cdr(pic, formal)) {
    kh_put(a, &scope->args, pic_sym_ptr(pic_car(pic, formal)), &ret);
  }
  if (pic_nil_p(formal)) {
    scope->rest = NULL;
  }
  else {
    scope->rest = pic_sym_ptr(formal);
    kh_put(a, &scope->locals, pic_sym_ptr(formal), &ret);
  }

  scope->up = up;
  scope->depth = up ? up->depth + 1 : 0;
  scope->defer = pic_nil_value();
}

static void
analyzer_scope_destroy(pic_state *pic, analyze_scope *scope)
{
  kh_destroy(a, &scope->args);
  kh_destroy(a, &scope->locals);
  kh_destroy(a, &scope->captures);
}

static bool
search_scope(analyze_scope *scope, pic_sym *sym)
{
  return kh_get(a, &scope->args, sym) != kh_end(&scope->args) || kh_get(a, &scope->locals, sym) != kh_end(&scope->locals) || scope->depth == 0;
}

static int
find_var(pic_state *pic, analyze_scope *scope, pic_sym *sym)
{
  int depth = 0, ret;

  while (scope) {
    if (search_scope(scope, sym)) {
      if (depth > 0) {
        kh_put(a, &scope->captures, sym, &ret); /* capture! */
      }
      return depth;
    }
    depth++;
    scope = scope->up;
  }
  PIC_UNREACHABLE();
}

static void
define_var(pic_state *pic, analyze_scope *scope, pic_sym *sym)
{
  int ret;

  if (search_scope(scope, sym)) {
    if (scope->depth > 0 || pic_dict_has(pic, pic->globals, sym)) {
      pic_warnf(pic, "redefining variable: ~s", pic_obj_value(sym));
    }
    return;
  }

  kh_put(a, &scope->locals, sym, &ret);
}

static pic_value analyze(pic_state *, analyze_scope *, pic_value, bool);
static pic_value analyze_lambda(pic_state *, analyze_scope *, pic_value);

static pic_value
analyze_var(pic_state *pic, analyze_scope *scope, pic_sym *sym)
{
  int depth;

  depth = find_var(pic, scope, sym);

  if (depth == scope->depth) {
    return pic_list2(pic, pic_obj_value(pic->sGREF), pic_obj_value(sym));
  } else if (depth == 0) {
    return pic_list2(pic, pic_obj_value(pic->sLREF), pic_obj_value(sym));
  } else {
    return pic_list3(pic, pic_obj_value(pic->sCREF), pic_int_value(depth), pic_obj_value(sym));
  }
}

static pic_value
analyze_defer(pic_state *pic, analyze_scope *scope, pic_value form)
{
  pic_sym *sNOWHERE = pic_intern_cstr(pic, "<<nowhere>>");
  pic_value skel;

  skel = pic_list2(pic, pic_obj_value(pic->sGREF), pic_obj_value(sNOWHERE));

  pic_push(pic, pic_cons(pic, skel, form), scope->defer);

  return skel;
}

static void
analyze_deferred(pic_state *pic, analyze_scope *scope)
{
  pic_value defer, it, skel, form, val;

  pic_for_each (defer, pic_reverse(pic, scope->defer), it) {
    skel = pic_car(pic, defer);
    form = pic_cdr(pic, defer);

    val = analyze_lambda(pic, scope, form);

    /* copy */
    pic_pair_ptr(skel)->car = pic_car(pic, val);
    pic_pair_ptr(skel)->cdr = pic_cdr(pic, val);
  }

  scope->defer = pic_nil_value();
}

static pic_value
analyze_lambda(pic_state *pic, analyze_scope *up, pic_value form)
{
  analyze_scope s, *scope = &s;
  pic_value formals, body;
  pic_value rest = pic_undef_value();
  pic_vec *args, *locals, *captures;
  size_t i, j;

  formals = pic_list_ref(pic, form, 1);
  body = pic_list_ref(pic, form, 2);

  analyzer_scope_init(pic, scope, formals, up);

  /* analyze body */
  body = analyze(pic, scope, body, true);
  analyze_deferred(pic, scope);

  args = pic_make_vec(pic, kh_size(&scope->args));
  for (i = 0; pic_pair_p(formals); formals = pic_cdr(pic, formals), i++) {
    args->data[i] = pic_car(pic, formals);
  }

  if (scope->rest != NULL) {
    rest = pic_obj_value(scope->rest);
  }

  locals = pic_make_vec(pic, kh_size(&scope->locals));
  for (i = kh_begin(&scope->locals), j = 0; i < kh_end(&scope->locals); ++i) {
    if (kh_exist(&scope->locals, i)) {
      locals->data[j++] = pic_obj_value(kh_key(&scope->locals, i));
    }
  }

  captures = pic_make_vec(pic, kh_size(&scope->captures));
  for (i = kh_begin(&scope->captures), j = 0; i < kh_end(&scope->captures); ++i) {
    if (kh_exist(&scope->captures, i)) {
      captures->data[j++] = pic_obj_value(kh_key(&scope->captures, i));
    }
  }

  analyzer_scope_destroy(pic, scope);

  return pic_list6(pic, pic_obj_value(pic->sLAMBDA), rest, pic_obj_value(args), pic_obj_value(locals), pic_obj_value(captures), body);
}

static pic_value
analyze_define(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  pic_value var, val;

  define_var(pic, scope, pic_sym_ptr(pic_list_ref(pic, obj, 1)));

  var = analyze(pic, scope, pic_list_ref(pic, obj, 1), false);
  val = analyze(pic, scope, pic_list_ref(pic, obj, 2), false);

  return pic_list3(pic, pic_obj_value(pic->sSETBANG), var, val);
}

static pic_value
analyze_if(pic_state *pic, analyze_scope *scope, pic_value obj, bool tailpos)
{
  pic_value cond, if_true, if_false;

  if_true = pic_list_ref(pic, obj, 2);
  if_false = pic_list_ref(pic, obj, 3);

  cond = analyze(pic, scope, pic_list_ref(pic, obj, 1), false);
  if_true = analyze(pic, scope, if_true, tailpos);
  if_false = analyze(pic, scope, if_false, tailpos);

  return pic_list4(pic, pic_obj_value(pic->sIF), cond, if_true, if_false);
}

static pic_value
analyze_begin(pic_state *pic, analyze_scope *scope, pic_value obj, bool tailpos)
{
  pic_value beg1, beg2;

  beg1 = pic_list_ref(pic, obj, 1);
  beg2 = pic_list_ref(pic, obj, 2);

  beg1 = analyze(pic, scope, beg1, false);
  beg2 = analyze(pic, scope, beg2, tailpos);

  return pic_list3(pic, pic_obj_value(pic->sBEGIN), beg1, beg2);
}

static pic_value
analyze_set(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  pic_value var, val;

  var = pic_list_ref(pic, obj, 1);
  val = pic_list_ref(pic, obj, 2);

  var = analyze(pic, scope, var, false);
  val = analyze(pic, scope, val, false);

  return pic_list3(pic, pic_obj_value(pic->sSETBANG), var, val);
}

static pic_value
analyze_quote(pic_state *pic, pic_value obj)
{
  return pic_list2(pic, pic_obj_value(pic->sQUOTE), pic_list_ref(pic, obj, 1));
}

#define ARGC_ASSERT_GE(n, name) do {                                    \
    if (pic_length(pic, obj) < (n) + 1) {                               \
      pic_errorf(pic,                                                   \
        #name ": wrong number of arguments (%d for at least %d)",       \
                 pic_length(pic, obj) - 1,                              \
                 n);                                                    \
    }                                                                   \
  } while (0)

#define FOLD_ARGS(sym) do {                                     \
    obj = analyze(pic, scope, pic_car(pic, args), false);       \
    pic_for_each (arg, pic_cdr(pic, args), it) {                \
      obj = pic_list3(pic, pic_obj_value(sym), obj,             \
                      analyze(pic, scope, arg, false));         \
    }                                                           \
  } while (0)

static pic_value
analyze_add(pic_state *pic, analyze_scope *scope, pic_value obj, bool tailpos)
{
  pic_value args, arg, it;

  ARGC_ASSERT_GE(0, "+");
  switch (pic_length(pic, obj)) {
  case 1:
    return pic_list2(pic, pic_obj_value(pic->sQUOTE), pic_int_value(0));
  case 2:
    return analyze(pic, scope, pic_car(pic, pic_cdr(pic, obj)), tailpos);
  default:
    args = pic_cdr(pic, obj);
    FOLD_ARGS(pic->sADD);
    return obj;
  }
}

static pic_value
analyze_sub(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  pic_value args, arg, it;

  ARGC_ASSERT_GE(1, "-");
  switch (pic_length(pic, obj)) {
  case 2:
    return pic_list2(pic, pic_obj_value(pic->sMINUS),
                     analyze(pic, scope, pic_car(pic, pic_cdr(pic, obj)), false));
  default:
    args = pic_cdr(pic, obj);
    FOLD_ARGS(pic->sSUB);
    return obj;
  }
}

static pic_value
analyze_mul(pic_state *pic, analyze_scope *scope, pic_value obj, bool tailpos)
{
  pic_value args, arg, it;

  ARGC_ASSERT_GE(0, "*");
  switch (pic_length(pic, obj)) {
  case 1:
    return pic_list2(pic, pic_obj_value(pic->sQUOTE), pic_int_value(1));
  case 2:
    return analyze(pic, scope, pic_car(pic, pic_cdr(pic, obj)), tailpos);
  default:
    args = pic_cdr(pic, obj);
    FOLD_ARGS(pic->sMUL);
    return obj;
  }
}

static pic_value
analyze_div(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  pic_value args, arg, it;

  ARGC_ASSERT_GE(1, "/");
  switch (pic_length(pic, obj)) {
  case 2:
    args = pic_cdr(pic, obj);
#if PIC_ENABLE_FLOAT
    obj = pic_list3(pic, pic_car(pic, obj), pic_float_value(1), pic_car(pic, args));
#else
    obj = pic_list3(pic, pic_car(pic, obj), pic_int_value(1), pic_car(pic, args));
#endif
    return analyze(pic, scope, obj, false);
  default:
    args = pic_cdr(pic, obj);
    FOLD_ARGS(pic->sDIV);
    return obj;
  }
}

static pic_value
analyze_call(pic_state *pic, analyze_scope *scope, pic_value obj, bool tailpos)
{
  pic_value seq, elt, it;
  pic_sym *call;

  if (! tailpos) {
    call = pic->sCALL;
  } else {
    call = pic->sTAILCALL;
  }
  seq = pic_list1(pic, pic_obj_value(call));
  pic_for_each (elt, obj, it) {
    seq = pic_cons(pic, analyze(pic, scope, elt, false), seq);
  }
  return pic_reverse(pic, seq);
}

static pic_value
analyze_values(pic_state *pic, analyze_scope *scope, pic_value obj, bool tailpos)
{
  pic_value v, seq, it;

  if (! tailpos) {
    return analyze_call(pic, scope, obj, false);
  }

  seq = pic_list1(pic, pic_obj_value(pic->sRETURN));
  pic_for_each (v, pic_cdr(pic, obj), it) {
    seq = pic_cons(pic, analyze(pic, scope, v, false), seq);
  }
  return pic_reverse(pic, seq);
}

static pic_value
analyze_call_with_values(pic_state *pic, analyze_scope *scope, pic_value obj, bool tailpos)
{
  pic_value prod, cnsm;
  pic_sym *call;

  if (pic_length(pic, obj) != 3) {
    pic_errorf(pic, "call-with-values: wrong number of arguments (%d for 2)", pic_length(pic, obj) - 1);
  }

  if (! tailpos) {
    call = pic->sCALL_WITH_VALUES;
  } else {
    call = pic->sTAILCALL_WITH_VALUES;
  }
  prod = analyze(pic, scope, pic_list_ref(pic, obj, 1), false);
  cnsm = analyze(pic, scope, pic_list_ref(pic, obj, 2), false);
  return pic_list3(pic, pic_obj_value(call), prod, cnsm);
}

#define ARGC_ASSERT(n, name) do {                                       \
    if (pic_length(pic, obj) != (n) + 1) {                              \
      pic_errorf(pic, #name ": wrong number of arguments (%d for %d)",  \
                 pic_length(pic, obj) - 1, n);                          \
    }                                                                   \
  } while (0)

#define ARGC_ASSERT_WITH_FALLBACK(n) do {       \
    if (pic_length(pic, obj) != (n) + 1) {      \
      goto fallback;                            \
    }						\
  } while (0)

#define CONSTRUCT_OP1(op)                                               \
  pic_list2(pic,                                                        \
            pic_obj_value(op),                                          \
            analyze(pic, scope, pic_list_ref(pic, obj, 1), false))

#define CONSTRUCT_OP2(op)                                               \
  pic_list3(pic,                                                        \
            pic_obj_value(op),                                          \
            analyze(pic, scope, pic_list_ref(pic, obj, 1), false),      \
            analyze(pic, scope, pic_list_ref(pic, obj, 2), false))

static pic_value
analyze_node(pic_state *pic, analyze_scope *scope, pic_value obj, bool tailpos)
{
  switch (pic_type(obj)) {
  case PIC_TT_SYMBOL: {
    return analyze_var(pic, scope, pic_sym_ptr(obj));
  }
  case PIC_TT_PAIR: {
    pic_value proc;

    if (! pic_list_p(obj)) {
      pic_errorf(pic, "invalid expression given: ~s", obj);
    }

    proc = pic_list_ref(pic, obj, 0);
    if (pic_sym_p(proc)) {
      pic_sym *sym = pic_sym_ptr(proc);

      if (sym == pic->uDEFINE) {
        return analyze_define(pic, scope, obj);
      }
      else if (sym == pic->uLAMBDA) {
        return analyze_defer(pic, scope, obj);
      }
      else if (sym == pic->uIF) {
        return analyze_if(pic, scope, obj, tailpos);
      }
      else if (sym == pic->uBEGIN) {
        return analyze_begin(pic, scope, obj, tailpos);
      }
      else if (sym == pic->uSETBANG) {
        return analyze_set(pic, scope, obj);
      }
      else if (sym == pic->uQUOTE) {
        return analyze_quote(pic, obj);
      }
      else if (sym == pic->uCONS) {
	ARGC_ASSERT(2, "cons");
        return CONSTRUCT_OP2(pic->sCONS);
      }
      else if (sym == pic->uCAR) {
	ARGC_ASSERT(1, "car");
        return CONSTRUCT_OP1(pic->sCAR);
      }
      else if (sym == pic->uCDR) {
	ARGC_ASSERT(1, "cdr");
        return CONSTRUCT_OP1(pic->sCDR);
      }
      else if (sym == pic->uNILP) {
	ARGC_ASSERT(1, "nil?");
        return CONSTRUCT_OP1(pic->sNILP);
      }
      else if (sym == pic->uSYMBOLP) {
        ARGC_ASSERT(1, "symbol?");
        return CONSTRUCT_OP1(pic->sSYMBOLP);
      }
      else if (sym == pic->uPAIRP) {
        ARGC_ASSERT(1, "pair?");
        return CONSTRUCT_OP1(pic->sPAIRP);
      }
      else if (sym == pic->uADD) {
        return analyze_add(pic, scope, obj, tailpos);
      }
      else if (sym == pic->uSUB) {
        return analyze_sub(pic, scope, obj);
      }
      else if (sym == pic->uMUL) {
        return analyze_mul(pic, scope, obj, tailpos);
      }
      else if (sym == pic->uDIV) {
        return analyze_div(pic, scope, obj);
      }
      else if (sym == pic->uEQ) {
	ARGC_ASSERT_WITH_FALLBACK(2);
        return CONSTRUCT_OP2(pic->sEQ);
      }
      else if (sym == pic->uLT) {
	ARGC_ASSERT_WITH_FALLBACK(2);
        return CONSTRUCT_OP2(pic->sLT);
      }
      else if (sym == pic->uLE) {
	ARGC_ASSERT_WITH_FALLBACK(2);
        return CONSTRUCT_OP2(pic->sLE);
      }
      else if (sym == pic->uGT) {
	ARGC_ASSERT_WITH_FALLBACK(2);
        return CONSTRUCT_OP2(pic->sGT);
      }
      else if (sym == pic->uGE) {
	ARGC_ASSERT_WITH_FALLBACK(2);
        return CONSTRUCT_OP2(pic->sGE);
      }
      else if (sym == pic->uNOT) {
        ARGC_ASSERT(1, "not");
        return CONSTRUCT_OP1(pic->sNOT);
      }
      else if (sym == pic->uVALUES) {
        return analyze_values(pic, scope, obj, tailpos);
      }
      else if (sym == pic->uCALL_WITH_VALUES) {
        return analyze_call_with_values(pic, scope, obj, tailpos);
      }
    }
    fallback:

    return analyze_call(pic, scope, obj, tailpos);
  }
  default:
    return pic_list2(pic, pic_obj_value(pic->sQUOTE), obj);
  }
}

static pic_value
analyze(pic_state *pic, analyze_scope *scope, pic_value obj, bool tailpos)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value res;
  pic_sym *tag;

  res = analyze_node(pic, scope, obj, tailpos);

  tag = pic_sym_ptr(pic_car(pic, res));
  if (tailpos) {
    if (tag == pic->sIF || tag == pic->sBEGIN || tag == pic->sTAILCALL || tag == pic->sTAILCALL_WITH_VALUES || tag == pic->sRETURN) {
      /* pass through */
    }
    else {
      res = pic_list2(pic, pic_obj_value(pic->sRETURN), res);
    }
  }

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, res);
  pic_gc_protect(pic, scope->defer);
  return res;
}

pic_value
pic_analyze(pic_state *pic, pic_value obj)
{
  analyze_scope s, *scope = &s;

  analyzer_scope_init(pic, scope, pic_nil_value(), NULL);

  obj = analyze(pic, scope, obj, true);

  analyze_deferred(pic, scope);

  analyzer_scope_destroy(pic, scope);
  return obj;
}

typedef struct codegen_context {
  /* rest args variable is counted as a local */
  pic_sym *rest;
  pic_vec *args, *locals, *captures;
  /* actual bit code sequence */
  pic_code *code;
  size_t clen, ccapa;
  /* child ireps */
  struct pic_irep **irep;
  size_t ilen, icapa;
  /* constant object pool */
  pic_value *pool;
  size_t plen, pcapa;
  /* symbol pool */
  pic_sym **syms;
  size_t slen, scapa;

  struct codegen_context *up;
} codegen_context;

static void create_activation(pic_state *, codegen_context *);

static void
codegen_context_init(pic_state *pic, codegen_context *cxt, codegen_context *up, pic_sym *rest, pic_vec *args, pic_vec *locals, pic_vec *captures)
{
  cxt->up = up;
  cxt->rest = rest;

  cxt->args = args;
  cxt->locals = locals;
  cxt->captures = captures;

  cxt->code = pic_calloc(pic, PIC_ISEQ_SIZE, sizeof(pic_code));
  cxt->clen = 0;
  cxt->ccapa = PIC_ISEQ_SIZE;

  cxt->irep = pic_calloc(pic, PIC_IREP_SIZE, sizeof(struct pic_irep *));
  cxt->ilen = 0;
  cxt->icapa = PIC_IREP_SIZE;

  cxt->pool = pic_calloc(pic, PIC_POOL_SIZE, sizeof(pic_value));
  cxt->plen = 0;
  cxt->pcapa = PIC_POOL_SIZE;

  cxt->syms = pic_calloc(pic, PIC_SYMS_SIZE, sizeof(pic_sym *));
  cxt->slen = 0;
  cxt->scapa = PIC_SYMS_SIZE;

  create_activation(pic, cxt);
}

static struct pic_irep *
codegen_context_destroy(pic_state *pic, codegen_context *cxt)
{
  struct pic_irep *irep;

  /* create irep */
  irep = (struct pic_irep *)pic_obj_alloc(pic, sizeof(struct pic_irep), PIC_TT_IREP);
  irep->varg = cxt->rest != NULL;
  irep->argc = (int)cxt->args->len + 1;
  irep->localc = (int)cxt->locals->len;
  irep->capturec = (int)cxt->captures->len;
  irep->code = pic_realloc(pic, cxt->code, sizeof(pic_code) * cxt->clen);
  irep->clen = cxt->clen;
  irep->irep = pic_realloc(pic, cxt->irep, sizeof(struct pic_irep *) * cxt->ilen);
  irep->ilen = cxt->ilen;
  irep->pool = pic_realloc(pic, cxt->pool, sizeof(pic_value) * cxt->plen);
  irep->plen = cxt->plen;
  irep->syms = pic_realloc(pic, cxt->syms, sizeof(pic_sym *) * cxt->slen);
  irep->slen = cxt->slen;

  return irep;
}

#define check_size(pic, cxt, x, name, type) do {                        \
    if (cxt->x##len >= cxt->x##capa) {                                  \
      cxt->x##capa *= 2;                                                \
      cxt->name = pic_realloc(pic, cxt->name, sizeof(type) * cxt->x##capa); \
    }                                                                   \
  } while (0)

#define check_code_size(pic, cxt) check_size(pic, cxt, c, code, pic_code)
#define check_syms_size(pic, cxt) check_size(pic, cxt, s, syms, pic_sym *)
#define check_irep_size(pic, cxt) check_size(pic, cxt, i, irep, struct pic_irep *)
#define check_pool_size(pic, cxt) check_size(pic, cxt, p, pool, pic_value)

#define emit_n(pic, cxt, ins) do {              \
    check_code_size(pic, cxt);                  \
    cxt->code[cxt->clen].insn = ins;            \
    cxt->clen++;                                \
  } while (0)                                   \

#define emit_i(pic, cxt, ins, I) do {           \
    check_code_size(pic, cxt);                  \
    cxt->code[cxt->clen].insn = ins;            \
    cxt->code[cxt->clen].u.i = I;               \
    cxt->clen++;                                \
  } while (0)                                   \

#define emit_c(pic, cxt, ins, C) do {           \
    check_code_size(pic, cxt);                  \
    cxt->code[cxt->clen].insn = ins;            \
    cxt->code[cxt->clen].u.c = C;               \
    cxt->clen++;                                \
  } while (0)                                   \

#define emit_r(pic, cxt, ins, D, I) do {        \
    check_code_size(pic, cxt);                  \
    cxt->code[cxt->clen].insn = ins;            \
    cxt->code[cxt->clen].u.r.depth = D;         \
    cxt->code[cxt->clen].u.r.idx = I;           \
    cxt->clen++;                                \
  } while (0)                                   \

static void
create_activation(pic_state *pic, codegen_context *cxt)
{
  size_t i, n;
  size_t offset;
  struct pic_reg *regs;

  regs = pic_make_reg(pic);

  offset = 1;
  for (i = 0; i < cxt->args->len; ++i) {
    n = i + offset;
    pic_reg_set(pic, regs, pic_sym_ptr(cxt->args->data[i]), pic_size_value(n));
  }
  offset += i;
  for (i = 0; i < cxt->locals->len; ++i) {
    n = i + offset;
    pic_reg_set(pic, regs, pic_sym_ptr(cxt->locals->data[i]), pic_size_value(n));
  }

  for (i = 0; i < cxt->captures->len; ++i) {
    n = (size_t)pic_int(pic_reg_ref(pic, regs, pic_sym_ptr(cxt->captures->data[i])));
    if (n <= cxt->args->len || cxt->rest == pic_sym_ptr(cxt->captures->data[i])) {
      /* copy arguments to capture variable area */
      emit_i(pic, cxt, OP_LREF, (int)n);
    } else {
      /* otherwise, just extend the stack */
      emit_n(pic, cxt, OP_PUSHUNDEF);
    }
  }
}

static int
index_capture(codegen_context *cxt, pic_sym *sym, int depth)
{
  size_t i;

  while (depth-- > 0) {
    cxt = cxt->up;
  }

  for (i = 0; i < cxt->captures->len; ++i) {
    if (pic_sym_ptr(cxt->captures->data[i]) == sym)
      return (int)i;
  }
  return -1;
}

static int
index_local(codegen_context *cxt, pic_sym *sym)
{
  size_t i, offset;

  offset = 1;
  for (i = 0; i < cxt->args->len; ++i) {
    if (pic_sym_ptr(cxt->args->data[i]) == sym)
      return (int)(i + offset);
  }
  offset += i;
  for (i = 0; i < cxt->locals->len; ++i) {
    if (pic_sym_ptr(cxt->locals->data[i]) == sym)
      return (int)(i + offset);
  }
  return -1;
}

static int
index_symbol(pic_state *pic, codegen_context *cxt, pic_sym *sym)
{
  size_t i;

  for (i = 0; i < cxt->slen; ++i) {
    if (cxt->syms[i] == sym) {
      return i;
    }
  }
  check_syms_size(pic, cxt);
  cxt->syms[cxt->slen++] = sym;
  return i;
}

static void codegen(pic_state *, codegen_context *, pic_value);

static struct pic_irep *
codegen_lambda(pic_state *pic, codegen_context *up, pic_value obj)
{
  codegen_context c, *cxt = &c;
  pic_value rest_opt, body;
  pic_sym *rest = NULL;
  pic_vec *args, *locals, *captures;

  rest_opt = pic_list_ref(pic, obj, 1);
  if (pic_sym_p(rest_opt)) {
    rest = pic_sym_ptr(rest_opt);
  }
  args = pic_vec_ptr(pic_list_ref(pic, obj, 2));
  locals = pic_vec_ptr(pic_list_ref(pic, obj, 3));
  captures = pic_vec_ptr(pic_list_ref(pic, obj, 4));
  body = pic_list_ref(pic, obj, 5);

  /* inner environment */
  codegen_context_init(pic, cxt, up, rest, args, locals, captures);
  {
    /* body */
    codegen(pic, cxt, body);
  }
  return codegen_context_destroy(pic, cxt);
}

static void
codegen(pic_state *pic, codegen_context *cxt, pic_value obj)
{
  pic_sym *sym;

  sym = pic_sym_ptr(pic_car(pic, obj));
  if (sym == pic->sGREF) {
    emit_i(pic, cxt, OP_GREF, index_symbol(pic, cxt, pic_sym_ptr(pic_list_ref(pic, obj, 1))));
    return;
  } else if (sym == pic->sCREF) {
    pic_sym *name;
    int depth;

    depth = pic_int(pic_list_ref(pic, obj, 1));
    name  = pic_sym_ptr(pic_list_ref(pic, obj, 2));
    emit_r(pic, cxt, OP_CREF, depth, index_capture(cxt, name, depth));
    return;
  } else if (sym == pic->sLREF) {
    pic_sym *name;
    int i;

    name = pic_sym_ptr(pic_list_ref(pic, obj, 1));
    if ((i = index_capture(cxt, name, 0)) != -1) {
      emit_i(pic, cxt, OP_LREF, i + (int)cxt->args->len + (int)cxt->locals->len + 1);
      return;
    }
    emit_i(pic, cxt, OP_LREF, index_local(cxt, name));
    return;
  } else if (sym == pic->sSETBANG) {
    pic_value var, val;
    pic_sym *type;

    val = pic_list_ref(pic, obj, 2);
    codegen(pic, cxt, val);

    var = pic_list_ref(pic, obj, 1);
    type = pic_sym_ptr(pic_list_ref(pic, var, 0));
    if (type == pic->sGREF) {
      emit_i(pic, cxt, OP_GSET, index_symbol(pic, cxt, pic_sym_ptr(pic_list_ref(pic, var, 1))));
      emit_n(pic, cxt, OP_PUSHUNDEF);
      return;
    }
    else if (type == pic->sCREF) {
      pic_sym *name;
      int depth;

      depth = pic_int(pic_list_ref(pic, var, 1));
      name  = pic_sym_ptr(pic_list_ref(pic, var, 2));
      emit_r(pic, cxt, OP_CSET, depth, index_capture(cxt, name, depth));
      emit_n(pic, cxt, OP_PUSHUNDEF);
      return;
    }
    else if (type == pic->sLREF) {
      pic_sym *name;
      int i;

      name = pic_sym_ptr(pic_list_ref(pic, var, 1));
      if ((i = index_capture(cxt, name, 0)) != -1) {
        emit_i(pic, cxt, OP_LSET, i + (int)cxt->args->len + (int)cxt->locals->len + 1);
        emit_n(pic, cxt, OP_PUSHUNDEF);
        return;
      }
      emit_i(pic, cxt, OP_LSET, index_local(cxt, name));
      emit_n(pic, cxt, OP_PUSHUNDEF);
      return;
    }
  }
  else if (sym == pic->sLAMBDA) {
    int k;

    check_irep_size(pic, cxt);
    k = (int)cxt->ilen++;
    emit_i(pic, cxt, OP_LAMBDA, k);

    cxt->irep[k] = codegen_lambda(pic, cxt, obj);
    return;
  }
  else if (sym == pic->sIF) {
    int s, t;

    codegen(pic, cxt, pic_list_ref(pic, obj, 1));

    s = (int)cxt->clen;

    emit_n(pic, cxt, OP_JMPIF);

    /* if false branch */
    codegen(pic, cxt, pic_list_ref(pic, obj, 3));

    t = (int)cxt->clen;

    emit_n(pic, cxt, OP_JMP);

    cxt->code[s].u.i = (int)cxt->clen - s;

    /* if true branch */
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    cxt->code[t].u.i = (int)cxt->clen - t;
    return;
  }
  else if (sym == pic->sBEGIN) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    emit_n(pic, cxt, OP_POP);
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    return;
  }
  else if (sym == pic->sQUOTE) {
    int pidx;

    obj = pic_list_ref(pic, obj, 1);
    switch (pic_type(obj)) {
    case PIC_TT_BOOL:
      emit_n(pic, cxt, (pic_true_p(obj) ? OP_PUSHTRUE : OP_PUSHFALSE));
      return;
    case PIC_TT_INT:
      emit_i(pic, cxt, OP_PUSHINT, pic_int(obj));
      return;
    case PIC_TT_NIL:
      emit_n(pic, cxt, OP_PUSHNIL);
      return;
    case PIC_TT_CHAR:
      emit_c(pic, cxt, OP_PUSHCHAR, pic_char(obj));
      return;
    default:
      check_pool_size(pic, cxt);
      pidx = (int)cxt->plen++;
      cxt->pool[pidx] = obj;
      emit_i(pic, cxt, OP_PUSHCONST, pidx);
      return;
    }
  }
  else if (sym == pic->sCONS) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    emit_n(pic, cxt, OP_CONS);
    return;
  }
  else if (sym == pic->sCAR) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    emit_n(pic, cxt, OP_CAR);
    return;
  }
  else if (sym == pic->sCDR) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    emit_n(pic, cxt, OP_CDR);
    return;
  }
  else if (sym == pic->sNILP) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    emit_n(pic, cxt, OP_NILP);
    return;
  }
  else if (sym == pic->sSYMBOLP) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    emit_n(pic, cxt, OP_SYMBOLP);
    return;
  }
  else if (sym == pic->sPAIRP) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    emit_n(pic, cxt, OP_PAIRP);
    return;
  }
  else if (sym == pic->sADD) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    emit_n(pic, cxt, OP_ADD);
    return;
  }
  else if (sym == pic->sSUB) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    emit_n(pic, cxt, OP_SUB);
    return;
  }
  else if (sym == pic->sMUL) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    emit_n(pic, cxt, OP_MUL);
    return;
  }
  else if (sym == pic->sDIV) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    emit_n(pic, cxt, OP_DIV);
    return;
  }
  else if (sym == pic->sMINUS) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    emit_n(pic, cxt, OP_MINUS);
    return;
  }
  else if (sym == pic->sEQ) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    emit_n(pic, cxt, OP_EQ);
    return;
  }
  else if (sym == pic->sLT) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    emit_n(pic, cxt, OP_LT);
    return;
  }
  else if (sym == pic->sLE) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    emit_n(pic, cxt, OP_LE);
    return;
  }
  else if (sym == pic->sGT) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    emit_n(pic, cxt, OP_LT);
    return;
  }
  else if (sym == pic->sGE) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    emit_n(pic, cxt, OP_LE);
    return;
  }
  else if (sym == pic->sNOT) {
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    emit_n(pic, cxt, OP_NOT);
    return;
  }
  else if (sym == pic->sCALL || sym == pic->sTAILCALL) {
    int len = (int)pic_length(pic, obj);
    pic_value elt, it;

    pic_for_each (elt, pic_cdr(pic, obj), it) {
      codegen(pic, cxt, elt);
    }
    emit_i(pic, cxt, (sym == pic->sCALL ? OP_CALL : OP_TAILCALL), len - 1);
    return;
  }
  else if (sym == pic->sCALL_WITH_VALUES || sym == pic->sTAILCALL_WITH_VALUES) {
    /* stack consumer at first */
    codegen(pic, cxt, pic_list_ref(pic, obj, 2));
    codegen(pic, cxt, pic_list_ref(pic, obj, 1));
    /* call producer */
    emit_i(pic, cxt, OP_CALL, 1);
    /* call consumer */
    emit_i(pic, cxt, (sym == pic->sCALL_WITH_VALUES ? OP_CALL : OP_TAILCALL), -1);
    return;
  }
  else if (sym == pic->sRETURN) {
    int len = (int)pic_length(pic, obj);
    pic_value elt, it;

    pic_for_each (elt, pic_cdr(pic, obj), it) {
      codegen(pic, cxt, elt);
    }
    emit_i(pic, cxt, OP_RET, len - 1);
    return;
  }
  pic_errorf(pic, "codegen: unknown AST type ~s", obj);
}

struct pic_irep *
pic_codegen(pic_state *pic, pic_value obj)
{
  pic_vec *empty = pic_make_vec(pic, 0);
  codegen_context c, *cxt = &c;

  codegen_context_init(pic, cxt, NULL, NULL, empty, empty, empty);

  codegen(pic, cxt, obj);

  return codegen_context_destroy(pic, cxt);
}

#define SAVE(pic, ai, obj) pic_gc_arena_restore(pic, ai); pic_gc_protect(pic, obj)

struct pic_proc *
pic_compile(pic_state *pic, pic_value obj, struct pic_env *env)
{
  struct pic_irep *irep;
  size_t ai = pic_gc_arena_preserve(pic);

#if DEBUG
  fprintf(stdout, "ai = %zu\n", pic_gc_arena_preserve(pic));

  fprintf(stdout, "# input expression\n");
  pic_write(pic, obj);
  fprintf(stdout, "\n");

  fprintf(stdout, "ai = %zu\n", pic_gc_arena_preserve(pic));
#endif

  /* expand */
  obj = pic_expand(pic, obj, env);
#if DEBUG
  fprintf(stdout, "## expand completed\n");
  pic_write(pic, obj);
  fprintf(stdout, "\n");
  fprintf(stdout, "ai = %zu\n", pic_gc_arena_preserve(pic));
#endif

  SAVE(pic, ai, obj);

  /* analyze */
  obj = pic_analyze(pic, obj);
#if DEBUG
  fprintf(stdout, "## analyzer completed\n");
  pic_write(pic, obj);
  fprintf(stdout, "\n");
  fprintf(stdout, "ai = %zu\n", pic_gc_arena_preserve(pic));
#endif

  SAVE(pic, ai, obj);

  /* codegen */
  irep = pic_codegen(pic, obj);
#if DEBUG
  fprintf(stdout, "## codegen completed\n");
  pic_dump_irep(irep);
#endif

#if DEBUG
  fprintf(stdout, "# compilation finished\n");
  puts("");
#endif

  SAVE(pic, ai, pic_obj_value(irep));

  return pic_make_proc_irep(pic, irep, NULL);
}
