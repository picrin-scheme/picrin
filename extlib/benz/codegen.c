/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/opcode.h"

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

static pic_value
optimize_beta(pic_state *pic, pic_value expr)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value functor, formals, args, tmp, val, it, defs;

  if (! pic_list_p(expr))
    return expr;

  if (pic_nil_p(expr))
    return expr;

  if (pic_sym_p(pic_list_ref(pic, expr, 0))) {
    pic_sym *sym = pic_sym_ptr(pic_list_ref(pic, expr, 0));

    if (sym == pic->uQUOTE) {
      return expr;
    } else if (sym == pic->uLAMBDA) {
      return pic_list3(pic, pic_list_ref(pic, expr, 0), pic_list_ref(pic, expr, 1), optimize_beta(pic, pic_list_ref(pic, expr, 2)));
    }
  }

  tmp = pic_nil_value();
  pic_for_each (val, expr, it) {
    pic_push(pic, optimize_beta(pic, val), tmp);
  }
  expr = pic_reverse(pic, tmp);

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, expr);

  functor = pic_list_ref(pic, expr, 0);
  if (pic_pair_p(functor) && pic_eq_p(pic_car(pic, functor), pic_obj_value(pic->uLAMBDA))) {
    formals = pic_list_ref(pic, functor, 1);
    if (! pic_list_p(formals))
      goto exit;              /* TODO: support ((lambda args x) 1 2) */
    args = pic_cdr(pic, expr);
    if (pic_length(pic, formals) != pic_length(pic, args))
      goto exit;
    defs = pic_nil_value();
    pic_for_each (val, args, it) {
      pic_push(pic, pic_list3(pic, pic_obj_value(pic->uDEFINE), pic_car(pic, formals), val), defs);
      formals = pic_cdr(pic, formals);
    }
    expr = pic_list_ref(pic, functor, 2);
    pic_for_each (val, defs, it) {
      expr = pic_list3(pic, pic_obj_value(pic->uBEGIN), val, expr);
    }
  }
 exit:

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, expr);
  return expr;
}

pic_value
pic_optimize(pic_state *pic, pic_value expr)
{
  return optimize_beta(pic, expr);
}

KHASH_DECLARE(a, pic_sym *, int)
KHASH_DEFINE2(a, pic_sym *, int, 0, kh_ptr_hash_func, kh_ptr_hash_equal)

/**
 * TODO: don't use khash_t, use kvec_t instead
 */

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
  scope->defer = pic_list1(pic, pic_nil_value());
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
    if (scope->depth > 0 || (pic_reg_has(pic, pic->globals, sym) && ! pic_invalid_p(pic_box_ptr(pic_reg_ref(pic, pic->globals, sym))->value))) {
      pic_warnf(pic, "redefining variable: ~s", pic_obj_value(sym));
    }
    return;
  }

  kh_put(a, &scope->locals, sym, &ret);
}

static pic_value analyze(pic_state *, analyze_scope *, pic_value);
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
  pic_value skel = pic_cons(pic, pic_invalid_value(), pic_invalid_value());

  pic_set_car(pic, scope->defer, pic_acons(pic, form, skel, pic_car(pic, scope->defer)));

  return skel;
}

static void
analyze_deferred(pic_state *pic, analyze_scope *scope)
{
  pic_value defer, val, src, dst, it;

  scope->defer = pic_car(pic, scope->defer);

  pic_for_each (defer, pic_reverse(pic, scope->defer), it) {
    src = pic_car(pic, defer);
    dst = pic_cdr(pic, defer);

    val = analyze_lambda(pic, scope, src);

    /* copy */
    pic_set_car(pic, dst, pic_car(pic, val));
    pic_set_cdr(pic, dst, pic_cdr(pic, val));
  }
}

static pic_value
analyze_lambda(pic_state *pic, analyze_scope *up, pic_value form)
{
  analyze_scope s, *scope = &s;
  pic_value formals, body;
  pic_value rest = pic_undef_value();
  pic_vec *args, *locals, *captures;
  size_t i, j;
  khiter_t it;

  formals = pic_list_ref(pic, form, 1);
  body = pic_list_ref(pic, form, 2);

  analyzer_scope_init(pic, scope, formals, up);

  /* analyze body */
  body = analyze(pic, scope, body);
  analyze_deferred(pic, scope);

  args = pic_make_vec(pic, kh_size(&scope->args));
  for (i = 0; pic_pair_p(formals); formals = pic_cdr(pic, formals), i++) {
    args->data[i] = pic_car(pic, formals);
  }

  if (scope->rest != NULL) {
    rest = pic_obj_value(scope->rest);
  }

  locals = pic_make_vec(pic, kh_size(&scope->locals));
  j = 0;
  if (scope->rest != NULL) {
    locals->data[j++] = pic_obj_value(scope->rest);
  }
  for (it = kh_begin(&scope->locals); it < kh_end(&scope->locals); ++it) {
    if (kh_exist(&scope->locals, it)) {
      if (scope->rest != NULL && kh_key(&scope->locals, it) == scope->rest)
        continue;
      locals->data[j++] = pic_obj_value(kh_key(&scope->locals, it));
    }
  }

  captures = pic_make_vec(pic, kh_size(&scope->captures));
  for (it = kh_begin(&scope->captures), j = 0; it < kh_end(&scope->captures); ++it) {
    if (kh_exist(&scope->captures, it)) {
      captures->data[j++] = pic_obj_value(kh_key(&scope->captures, it));
    }
  }

  analyzer_scope_destroy(pic, scope);

  return pic_list6(pic, pic_obj_value(pic->uLAMBDA), rest, pic_obj_value(args), pic_obj_value(locals), pic_obj_value(captures), body);
}

static pic_value
analyze_list(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  pic_value seq = pic_nil_value(), val, it;

  pic_for_each (val, obj, it) {
    pic_push(pic, analyze(pic, scope, val), seq);
  }

  return pic_reverse(pic, seq);
}

static pic_value
analyze_define(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  define_var(pic, scope, pic_sym_ptr(pic_list_ref(pic, obj, 1)));

  return pic_cons(pic, pic_car(pic, obj), analyze_list(pic, scope, pic_cdr(pic, obj)));
}

static pic_value
analyze_call(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  return pic_cons(pic, pic_obj_value(pic->sCALL), analyze_list(pic, scope, obj));
}

static pic_value
analyze_node(pic_state *pic, analyze_scope *scope, pic_value obj)
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
      else if (sym == pic->uQUOTE) {
        return obj;
      }
      else if (sym == pic->uBEGIN || sym == pic->uSETBANG || sym == pic->uIF) {
        return pic_cons(pic, pic_car(pic, obj), analyze_list(pic, scope, pic_cdr(pic, obj)));
      }
    }

    return analyze_call(pic, scope, obj);
  }
  default:
    return pic_list2(pic, pic_obj_value(pic->uQUOTE), obj);
  }
}

static pic_value
analyze(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value res;

  res = analyze_node(pic, scope, obj);

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, res);
  return res;
}

pic_value
pic_analyze(pic_state *pic, pic_value obj)
{
  analyze_scope s, *scope = &s;

  analyzer_scope_init(pic, scope, pic_nil_value(), NULL);

  obj = analyze(pic, scope, obj);

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

  return irep;
}

#define check_size(pic, cxt, x, name, type) do {                        \
    if (cxt->x##len >= cxt->x##capa) {                                  \
      cxt->x##capa *= 2;                                                \
      cxt->name = pic_realloc(pic, cxt->name, sizeof(type) * cxt->x##capa); \
    }                                                                   \
  } while (0)

#define check_code_size(pic, cxt) check_size(pic, cxt, c, code, pic_code)
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

#define emit_ret(pic, cxt, tailpos) if (tailpos) emit_n(pic, cxt, OP_RET)

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
index_global(pic_state *pic, codegen_context *cxt, pic_sym *name)
{
  extern struct pic_box *pic_vm_gref_slot(pic_state *, pic_sym *);
  int pidx;
  struct pic_box *slot;

  slot = pic_vm_gref_slot(pic, name);

  check_pool_size(pic, cxt);
  pidx = (int)cxt->plen++;
  cxt->pool[pidx] = pic_obj_value(slot);

  return pidx;
}

static void
create_activation(pic_state *pic, codegen_context *cxt)
{
  size_t i;
  int n;

  for (i = 0; i < cxt->captures->len; ++i) {
    n = index_local(cxt, pic_sym_ptr(cxt->captures->data[i]));
    assert(n != -1);
    if (n <= (int)cxt->args->len || cxt->rest == pic_sym_ptr(cxt->captures->data[i])) {
      /* copy arguments to capture variable area */
      emit_i(pic, cxt, OP_LREF, n);
    } else {
      /* otherwise, just extend the stack */
      emit_n(pic, cxt, OP_PUSHUNDEF);
    }
  }
}

static void codegen(pic_state *, codegen_context *, pic_value, bool);

static void
codegen_ref(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  pic_sym *sym;

  sym = pic_sym_ptr(pic_car(pic, obj));
  if (sym == pic->sGREF) {
    pic_sym *name;

    name = pic_sym_ptr(pic_list_ref(pic, obj, 1));
    emit_i(pic, cxt, OP_GREF, index_global(pic, cxt, name));
    emit_ret(pic, cxt, tailpos);
  }
  else if (sym == pic->sCREF) {
    pic_sym *name;
    int depth;

    depth = pic_int(pic_list_ref(pic, obj, 1));
    name  = pic_sym_ptr(pic_list_ref(pic, obj, 2));
    emit_r(pic, cxt, OP_CREF, depth, index_capture(cxt, name, depth));
    emit_ret(pic, cxt, tailpos);
  }
  else if (sym == pic->sLREF) {
    pic_sym *name;
    int i;

    name = pic_sym_ptr(pic_list_ref(pic, obj, 1));
    if ((i = index_capture(cxt, name, 0)) != -1) {
      emit_i(pic, cxt, OP_LREF, i + (int)cxt->args->len + (int)cxt->locals->len + 1);
      emit_ret(pic, cxt, tailpos);
    } else {
      emit_i(pic, cxt, OP_LREF, index_local(cxt, name));
      emit_ret(pic, cxt, tailpos);
    }
  }
}

static void
codegen_set(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  pic_value var, val;
  pic_sym *type;

  val = pic_list_ref(pic, obj, 2);
  codegen(pic, cxt, val, false);

  var = pic_list_ref(pic, obj, 1);
  type = pic_sym_ptr(pic_list_ref(pic, var, 0));
  if (type == pic->sGREF) {
    pic_sym *name;

    name = pic_sym_ptr(pic_list_ref(pic, var, 1));
    emit_i(pic, cxt, OP_GSET, index_global(pic, cxt, name));
    emit_ret(pic, cxt, tailpos);
  }
  else if (type == pic->sCREF) {
    pic_sym *name;
    int depth;

    depth = pic_int(pic_list_ref(pic, var, 1));
    name  = pic_sym_ptr(pic_list_ref(pic, var, 2));
    emit_r(pic, cxt, OP_CSET, depth, index_capture(cxt, name, depth));
    emit_ret(pic, cxt, tailpos);
  }
  else if (type == pic->sLREF) {
    pic_sym *name;
    int i;

    name = pic_sym_ptr(pic_list_ref(pic, var, 1));
    if ((i = index_capture(cxt, name, 0)) != -1) {
      emit_i(pic, cxt, OP_LSET, i + (int)cxt->args->len + (int)cxt->locals->len + 1);
      emit_ret(pic, cxt, tailpos);
    } else {
      emit_i(pic, cxt, OP_LSET, index_local(cxt, name));
      emit_ret(pic, cxt, tailpos);
    }
  }
}

static void
codegen_lambda(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  codegen_context c, *inner_cxt = &c;
  pic_value rest_opt, body;
  pic_sym *rest = NULL;
  pic_vec *args, *locals, *captures;

  check_irep_size(pic, cxt);

  /* extract arguments */
  rest_opt = pic_list_ref(pic, obj, 1);
  if (pic_sym_p(rest_opt)) {
    rest = pic_sym_ptr(rest_opt);
  }
  args = pic_vec_ptr(pic_list_ref(pic, obj, 2));
  locals = pic_vec_ptr(pic_list_ref(pic, obj, 3));
  captures = pic_vec_ptr(pic_list_ref(pic, obj, 4));
  body = pic_list_ref(pic, obj, 5);

  /* emit irep */
  codegen_context_init(pic, inner_cxt, cxt, rest, args, locals, captures);
  codegen(pic, inner_cxt, body, true);
  cxt->irep[cxt->ilen] = codegen_context_destroy(pic, inner_cxt);

  /* emit OP_LAMBDA */
  emit_i(pic, cxt, OP_LAMBDA, cxt->ilen++);
  emit_ret(pic, cxt, tailpos);
}

static void
codegen_if(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  int s, t;

  codegen(pic, cxt, pic_list_ref(pic, obj, 1), false);

  s = (int)cxt->clen;

  emit_n(pic, cxt, OP_JMPIF);

  /* if false branch */
  codegen(pic, cxt, pic_list_ref(pic, obj, 3), tailpos);

  t = (int)cxt->clen;

  emit_n(pic, cxt, OP_JMP);

  cxt->code[s].u.i = (int)cxt->clen - s;

  /* if true branch */
  codegen(pic, cxt, pic_list_ref(pic, obj, 2), tailpos);
  cxt->code[t].u.i = (int)cxt->clen - t;
}

static void
codegen_begin(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  codegen(pic, cxt, pic_list_ref(pic, obj, 1), false);
  emit_n(pic, cxt, OP_POP);
  codegen(pic, cxt, pic_list_ref(pic, obj, 2), tailpos);
}

static void
codegen_quote(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  int pidx;

  obj = pic_list_ref(pic, obj, 1);
  switch (pic_type(obj)) {
  case PIC_TT_BOOL:
    emit_n(pic, cxt, (pic_true_p(obj) ? OP_PUSHTRUE : OP_PUSHFALSE));
    emit_ret(pic, cxt, tailpos);
    break;
  case PIC_TT_INT:
    emit_i(pic, cxt, OP_PUSHINT, pic_int(obj));
    emit_ret(pic, cxt, tailpos);
    break;
  case PIC_TT_NIL:
    emit_n(pic, cxt, OP_PUSHNIL);
    emit_ret(pic, cxt, tailpos);
    break;
  case PIC_TT_CHAR:
    emit_c(pic, cxt, OP_PUSHCHAR, pic_char(obj));
    emit_ret(pic, cxt, tailpos);
    break;
  default:
    check_pool_size(pic, cxt);
    pidx = (int)cxt->plen++;
    cxt->pool[pidx] = obj;
    emit_i(pic, cxt, OP_PUSHCONST, pidx);
    emit_ret(pic, cxt, tailpos);
    break;
  }
}

static bool
codegen_call_vm(pic_state *pic, codegen_context *cxt, pic_value proc, size_t len, bool tailpos)
{
  if (pic_sym_ptr(pic_list_ref(pic, proc, 0)) == pic->sGREF) {
    pic_sym *sym;

    sym = pic_sym_ptr(pic_list_ref(pic, proc, 1));

#define VM(uid, op)                             \
    if (sym == uid) {                           \
      emit_i(pic, cxt, op, len);                \
      emit_ret(pic, cxt, tailpos);              \
      return true;                              \
    }

    VM(pic->uCONS, OP_CONS)
    VM(pic->uCAR, OP_CAR)
    VM(pic->uCDR, OP_CDR)
    VM(pic->uNILP, OP_NILP)
    VM(pic->uSYMBOLP, OP_SYMBOLP)
    VM(pic->uPAIRP, OP_PAIRP)
    VM(pic->uNOT, OP_NOT)
    VM(pic->uEQ, OP_EQ)
    VM(pic->uLT, OP_LT)
    VM(pic->uLE, OP_LE)
    VM(pic->uGT, OP_GT)
    VM(pic->uGE, OP_GE)
    VM(pic->uADD, OP_ADD)
    VM(pic->uSUB, OP_SUB)
    VM(pic->uMUL, OP_MUL)
    VM(pic->uDIV, OP_DIV)
  }
  return false;
}

static void
codegen_call(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  int len = (int)pic_length(pic, obj);
  pic_value elt, it;

  pic_for_each (elt, pic_cdr(pic, obj), it) {
    codegen(pic, cxt, elt, false);
  }

  if (codegen_call_vm(pic, cxt, pic_list_ref(pic, obj, 1), len - 1, tailpos)) {
    return;
  }

  emit_i(pic, cxt, (tailpos ? OP_TAILCALL : OP_CALL), len - 1);
}

static void
codegen(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  pic_sym *sym;

  sym = pic_sym_ptr(pic_car(pic, obj));
  if (sym == pic->sGREF || sym == pic->sCREF || sym == pic->sLREF) {
    codegen_ref(pic, cxt, obj, tailpos);
  }
  else if (sym == pic->uSETBANG || sym == pic->uDEFINE) {
    codegen_set(pic, cxt, obj, tailpos);
  }
  else if (sym == pic->uLAMBDA) {
    codegen_lambda(pic, cxt, obj, tailpos);
  }
  else if (sym == pic->uIF) {
    codegen_if(pic, cxt, obj, tailpos);
  }
  else if (sym == pic->uBEGIN) {
    codegen_begin(pic, cxt, obj, tailpos);
  }
  else if (sym == pic->uQUOTE) {
    codegen_quote(pic, cxt, obj, tailpos);
  }
  else if (sym == pic->sCALL) {
    codegen_call(pic, cxt, obj, tailpos);
  }
  else {
    pic_errorf(pic, "codegen: unknown AST type ~s", obj);
  }
}

struct pic_irep *
pic_codegen(pic_state *pic, pic_value obj)
{
  pic_vec *empty = pic_make_vec(pic, 0);
  codegen_context c, *cxt = &c;

  codegen_context_init(pic, cxt, NULL, NULL, empty, empty, empty);

  codegen(pic, cxt, obj, true);

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

  /* optimize */
  obj = pic_optimize(pic, obj);
#if DEBUG
  fprintf(stdout, "## optimize completed\n");
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
