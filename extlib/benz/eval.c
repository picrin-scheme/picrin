/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "picrin/private/object.h"
#include "picrin/private/vm.h"
#include "picrin/private/state.h"

static pic_value pic_compile(pic_state *, pic_value);

#define EQ(sym, lit) (strcmp(pic_sym(pic, sym), lit) == 0)
#define S(lit) (pic_intern_lit(pic, lit))

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
  return pic_cons(pic, S("quote"), pic_cdr(pic, expr));
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

  return pic_list(pic, 3, S("lambda"), formal, body);
}

static pic_value
expand_define(pic_state *pic, pic_value expr, pic_value env, pic_value deferred)
{
  pic_value uid, val;

  uid = pic_add_identifier(pic, pic_list_ref(pic, expr, 1), env);

  shadow_macro(pic, uid);

  val = expand(pic, pic_list_ref(pic, expr, 2), env, deferred);

  return pic_list(pic, 3, S("define"), uid, val);
}

static pic_value
expand_defmacro(pic_state *pic, pic_value expr, pic_value env)
{
  pic_value uid, val;

  uid = pic_add_identifier(pic, pic_list_ref(pic, expr, 1), env);

  val = pic_call(pic, pic_compile(pic, pic_expand(pic, pic_list_ref(pic, expr, 2), env)), 0);
  if (! pic_proc_p(pic, val)) {
    pic_errorf(pic, "macro definition \"~s\" evaluates to non-procedure object", pic_list_ref(pic, expr, 1));
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

      if (EQ(functor, "define-macro")) {
        return expand_defmacro(pic, expr, env);
      }
      else if (EQ(functor, "lambda")) {
        return expand_defer(pic, expr, deferred);
      }
      else if (EQ(functor, "define")) {
        return expand_define(pic, expr, env, deferred);
      }
      else if (EQ(functor, "quote")) {
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

  deferred = pic_list(pic, 1, pic_nil_value(pic));

  v = expand(pic, expr, env, deferred);

  expand_deferred(pic, deferred, env);

  return v;
}

static pic_value
optimize_beta(pic_state *pic, pic_value expr)
{
  size_t ai = pic_enter(pic);
  pic_value functor, formals, args, tmp, val, it, defs;

  if (! pic_list_p(pic, expr))
    return expr;

  if (pic_nil_p(pic, expr))
    return expr;

  if (pic_sym_p(pic, pic_list_ref(pic, expr, 0))) {
    pic_value sym = pic_list_ref(pic, expr, 0);

    if (EQ(sym, "quote")) {
      return expr;
    } else if (EQ(sym, "lambda")) {
      return pic_list(pic, 3, S("lambda"), pic_list_ref(pic, expr, 1), optimize_beta(pic, pic_list_ref(pic, expr, 2)));
    }
  }

  tmp = pic_nil_value(pic);
  pic_for_each (val, expr, it) {
    pic_push(pic, optimize_beta(pic, val), tmp);
  }
  expr = pic_reverse(pic, tmp);

  pic_leave(pic, ai);
  pic_protect(pic, expr);

  functor = pic_list_ref(pic, expr, 0);
  if (pic_pair_p(pic, functor) && EQ(pic_car(pic, functor), "lambda")) {
    formals = pic_list_ref(pic, functor, 1);
    if (! pic_list_p(pic, formals))
      goto exit;              /* TODO: support ((lambda args x) 1 2) */
    args = pic_cdr(pic, expr);
    if (pic_length(pic, formals) != pic_length(pic, args))
      goto exit;
    defs = pic_nil_value(pic);
    pic_for_each (val, args, it) {
      pic_push(pic, pic_list(pic, 3, S("define"), pic_car(pic, formals), val), defs);
      formals = pic_cdr(pic, formals);
    }
    expr = pic_list_ref(pic, functor, 2);
    pic_for_each (val, defs, it) {
      expr = pic_list(pic, 3, S("begin"), val, expr);
    }
  }
 exit:

  pic_leave(pic, ai);
  pic_protect(pic, expr);
  return expr;
}

static pic_value
pic_optimize(pic_state *pic, pic_value expr)
{
  return optimize_beta(pic, expr);
}

typedef struct analyze_scope {
  int depth;
  pic_value rest;                   /* Nullable */
  pic_value args, locals, captures; /* rest args variable is counted as a local */
  pic_value defer;
  struct analyze_scope *up;
} analyze_scope;

static void
analyzer_scope_init(pic_state *pic, analyze_scope *scope, pic_value formal, analyze_scope *up)
{
  scope->args = pic_make_dict(pic);
  scope->locals = pic_make_dict(pic);
  scope->captures = pic_make_dict(pic);

  /* analyze formal */
  for (; pic_pair_p(pic, formal); formal = pic_cdr(pic, formal)) {
    pic_dict_set(pic, scope->args, pic_car(pic, formal), pic_true_value(pic));
  }
  if (pic_nil_p(pic, formal)) {
    scope->rest = pic_false_value(pic);
  }
  else {
    scope->rest = formal;
    pic_dict_set(pic, scope->locals, formal, pic_true_value(pic));
  }

  scope->up = up;
  scope->depth = up ? up->depth + 1 : 0;
  scope->defer = pic_list(pic, 1, pic_nil_value(pic));
}

static void
analyzer_scope_destroy(pic_state *PIC_UNUSED(pic), analyze_scope *PIC_UNUSED(scope))
{
  /* nothing here */
}

static bool
find_local_var(pic_state *pic, analyze_scope *scope, pic_value sym)
{
  return pic_dict_has(pic, scope->args, sym) || pic_dict_has(pic, scope->locals, sym) || scope->depth == 0;
}

static int
find_var(pic_state *pic, analyze_scope *scope, pic_value sym)
{
  int depth = 0;

  while (scope) {
    if (find_local_var(pic, scope, sym)) {
      if (depth > 0) {
        pic_dict_set(pic, scope->captures, sym, pic_true_value(pic)); /* capture! */
      }
      return depth;
    }
    depth++;
    scope = scope->up;
  }
  PIC_UNREACHABLE();
}

static void
define_var(pic_state *pic, analyze_scope *scope, pic_value sym)
{
  if (scope->depth > 0) {
    /* local */
    if (find_local_var(pic, scope, sym)) {
      pic_warnf(pic, "redefining variable: %s", pic_sym(pic, sym));
      return;
    }
    pic_dict_set(pic, scope->locals, sym, pic_true_value(pic));
  } else {
    /* global */
    if (pic_weak_has(pic, pic->globals, sym)) {
      pic_warnf(pic, "redefining variable: %s", pic_sym(pic, sym));
      return;
    }
    pic_weak_set(pic, pic->globals, sym, pic_invalid_value(pic));
  }
}

static pic_value analyze(pic_state *, analyze_scope *, pic_value);
static pic_value analyze_lambda(pic_state *, analyze_scope *, pic_value);

static pic_value
analyze_var(pic_state *pic, analyze_scope *scope, pic_value sym)
{
  int depth;

  depth = find_var(pic, scope, sym);

  if (depth == scope->depth) {
    return pic_list(pic, 2, S("gref"), sym);
  } else if (depth == 0) {
    return pic_list(pic, 2, S("lref"), sym);
  } else {
    return pic_list(pic, 3, S("cref"), pic_int_value(pic, depth), sym);
  }
}

static pic_value
analyze_defer(pic_state *pic, analyze_scope *scope, pic_value form)
{
  pic_value skel = pic_cons(pic, pic_invalid_value(pic), pic_invalid_value(pic));

  pic_set_car(pic, scope->defer, pic_cons(pic, pic_cons(pic, form, skel), pic_car(pic, scope->defer)));

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
  pic_value rest;
  pic_value args, locals, captures, key;
  int i, j, it;

  formals = pic_list_ref(pic, form, 1);
  body = pic_list_ref(pic, form, 2);

  analyzer_scope_init(pic, scope, formals, up);

  /* analyze body */
  body = analyze(pic, scope, body);
  analyze_deferred(pic, scope);

  args = pic_make_vec(pic, pic_dict_size(pic, scope->args), NULL);
  for (i = 0; pic_pair_p(pic, formals); formals = pic_cdr(pic, formals), i++) {
    pic_vec_set(pic, args, i, pic_car(pic, formals));
  }

  rest = scope->rest;

  locals = pic_make_vec(pic, pic_dict_size(pic, scope->locals), NULL);
  j = 0;
  if (pic_sym_p(pic, scope->rest)) {
    pic_vec_set(pic, locals, j++, scope->rest);
  }
  it = 0;
  while (pic_dict_next(pic, scope->locals, &it, &key, NULL)) {
    if (pic_eq_p(pic, key, rest))
      continue;
    pic_vec_set(pic, locals, j++, key);
  }

  captures = pic_make_vec(pic, pic_dict_size(pic, scope->captures), NULL);
  it = 0;
  j = 0;
  while (pic_dict_next(pic, scope->captures, &it, &key, NULL)) {
    pic_vec_set(pic, captures, j++, key);
  }

  analyzer_scope_destroy(pic, scope);

  return pic_list(pic, 6, S("lambda"), rest, args, locals, captures, body);
}

static pic_value
analyze_list(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  pic_value seq = pic_nil_value(pic), val, it;

  pic_for_each (val, obj, it) {
    pic_push(pic, analyze(pic, scope, val), seq);
  }

  return pic_reverse(pic, seq);
}

static pic_value
analyze_define(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  define_var(pic, scope, pic_list_ref(pic, obj, 1));

  return pic_cons(pic, pic_car(pic, obj), analyze_list(pic, scope, pic_cdr(pic, obj)));
}

static pic_value
analyze_call(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  return pic_cons(pic, S("call"), analyze_list(pic, scope, obj));
}

static pic_value
analyze_node(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  switch (pic_type(pic, obj)) {
  case PIC_TYPE_SYMBOL: {
    return analyze_var(pic, scope, obj);
  }
  case PIC_TYPE_PAIR: {
    pic_value proc;

    if (! pic_list_p(pic, obj)) {
      pic_errorf(pic, "invalid expression given: ~s", obj);
    }

    proc = pic_list_ref(pic, obj, 0);
    if (pic_sym_p(pic, proc)) {
      pic_value sym = proc;

      if (EQ(sym, "define")) {
        return analyze_define(pic, scope, obj);
      }
      else if (EQ(sym, "lambda")) {
        return analyze_defer(pic, scope, obj);
      }
      else if (EQ(sym, "quote")) {
        return obj;
      }
      else if (EQ(sym, "begin") || EQ(sym, "set!") || EQ(sym, "if")) {
        return pic_cons(pic, pic_car(pic, obj), analyze_list(pic, scope, pic_cdr(pic, obj)));
      }
    }

    return analyze_call(pic, scope, obj);
  }
  default:
    return pic_list(pic, 2, S("quote"), obj);
  }
}

static pic_value
analyze(pic_state *pic, analyze_scope *scope, pic_value obj)
{
  size_t ai = pic_enter(pic);
  pic_value res;

  res = analyze_node(pic, scope, obj);

  pic_leave(pic, ai);
  pic_protect(pic, res);
  return res;
}

static pic_value
pic_analyze(pic_state *pic, pic_value obj)
{
  analyze_scope s, *scope = &s;

  analyzer_scope_init(pic, scope, pic_nil_value(pic), NULL);

  obj = analyze(pic, scope, obj);

  analyze_deferred(pic, scope);

  analyzer_scope_destroy(pic, scope);
  return obj;
}

typedef struct codegen_context {
  /* rest args variable is counted as a local */
  pic_value rest;
  pic_value args, locals, captures;
  /* actual bit code sequence */
  struct code *code;
  size_t clen, ccapa;
  /* child ireps */
  struct irep **irep;
  size_t ilen, icapa;
  /* constant object pool */
  int *ints;
  size_t klen, kcapa;
  double *nums;
  size_t flen, fcapa;
  struct object **pool;
  size_t plen, pcapa;

  struct codegen_context *up;
} codegen_context;

static void create_activation(pic_state *, codegen_context *);

static void
codegen_context_init(pic_state *pic, codegen_context *cxt, codegen_context *up, pic_value rest, pic_value args, pic_value locals, pic_value captures)
{
  cxt->up = up;
  cxt->rest = rest;

  cxt->args = args;
  cxt->locals = locals;
  cxt->captures = captures;

  cxt->code = pic_calloc(pic, PIC_ISEQ_SIZE, sizeof(struct code));
  cxt->clen = 0;
  cxt->ccapa = PIC_ISEQ_SIZE;

  cxt->irep = pic_calloc(pic, PIC_IREP_SIZE, sizeof(struct irep *));
  cxt->ilen = 0;
  cxt->icapa = PIC_IREP_SIZE;

  cxt->pool = pic_calloc(pic, PIC_POOL_SIZE, sizeof(struct object *));
  cxt->plen = 0;
  cxt->pcapa = PIC_POOL_SIZE;

  cxt->ints = pic_calloc(pic, PIC_POOL_SIZE, sizeof(int));
  cxt->klen = 0;
  cxt->kcapa = PIC_POOL_SIZE;

  cxt->nums = pic_calloc(pic, PIC_POOL_SIZE, sizeof(double));
  cxt->flen = 0;
  cxt->fcapa = PIC_POOL_SIZE;

  create_activation(pic, cxt);
}

static struct irep *
codegen_context_destroy(pic_state *pic, codegen_context *cxt)
{
  struct irep *irep;

  /* create irep */
  irep = pic_malloc(pic, sizeof(struct irep));
  irep->refc = 1;
  irep->varg = pic_sym_p(pic, cxt->rest);
  irep->argc = pic_vec_len(pic, cxt->args) + 1;
  irep->localc = pic_vec_len(pic, cxt->locals);
  irep->capturec = pic_vec_len(pic, cxt->captures);
  irep->code = pic_realloc(pic, cxt->code, sizeof(struct code) * cxt->clen);
  irep->irep = pic_realloc(pic, cxt->irep, sizeof(struct irep *) * cxt->ilen);
  irep->ints = pic_realloc(pic, cxt->ints, sizeof(int) * cxt->klen);
  irep->nums = pic_realloc(pic, cxt->nums, sizeof(double) * cxt->flen);
  irep->pool = pic_realloc(pic, cxt->pool, sizeof(struct object *) * cxt->plen);
  irep->ncode = cxt->clen;
  irep->nirep = cxt->ilen;
  irep->nints = cxt->klen;
  irep->nnums = cxt->flen;
  irep->npool = cxt->plen;

  irep->list.next = pic->ireps.next;
  irep->list.prev = &pic->ireps;
  irep->list.next->prev = &irep->list;
  irep->list.prev->next = &irep->list;

  return irep;
}

#define check_size(pic, cxt, x, name, type) do {                        \
    if (cxt->x##len >= cxt->x##capa) {                                  \
      cxt->x##capa *= 2;                                                \
      cxt->name = pic_realloc(pic, cxt->name, sizeof(type) * cxt->x##capa); \
    }                                                                   \
  } while (0)

#define check_code_size(pic, cxt) check_size(pic, cxt, c, code, struct code)
#define check_irep_size(pic, cxt) check_size(pic, cxt, i, irep, struct irep *)
#define check_pool_size(pic, cxt) check_size(pic, cxt, p, pool, struct object *)
#define check_ints_size(pic, cxt) check_size(pic, cxt, k, ints, int)
#define check_nums_size(pic, cxt) check_size(pic, cxt, f, nums, double)

#define emit_n(pic, cxt, ins) do {              \
    check_code_size(pic, cxt);                  \
    cxt->code[cxt->clen].insn = ins;            \
    cxt->clen++;                                \
  } while (0)                                   \

#define emit_i(pic, cxt, ins, I) do {           \
    check_code_size(pic, cxt);                  \
    cxt->code[cxt->clen].insn = ins;            \
    cxt->code[cxt->clen].a = I;                 \
    cxt->clen++;                                \
  } while (0)                                   \

#define emit_r(pic, cxt, ins, D, I) do {        \
    check_code_size(pic, cxt);                  \
    cxt->code[cxt->clen].insn = ins;            \
    cxt->code[cxt->clen].a = D;                 \
    cxt->code[cxt->clen].b = I;                 \
    cxt->clen++;                                \
  } while (0)                                   \

#define emit_ret(pic, cxt, tailpos) if (tailpos) emit_n(pic, cxt, OP_RET)

static int
index_capture(pic_state *pic, codegen_context *cxt, pic_value sym, int depth)
{
  int i;

  while (depth-- > 0) {
    cxt = cxt->up;
  }

  for (i = 0; i < pic_vec_len(pic, cxt->captures); ++i) {
    if (pic_eq_p(pic, sym, pic_vec_ref(pic, cxt->captures, i)))
      return i;
  }
  return -1;
}

static int
index_local(pic_state *pic, codegen_context *cxt, pic_value sym)
{
  int i, offset;

  offset = 1;
  for (i = 0; i < pic_vec_len(pic, cxt->args); ++i) {
    if (pic_eq_p(pic, sym, pic_vec_ref(pic, cxt->args, i)))
      return i + offset;
  }
  offset += i;
  for (i = 0; i < pic_vec_len(pic, cxt->locals); ++i) {
    if (pic_eq_p(pic, sym, pic_vec_ref(pic, cxt->locals, i)))
      return i + offset;
  }
  return -1;
}

static int
index_global(pic_state *pic, codegen_context *cxt, pic_value name)
{
  int pidx;

  check_pool_size(pic, cxt);
  pidx = (int)cxt->plen++;
  cxt->pool[pidx] = (struct object *)pic_sym_ptr(pic, name);

  return pidx;
}

static void
create_activation(pic_state *pic, codegen_context *cxt)
{
  int i, n;

  for (i = 0; i < pic_vec_len(pic, cxt->captures); ++i) {
    pic_value sym = pic_vec_ref(pic, cxt->captures, i);
    n = index_local(pic, cxt, sym);
    assert(n != -1);
    if (n <= pic_vec_len(pic, cxt->args) || pic_eq_p(pic, sym, cxt->rest)) {
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
  pic_value sym;

  sym = pic_car(pic, obj);
  if (EQ(sym, "gref")) {
    pic_value name;

    name = pic_list_ref(pic, obj, 1);
    emit_i(pic, cxt, OP_GREF, index_global(pic, cxt, name));
    emit_ret(pic, cxt, tailpos);
  }
  else if (EQ(sym, "cref")) {
    pic_value name;
    int depth;

    depth = pic_int(pic, pic_list_ref(pic, obj, 1));
    name  = pic_list_ref(pic, obj, 2);
    emit_r(pic, cxt, OP_CREF, depth, index_capture(pic, cxt, name, depth));
    emit_ret(pic, cxt, tailpos);
  }
  else if (EQ(sym, "lref")) {
    pic_value name;
    int i;

    name = pic_list_ref(pic, obj, 1);
    if ((i = index_capture(pic, cxt, name, 0)) != -1) {
      emit_i(pic, cxt, OP_LREF, i + pic_vec_len(pic, cxt->args) + pic_vec_len(pic, cxt->locals) + 1);
      emit_ret(pic, cxt, tailpos);
    } else {
      emit_i(pic, cxt, OP_LREF, index_local(pic, cxt, name));
      emit_ret(pic, cxt, tailpos);
    }
  }
}

static void
codegen_set(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  pic_value var, val;
  pic_value type;

  val = pic_list_ref(pic, obj, 2);
  codegen(pic, cxt, val, false);

  var = pic_list_ref(pic, obj, 1);
  type = pic_list_ref(pic, var, 0);
  if (EQ(type, "gref")) {
    pic_value name;

    name = pic_list_ref(pic, var, 1);
    emit_i(pic, cxt, OP_GSET, index_global(pic, cxt, name));
    emit_ret(pic, cxt, tailpos);
  }
  else if (EQ(type, "cref")) {
    pic_value name;
    int depth;

    depth = pic_int(pic, pic_list_ref(pic, var, 1));
    name  = pic_list_ref(pic, var, 2);
    emit_r(pic, cxt, OP_CSET, depth, index_capture(pic, cxt, name, depth));
    emit_ret(pic, cxt, tailpos);
  }
  else if (EQ(type, "lref")) {
    pic_value name;
    int i;

    name = pic_list_ref(pic, var, 1);
    if ((i = index_capture(pic, cxt, name, 0)) != -1) {
      emit_i(pic, cxt, OP_LSET, i + pic_vec_len(pic, cxt->args) + pic_vec_len(pic, cxt->locals) + 1);
      emit_ret(pic, cxt, tailpos);
    } else {
      emit_i(pic, cxt, OP_LSET, index_local(pic, cxt, name));
      emit_ret(pic, cxt, tailpos);
    }
  }
}

static void
codegen_lambda(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  codegen_context c, *inner_cxt = &c;
  pic_value rest, body;
  pic_value args, locals, captures;

  check_irep_size(pic, cxt);

  /* extract arguments */
  rest = pic_list_ref(pic, obj, 1);
  args = pic_list_ref(pic, obj, 2);
  locals = pic_list_ref(pic, obj, 3);
  captures = pic_list_ref(pic, obj, 4);
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

  cxt->code[s].a = (int)cxt->clen - s;

  /* if true branch */
  codegen(pic, cxt, pic_list_ref(pic, obj, 2), tailpos);
  cxt->code[t].a = (int)cxt->clen - t;
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
  switch (pic_type(pic, obj)) {
  case PIC_TYPE_UNDEF:
    emit_n(pic, cxt, OP_PUSHUNDEF);
    break;
  case PIC_TYPE_TRUE:
    emit_n(pic, cxt, OP_PUSHTRUE);
    break;
  case PIC_TYPE_FALSE:
    emit_n(pic, cxt, OP_PUSHFALSE);
    break;
  case PIC_TYPE_INT:
    check_ints_size(pic, cxt);
    pidx = (int)cxt->klen++;
    cxt->ints[pidx] = pic_int(pic, obj);
    emit_i(pic, cxt, OP_PUSHINT, pidx);
    break;
  case PIC_TYPE_FLOAT:
    check_nums_size(pic, cxt);
    pidx = (int)cxt->flen++;
    cxt->nums[pidx] = pic_float(pic, obj);
    emit_i(pic, cxt, OP_PUSHFLOAT, pidx);
    break;
  case PIC_TYPE_NIL:
    emit_n(pic, cxt, OP_PUSHNIL);
    break;
  case PIC_TYPE_EOF:
    emit_n(pic, cxt, OP_PUSHEOF);
    break;
  case PIC_TYPE_CHAR:
    check_ints_size(pic, cxt);
    pidx = (int)cxt->klen++;
    cxt->ints[pidx] = pic_char(pic, obj);
    emit_i(pic, cxt, OP_PUSHCHAR, pidx);
    break;
  default:
    assert(pic_obj_p(pic,obj));
    check_pool_size(pic, cxt);
    pidx = (int)cxt->plen++;
    cxt->pool[pidx] = pic_obj_ptr(obj);
    emit_i(pic, cxt, OP_PUSHCONST, pidx);
    break;
  }
  emit_ret(pic, cxt, tailpos);
}

#define VM(name, op)                            \
  if (EQ(sym, name)) {                          \
    emit_i(pic, cxt, op, len - 1);              \
    emit_ret(pic, cxt, tailpos);                \
    return;                                     \
  }

static void
codegen_call(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  int len = (int)pic_length(pic, obj);
  pic_value elt, it, functor;

  pic_for_each (elt, pic_cdr(pic, obj), it) {
    codegen(pic, cxt, elt, false);
  }

  functor = pic_list_ref(pic, obj, 1);
  if (EQ(pic_list_ref(pic, functor, 0), "gref")) {
    pic_value sym;

    sym = pic_list_ref(pic, functor, 1);

    VM("cons", OP_CONS)
    VM("car", OP_CAR)
    VM("cdr", OP_CDR)
    VM("null?", OP_NILP)
    VM("symbol?", OP_SYMBOLP)
    VM("pair?", OP_PAIRP)
    VM("not", OP_NOT)
    VM("=", OP_EQ)
    VM("<", OP_LT)
    VM("<=", OP_LE)
    VM(">", OP_GT)
    VM(">=", OP_GE)
    VM("+", OP_ADD)
    VM("-", OP_SUB)
    VM("*", OP_MUL)
    VM("/", OP_DIV)
  }

  emit_i(pic, cxt, (tailpos ? OP_TAILCALL : OP_CALL), len - 1);
}

static void
codegen(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  pic_value sym;

  sym = pic_car(pic, obj);
  if (EQ(sym, "gref") || EQ(sym, "cref") || EQ(sym, "lref")) {
    codegen_ref(pic, cxt, obj, tailpos);
  }
  else if (EQ(sym, "set!") || EQ(sym, "define")) {
    codegen_set(pic, cxt, obj, tailpos);
  }
  else if (EQ(sym, "lambda")) {
    codegen_lambda(pic, cxt, obj, tailpos);
  }
  else if (EQ(sym, "if")) {
    codegen_if(pic, cxt, obj, tailpos);
  }
  else if (EQ(sym, "begin")) {
    codegen_begin(pic, cxt, obj, tailpos);
  }
  else if (EQ(sym, "quote")) {
    codegen_quote(pic, cxt, obj, tailpos);
  }
  else if (EQ(sym, "call")) {
    codegen_call(pic, cxt, obj, tailpos);
  }
  else {
    pic_errorf(pic, "codegen: unknown AST type ~s", obj);
  }
}

static struct irep *
pic_codegen(pic_state *pic, pic_value obj)
{
  pic_value empty = pic_make_vec(pic, 0, NULL);
  codegen_context c, *cxt = &c;

  codegen_context_init(pic, cxt, NULL, pic_false_value(pic), empty, empty, empty);

  codegen(pic, cxt, obj, true);

  return codegen_context_destroy(pic, cxt);
}

#define SAVE(pic, ai, obj) pic_leave(pic, ai); pic_protect(pic, obj)

static pic_value
pic_compile(pic_state *pic, pic_value obj)
{
  struct irep *irep;
  pic_value proc;
  size_t ai = pic_enter(pic);

#if 0
  fprintf(stdout, "ai = %zu\n", pic_enter(pic));

  fprintf(stdout, "# input expression\n");
  pic_write(pic, obj);
  fprintf(stdout, "\n");

  fprintf(stdout, "ai = %zu\n", pic_enter(pic));
#endif

  /* optimize */
  obj = pic_optimize(pic, obj);
#if 0
  fprintf(stdout, "## optimize completed\n");
  pic_write(pic, obj);
  fprintf(stdout, "\n");
  fprintf(stdout, "ai = %zu\n", pic_enter(pic));
#endif

  SAVE(pic, ai, obj);

  /* analyze */
  obj = pic_analyze(pic, obj);
#if 0
  fprintf(stdout, "## analyzer completed\n");
  pic_write(pic, obj);
  fprintf(stdout, "\n");
  fprintf(stdout, "ai = %zu\n", pic_enter(pic));
#endif

  SAVE(pic, ai, obj);

  /* codegen */
  irep = pic_codegen(pic, obj);

  proc = pic_make_proc_irep(pic, irep, NULL);

  pic_irep_decref(pic, irep);

  return proc;
}

pic_value
pic_eval(pic_state *pic, pic_value program, const char *lib)
{
  const char *prev_lib = pic_current_library(pic);
  pic_value env, r;

  env = pic_library_environment(pic, lib);

  pic_in_library(pic, lib);
  pic_try {
    r = pic_call(pic, pic_compile(pic, pic_expand(pic, program, env)), 0);
  }
  pic_catch {
    pic_in_library(pic, prev_lib);
    pic_raise(pic, pic_err(pic));
  }
  pic_in_library(pic, prev_lib);

  return r;
}

static pic_value
pic_eval_eval(pic_state *pic)
{
  pic_value program;
  const char *str;

  pic_get_args(pic, "oz", &program, &str);

  return pic_eval(pic, program, str);
}

void
pic_init_eval(pic_state *pic)
{
  pic_defun(pic, "eval", pic_eval_eval);
}
