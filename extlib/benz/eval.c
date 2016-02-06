/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/opcode.h"

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

    if (sym == pic->sQUOTE) {
      return expr;
    } else if (sym == pic->sLAMBDA) {
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
  if (pic_pair_p(functor) && pic_eq_p(pic_car(pic, functor), pic_obj_value(pic->sLAMBDA))) {
    formals = pic_list_ref(pic, functor, 1);
    if (! pic_list_p(formals))
      goto exit;              /* TODO: support ((lambda args x) 1 2) */
    args = pic_cdr(pic, expr);
    if (pic_length(pic, formals) != pic_length(pic, args))
      goto exit;
    defs = pic_nil_value();
    pic_for_each (val, args, it) {
      pic_push(pic, pic_list3(pic, pic_obj_value(pic->sDEFINE), pic_car(pic, formals), val), defs);
      formals = pic_cdr(pic, formals);
    }
    expr = pic_list_ref(pic, functor, 2);
    pic_for_each (val, defs, it) {
      expr = pic_list3(pic, pic_obj_value(pic->sBEGIN), val, expr);
    }
  }
 exit:

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, expr);
  return expr;
}

static pic_value
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

#define GREF pic_intern(pic, "gref")
#define LREF pic_intern(pic, "lref")
#define CREF pic_intern(pic, "cref")
#define CALL pic_intern(pic, "call")

static pic_value
analyze_var(pic_state *pic, analyze_scope *scope, pic_sym *sym)
{
  int depth;

  depth = find_var(pic, scope, sym);

  if (depth == scope->depth) {
    return pic_list2(pic, pic_obj_value(GREF), pic_obj_value(sym));
  } else if (depth == 0) {
    return pic_list2(pic, pic_obj_value(LREF), pic_obj_value(sym));
  } else {
    return pic_list3(pic, pic_obj_value(CREF), pic_int_value(depth), pic_obj_value(sym));
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
  int i, j;
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

  return pic_list6(pic, pic_obj_value(pic->sLAMBDA), rest, pic_obj_value(args), pic_obj_value(locals), pic_obj_value(captures), body);
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
  return pic_cons(pic, pic_obj_value(CALL), analyze_list(pic, scope, obj));
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

      if (sym == pic->sDEFINE) {
        return analyze_define(pic, scope, obj);
      }
      else if (sym == pic->sLAMBDA) {
        return analyze_defer(pic, scope, obj);
      }
      else if (sym == pic->sQUOTE) {
        return obj;
      }
      else if (sym == pic->sBEGIN || sym == pic->sSETBANG || sym == pic->sIF) {
        return pic_cons(pic, pic_car(pic, obj), analyze_list(pic, scope, pic_cdr(pic, obj)));
      }
    }

    return analyze_call(pic, scope, obj);
  }
  default:
    return pic_list2(pic, pic_obj_value(pic->sQUOTE), obj);
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

static pic_value
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
  union irep_node *irep;
  size_t ilen, icapa;
  /* constant object pool */
  int *ints;
  size_t klen, kcapa;
  double *nums;
  size_t flen, fcapa;
  struct pic_object **pool;
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

  cxt->irep = pic_calloc(pic, PIC_IREP_SIZE, sizeof(union irep_node));
  cxt->ilen = 0;
  cxt->icapa = PIC_IREP_SIZE;

  cxt->pool = pic_calloc(pic, PIC_POOL_SIZE, sizeof(struct pic_object *));
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

static struct pic_irep *
codegen_context_destroy(pic_state *pic, codegen_context *cxt)
{
  struct pic_irep *irep;

  /* create irep */
  irep = pic_malloc(pic, sizeof(struct pic_irep));
  irep->refc = 1;
  irep->varg = cxt->rest != NULL;
  irep->argc = (int)cxt->args->len + 1;
  irep->localc = (int)cxt->locals->len;
  irep->capturec = (int)cxt->captures->len;
  irep->u.s.code = pic_realloc(pic, cxt->code, sizeof(pic_code) * cxt->clen);
  irep->u.s.irep = pic_realloc(pic, cxt->irep, sizeof(union irep_node) * cxt->ilen);
  irep->u.s.ints = pic_realloc(pic, cxt->ints, sizeof(int) * cxt->klen);
  irep->u.s.nums = pic_realloc(pic, cxt->nums, sizeof(double) * cxt->flen);
  irep->pool = pic_realloc(pic, cxt->pool, sizeof(struct pic_object *) * cxt->plen);
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

#define check_code_size(pic, cxt) check_size(pic, cxt, c, code, pic_code)
#define check_irep_size(pic, cxt) check_size(pic, cxt, i, irep, struct pic_irep *)
#define check_pool_size(pic, cxt) check_size(pic, cxt, p, pool, struct pic_object *)
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
index_capture(codegen_context *cxt, pic_sym *sym, int depth)
{
  int i;

  while (depth-- > 0) {
    cxt = cxt->up;
  }

  for (i = 0; i < cxt->captures->len; ++i) {
    if (pic_sym_ptr(cxt->captures->data[i]) == sym)
      return i;
  }
  return -1;
}

static int
index_local(codegen_context *cxt, pic_sym *sym)
{
  int i, offset;

  offset = 1;
  for (i = 0; i < cxt->args->len; ++i) {
    if (pic_sym_ptr(cxt->args->data[i]) == sym)
      return i + offset;
  }
  offset += i;
  for (i = 0; i < cxt->locals->len; ++i) {
    if (pic_sym_ptr(cxt->locals->data[i]) == sym)
      return i + offset;
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
  cxt->pool[pidx] = (struct pic_object *)(slot);

  return pidx;
}

static void
create_activation(pic_state *pic, codegen_context *cxt)
{
  int i, n;

  for (i = 0; i < cxt->captures->len; ++i) {
    n = index_local(cxt, pic_sym_ptr(cxt->captures->data[i]));
    assert(n != -1);
    if (n <= cxt->args->len || cxt->rest == pic_sym_ptr(cxt->captures->data[i])) {
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
  if (sym == GREF) {
    pic_sym *name;

    name = pic_sym_ptr(pic_list_ref(pic, obj, 1));
    emit_i(pic, cxt, OP_GREF, index_global(pic, cxt, name));
    emit_ret(pic, cxt, tailpos);
  }
  else if (sym == CREF) {
    pic_sym *name;
    int depth;

    depth = pic_int(pic_list_ref(pic, obj, 1));
    name  = pic_sym_ptr(pic_list_ref(pic, obj, 2));
    emit_r(pic, cxt, OP_CREF, depth, index_capture(cxt, name, depth));
    emit_ret(pic, cxt, tailpos);
  }
  else if (sym == LREF) {
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
  if (type == GREF) {
    pic_sym *name;

    name = pic_sym_ptr(pic_list_ref(pic, var, 1));
    emit_i(pic, cxt, OP_GSET, index_global(pic, cxt, name));
    emit_ret(pic, cxt, tailpos);
  }
  else if (type == CREF) {
    pic_sym *name;
    int depth;

    depth = pic_int(pic_list_ref(pic, var, 1));
    name  = pic_sym_ptr(pic_list_ref(pic, var, 2));
    emit_r(pic, cxt, OP_CSET, depth, index_capture(cxt, name, depth));
    emit_ret(pic, cxt, tailpos);
  }
  else if (type == LREF) {
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
  cxt->irep[cxt->ilen].i = codegen_context_destroy(pic, inner_cxt);

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
  switch (pic_type(obj)) {
  case PIC_TT_UNDEF:
    emit_n(pic, cxt, OP_PUSHUNDEF);
    break;
  case PIC_TT_BOOL:
    emit_n(pic, cxt, (pic_true_p(obj) ? OP_PUSHTRUE : OP_PUSHFALSE));
    break;
  case PIC_TT_INT:
    check_ints_size(pic, cxt);
    pidx = (int)cxt->klen++;
    cxt->ints[pidx] = pic_int(obj);
    emit_i(pic, cxt, OP_PUSHINT, pidx);
    break;
  case PIC_TT_FLOAT:
    check_nums_size(pic, cxt);
    pidx = (int)cxt->flen++;
    cxt->nums[pidx] = pic_float(obj);
    emit_i(pic, cxt, OP_PUSHFLOAT, pidx);
    break;
  case PIC_TT_NIL:
    emit_n(pic, cxt, OP_PUSHNIL);
    break;
  case PIC_TT_EOF:
    emit_n(pic, cxt, OP_PUSHEOF);
    break;
  case PIC_TT_CHAR:
    check_ints_size(pic, cxt);
    pidx = (int)cxt->klen++;
    cxt->ints[pidx] = pic_char(obj);
    emit_i(pic, cxt, OP_PUSHCHAR, pidx);
    break;
  default:
    assert(pic_obj_p(obj));
    check_pool_size(pic, cxt);
    pidx = (int)cxt->plen++;
    cxt->pool[pidx] = pic_obj_ptr(obj);
    emit_i(pic, cxt, OP_PUSHCONST, pidx);
    break;
  }
  emit_ret(pic, cxt, tailpos);
}

#define VM(uid, op)                             \
    if (sym == uid) {                           \
      emit_i(pic, cxt, op, len - 1);            \
      emit_ret(pic, cxt, tailpos);              \
      return;                                   \
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
  if (pic_sym_ptr(pic_list_ref(pic, functor, 0)) == GREF) {
    pic_sym *sym;

    sym = pic_sym_ptr(pic_list_ref(pic, functor, 1));

    VM(pic->sCONS, OP_CONS)
    VM(pic->sCAR, OP_CAR)
    VM(pic->sCDR, OP_CDR)
    VM(pic->sNILP, OP_NILP)
    VM(pic->sSYMBOLP, OP_SYMBOLP)
    VM(pic->sPAIRP, OP_PAIRP)
    VM(pic->sNOT, OP_NOT)
    VM(pic->sEQ, OP_EQ)
    VM(pic->sLT, OP_LT)
    VM(pic->sLE, OP_LE)
    VM(pic->sGT, OP_GT)
    VM(pic->sGE, OP_GE)
    VM(pic->sADD, OP_ADD)
    VM(pic->sSUB, OP_SUB)
    VM(pic->sMUL, OP_MUL)
    VM(pic->sDIV, OP_DIV)
  }

  emit_i(pic, cxt, (tailpos ? OP_TAILCALL : OP_CALL), len - 1);
}

static void
codegen(pic_state *pic, codegen_context *cxt, pic_value obj, bool tailpos)
{
  pic_sym *sym;

  sym = pic_sym_ptr(pic_car(pic, obj));
  if (sym == GREF || sym == CREF || sym == LREF) {
    codegen_ref(pic, cxt, obj, tailpos);
  }
  else if (sym == pic->sSETBANG || sym == pic->sDEFINE) {
    codegen_set(pic, cxt, obj, tailpos);
  }
  else if (sym == pic->sLAMBDA) {
    codegen_lambda(pic, cxt, obj, tailpos);
  }
  else if (sym == pic->sIF) {
    codegen_if(pic, cxt, obj, tailpos);
  }
  else if (sym == pic->sBEGIN) {
    codegen_begin(pic, cxt, obj, tailpos);
  }
  else if (sym == pic->sQUOTE) {
    codegen_quote(pic, cxt, obj, tailpos);
  }
  else if (sym == CALL) {
    codegen_call(pic, cxt, obj, tailpos);
  }
  else {
    pic_errorf(pic, "codegen: unknown AST type ~s", obj);
  }
}

static struct pic_irep *
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
pic_compile(pic_state *pic, pic_value obj)
{
  struct pic_irep *irep;
  struct pic_proc *proc;
  size_t ai = pic_gc_arena_preserve(pic);

#if DEBUG
  fprintf(stdout, "ai = %zu\n", pic_gc_arena_preserve(pic));

  fprintf(stdout, "# input expression\n");
  pic_write(pic, obj);
  fprintf(stdout, "\n");

  fprintf(stdout, "ai = %zu\n", pic_gc_arena_preserve(pic));
#endif

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

  proc = pic_make_proc_irep(pic, irep, NULL);

  pic_irep_decref(pic, irep);

  return proc;
}

pic_value
pic_eval(pic_state *pic, pic_value program, struct pic_lib *lib)
{
  pic_value r;

  pic_try {
    pic->prev_lib = pic->lib;
    pic->lib = lib;

    r = pic_apply0(pic, pic_compile(pic, pic_expand(pic, program, lib->env)));
  }
  pic_catch {
    pic->lib = pic->prev_lib;
    pic_raise(pic, pic->err);
  }

  return r;
}

static pic_value
pic_eval_eval(pic_state *pic)
{
  pic_value program, lib;

  pic_get_args(pic, "oo", &program, &lib);

  pic_assert_type(pic, lib, lib);

  return pic_eval(pic, program, pic_lib_ptr(lib));
}

void
pic_init_eval(pic_state *pic)
{
  pic_defun(pic, "eval", pic_eval_eval);
}
