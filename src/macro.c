/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/proc.h"
#include "picrin/macro.h"
#include "picrin/lib.h"
#include "picrin/error.h"

static pic_value macroexpand(pic_state *, pic_value, struct pic_senv *);
static pic_value macroexpand_list(pic_state *, pic_value, struct pic_senv *);

static struct pic_senv *
senv_new(pic_state *pic, struct pic_senv *up)
{
  struct pic_senv *senv;

  senv = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
  senv->up = up;
  senv->name = xh_new_int();

  return senv;
}

static struct pic_senv *
senv_new_local(pic_state *pic, pic_value formals, struct pic_senv *up)
{
  struct pic_senv *senv;
  pic_value a;
  pic_sym sym;

  senv = senv_new(pic, up);

  for (a = formals; pic_pair_p(a); a = pic_cdr(pic, a)) {
    pic_value v = pic_car(pic, a);

    if (! pic_sym_p(v)) {
      v = macroexpand(pic, v, up);
    }
    if (! pic_sym_p(v)) {
      pic_error(pic, "syntax error");
    }
    sym = pic_sym(v);
    xh_put_int(senv->name, sym, pic_gensym(pic, sym));
  }
  if (! pic_sym_p(a)) {
    a = macroexpand(pic, a, up);
  }
  if (pic_sym_p(a)) {
    sym = pic_sym(a);
    xh_put_int(senv->name, sym, pic_gensym(pic, sym));
  }
  else if (! pic_nil_p(a)) {
    pic_error(pic, "syntax error");
  }
  return senv;
}

struct pic_macro *
macro_new(pic_state *pic, struct pic_proc *proc, struct pic_senv *mac_env)
{
  struct pic_macro *mac;

  mac = (struct pic_macro *)pic_obj_alloc(pic, sizeof(struct pic_macro), PIC_TT_MACRO);
  mac->senv = mac_env;
  mac->proc = proc;
  return mac;
}

static struct pic_sc *
sc_new(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  struct pic_sc *sc;

  sc = (struct pic_sc *)pic_obj_alloc(pic, sizeof(struct pic_sc), PIC_TT_SC);
  sc->expr = expr;
  sc->senv = senv;
  return sc;
}

static bool
identifier_p(pic_value obj)
{
  if (pic_sym_p(obj)) {
    return true;
  }
  if (pic_sc_p(obj)) {
    return identifier_p(pic_sc(obj)->expr);
  }
  return false;
}

static bool
identifier_eq_p(pic_state *pic, struct pic_senv *e1, pic_value x, struct pic_senv *e2, pic_value y)
{
  if (! (identifier_p(x) && identifier_p(y))) {
    return false;
  }

  x = macroexpand(pic, x, e1);
  y = macroexpand(pic, y, e2);

  return pic_eq_p(x, y);
}

void
pic_import(pic_state *pic, pic_value spec)
{
  struct pic_lib *lib;
  xh_iter it;

  lib = pic_find_library(pic, spec);
  if (! lib) {
    pic_error(pic, "library not found");
  }
  for (xh_begin(lib->exports, &it); ! xh_isend(&it); xh_next(&it)) {

#if DEBUG
    assert(it.e->val >= 0);
    printf("* importing %s as %s\n",
           pic_symbol_name(pic, (long)it.e->key),
           pic_symbol_name(pic, it.e->val));
#endif

    xh_put_int(pic->lib->senv->name, (long)it.e->key, it.e->val);
  }
}

void
pic_export(pic_state *pic, pic_sym sym)
{
  xh_entry *e;

  e = xh_get_int(pic->lib->senv->name, sym);
  if (! e) {
    pic_errorf(pic, "export: symbol not defined %s", pic_symbol_name(pic, sym));
  }
  xh_put_int(pic->lib->exports, (long)e->key, e->val);
}

void
pic_defmacro(pic_state *pic, const char *name, struct pic_proc *macro)
{
  struct pic_macro *mac;
  pic_sym sym, uniq;

  /* new macro */
  mac = macro_new(pic, macro, NULL);

  /* symbol registration */
  sym = pic_intern_cstr(pic, name);
  uniq = pic_gensym(pic, sym);
  xh_put_int(pic->lib->senv->name, sym, uniq);
  xh_put_int(pic->macros, uniq, (long)mac);

  /* auto export! */
  pic_export(pic, sym);
}

static pic_sym
symbol_rename(pic_state *pic, pic_sym sym, struct pic_senv *senv)
{
  xh_entry *e;

  if (! pic_interned_p(pic, sym)) {
    return sym;
  }
  while (true) {
    if ((e = xh_get_int(senv->name, sym)) != NULL) {
      return (pic_sym)e->val;
    }
    if (! senv->up)
      break;
    senv = senv->up;
  }
  return sym;
}

static pic_value
macroexpand(pic_state *pic, pic_value expr, struct pic_senv *senv)
{
  int ai = pic_gc_arena_preserve(pic);

#if DEBUG
  printf("[macroexpand] expanding... ");
  pic_debug(pic, expr);
  puts("");
#endif

  switch (pic_type(expr)) {
  case PIC_TT_SC: {
    struct pic_sc *sc;

    sc = pic_sc(expr);
    return macroexpand(pic, sc->expr, sc->senv);
  }
  case PIC_TT_SYMBOL: {
    return pic_symbol_value(symbol_rename(pic, pic_sym(expr), senv));
  }
  case PIC_TT_PAIR: {
    pic_value car, v;
    xh_entry *e;

    car = macroexpand(pic, pic_car(pic, expr), senv);
    if (pic_sym_p(car)) {
      pic_sym tag = pic_sym(car);

      if (tag == pic->sDEFINE_LIBRARY) {
        struct pic_lib *prev = pic->lib;

        if (pic_length(pic, expr) < 2) {
          pic_error(pic, "syntax error");
        }
        pic_make_library(pic, pic_cadr(pic, expr));

        pic_try {
          pic_in_library(pic, pic_cadr(pic, expr));

          pic_for_each (v, pic_cddr(pic, expr)) {
            int ai = pic_gc_arena_preserve(pic);

            pic_eval(pic, v);

            pic_gc_arena_restore(pic, ai);
          }

          pic_in_library(pic, prev->name);
        }
        pic_catch {
          /* restores pic->lib even if an error occurs */
          pic_in_library(pic, prev->name);
          pic_throw(pic, pic->err);
        }

        return pic_none_value();
      }

      else if (tag == pic->sIMPORT) {
        pic_value spec;
        pic_for_each (spec, pic_cdr(pic, expr)) {
          pic_import(pic, spec);
        }
        return pic_none_value();
      }

      else if (tag == pic->sEXPORT) {
        pic_value spec;
        pic_for_each (spec, pic_cdr(pic, expr)) {
          if (! pic_sym_p(spec)) {
            pic_error(pic, "syntax error");
          }
          /* TODO: warn if symbol is shadowed by local variable */
          pic_export(pic, pic_sym(spec));
        }
        return pic_none_value();
      }

      else if (tag == pic->sDEFINE_SYNTAX) {
	pic_value var, val;
        pic_sym uniq;
        struct pic_macro *mac;

	if (pic_length(pic, expr) != 3) {
	  pic_error(pic, "syntax error");
	}

	var = pic_cadr(pic, expr);
	if (! pic_sym_p(var)) {
	  var = macroexpand(pic, var, senv);
	}
        if (! pic_sym_p(var)) {
          pic_error(pic, "binding to non-symbol object");
        }
        uniq = pic_gensym(pic, pic_sym(var));
        xh_put_int(senv->name, pic_sym(var), uniq);

	val = pic_cadr(pic, pic_cdr(pic, expr));

        pic_try {
          v = pic_eval(pic, val);
        } pic_catch {
	  pic_errorf(pic, "macroexpand error: %s", pic_errmsg(pic));
        }

        if (! pic_proc_p(v)) {
          pic_errorf(pic, "macro definition \"~s\" evaluates to non-procedure object", var);
        }

        mac = macro_new(pic, pic_proc_ptr(v), senv);
        xh_put_int(pic->macros, uniq, (long)mac);

	pic_gc_arena_restore(pic, ai);
	return pic_none_value();
      }

      else if (tag == pic->sDEFINE_MACRO) {
	pic_value var, val;
        pic_sym uniq;
        struct pic_macro *mac;

	if (pic_length(pic, expr) < 2) {
	  pic_error(pic, "syntax error");
	}

	var = pic_car(pic, pic_cdr(pic, expr));
	if (pic_pair_p(var)) {
	  /* FIXME: unhygienic */
	  val = pic_cons(pic, pic_symbol_value(pic->sLAMBDA),
			 pic_cons(pic, pic_cdr(pic, var),
				  pic_cdr(pic, pic_cdr(pic, expr))));
	  var = pic_car(pic, var);
	}
	else {
	  if (pic_length(pic, expr) != 3) {
	    pic_error(pic, "syntax_error");
	  }
	  val = pic_car(pic, pic_cdr(pic, pic_cdr(pic, expr)));
	}
	if (! pic_sym_p(var)) {
	  pic_error(pic, "syntax error");
	}
        uniq = pic_gensym(pic, pic_sym(var));
        xh_put_int(senv->name, pic_sym(var), uniq);

        pic_try {
          v = pic_eval(pic, val);
        } pic_catch {
	  pic_errorf(pic, "macroexpand error: %s", pic_errmsg(pic));
        }

        if (! pic_proc_p(v)) {
          pic_errorf(pic, "macro definition \"~s\" evaluates to non-procedure object", var);
        }

        mac = macro_new(pic, pic_proc_ptr(v), NULL);
        xh_put_int(pic->macros, uniq, (long)mac);

	pic_gc_arena_restore(pic, ai);
	return pic_none_value();
      }

      else if (tag == pic->sLAMBDA) {
	struct pic_senv *in = senv_new_local(pic, pic_cadr(pic, expr), senv);

	v = pic_cons(pic, car,
		     pic_cons(pic,
			      macroexpand_list(pic, pic_cadr(pic, expr), in),
			      macroexpand_list(pic, pic_cddr(pic, expr), in)));

	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);
	return v;
      }

      else if (tag == pic->sDEFINE) {
	pic_sym var;
	pic_value formals;

	if (pic_length(pic, expr) < 2) {
	  pic_error(pic, "syntax error");
	}

	formals = pic_cadr(pic, expr);
	if (pic_pair_p(formals)) {
	  struct pic_senv *in = senv_new_local(pic, pic_cdr(pic, formals), senv);
	  pic_value a;

	  /* defined symbol */
	  a = pic_car(pic, formals);
          if (! pic_sym_p(a)) {
            a = macroexpand(pic, a, senv);
          }
	  if (! pic_sym_p(a)) {
	    pic_error(pic, "binding to non-symbol object");
	  }
	  var = pic_sym(a);
	  xh_put_int(senv->name, var, pic_gensym(pic, var));

	  /* binding value */
	  v = pic_cons(pic, car,
                       pic_cons(pic,
				macroexpand_list(pic, pic_cadr(pic, expr), in),
				macroexpand_list(pic, pic_cddr(pic, expr), in)));

	  pic_gc_arena_restore(pic, ai);
	  pic_gc_protect(pic, v);
	  return v;
	}

        if (! pic_sym_p(formals)) {
          formals = macroexpand(pic, formals, senv);
        }
	if (! pic_sym_p(formals)) {
	  pic_error(pic, "binding to non-symbol object");
	}
        var = pic_sym(formals);
        /* do not make duplicate variable slot */
        if (xh_get_int(senv->name, var) == NULL) {
          xh_put_int(senv->name, var, pic_gensym(pic, var));
        }

	v = pic_cons(pic, pic_symbol_value(tag),
                     macroexpand_list(pic, pic_cdr(pic, expr), senv));
	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);
	return v;
      }

      else if (tag == pic->sSETBANG || tag == pic->sIF || tag == pic->sBEGIN) {
	v = pic_cons(pic, car, macroexpand_list(pic, pic_cdr(pic, expr), senv));
	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);
	return v;
      }

      else if (tag == pic->sQUOTE) {
	v = pic_cons(pic, car, pic_cdr(pic, expr));
	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);
	return v;
      }

      /* macro */
      if ((e = xh_get_int(pic->macros, tag)) != NULL) {
        pic_value v, args;
        struct pic_macro *mac;

#if DEBUG
	puts("before expand-1:");
	pic_debug(pic, expr);
	puts("");
#endif

        mac = (struct pic_macro *)e->val;
	if (mac->senv == NULL) { /* legacy macro */
          args = pic_cdr(pic, expr);
        }
        else {
          args = pic_list3(pic, expr, pic_obj_value(senv), pic_obj_value(mac->senv));
        }

        pic_try {
          v = pic_apply(pic, mac->proc, args);
        } pic_catch {
          pic_errorf(pic, "macroexpand error: %s", pic_errmsg(pic));
        }
	pic_gc_arena_restore(pic, ai);
	pic_gc_protect(pic, v);

#if DEBUG
	puts("after expand-1:");
	pic_debug(pic, v);
	puts("");
#endif

	return macroexpand(pic, v, senv);
      }
    }

    v = pic_cons(pic, car, macroexpand_list(pic, pic_cdr(pic, expr), senv));
    pic_gc_arena_restore(pic, ai);
    pic_gc_protect(pic, v);
    return v;
  }
  case PIC_TT_EOF:
  case PIC_TT_NIL:
  case PIC_TT_BOOL:
  case PIC_TT_FLOAT:
  case PIC_TT_INT:
  case PIC_TT_CHAR:
  case PIC_TT_STRING:
  case PIC_TT_VECTOR:
  case PIC_TT_BLOB: {
    return expr;
  }
  case PIC_TT_PROC:
  case PIC_TT_PORT:
  case PIC_TT_ERROR:
  case PIC_TT_ENV:
  case PIC_TT_CONT:
  case PIC_TT_UNDEF:
  case PIC_TT_SENV:
  case PIC_TT_MACRO:
  case PIC_TT_LIB:
  case PIC_TT_VAR:
  case PIC_TT_IREP:
    pic_error(pic, "unexpected value type");
    return pic_undef_value();	/* unreachable */
  }
  UNREACHABLE();
}

static pic_value
macroexpand_list(pic_state *pic, pic_value list, struct pic_senv *senv)
{
  int ai = pic_gc_arena_preserve(pic);
  pic_value v, vs;

  /* macroexpand in order */
  vs = pic_nil_value();
  while (pic_pair_p(list)) {
    v = pic_car(pic, list);

    vs = pic_cons(pic, macroexpand(pic, v, senv), vs);
    list = pic_cdr(pic, list);

    pic_gc_arena_restore(pic, ai);
    pic_gc_protect(pic, vs);
    pic_gc_protect(pic, list);
  }

  list = macroexpand(pic, list, senv);

  /* reverse the result */
  pic_for_each (v, vs) {
    list = pic_cons(pic, v, list);

    pic_gc_arena_restore(pic, ai);
    pic_gc_protect(pic, vs);
    pic_gc_protect(pic, list);
  }

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, list);
  return list;
}

pic_value
pic_macroexpand(pic_state *pic, pic_value expr)
{
  pic_value v;

#if DEBUG
  puts("before expand:");
  pic_debug(pic, expr);
  puts("");
#endif

  v = macroexpand(pic, expr, pic->lib->senv);

#if DEBUG
  puts("after expand:");
  pic_debug(pic, v);
  puts("");
#endif

  return v;
}

struct pic_senv *
pic_null_syntactic_env(pic_state *pic)
{
  return senv_new(pic, NULL);
}

#define register_core_syntax(pic,senv,id) do {                          \
    pic_sym sym = pic_intern_cstr(pic, id);                             \
    xh_put_int(senv->name, sym, sym);                                   \
  } while (0)

struct pic_senv *
pic_minimal_syntactic_env(pic_state *pic)
{
  struct pic_senv *senv = pic_null_syntactic_env(pic);

  register_core_syntax(pic, senv, "define-library");
  register_core_syntax(pic, senv, "import");
  register_core_syntax(pic, senv, "export");

  return senv;
}

struct pic_senv *
pic_core_syntactic_env(pic_state *pic)
{
  struct pic_senv *senv = pic_minimal_syntactic_env(pic);

  register_core_syntax(pic, senv, "define");
  register_core_syntax(pic, senv, "set!");
  register_core_syntax(pic, senv, "quote");
  register_core_syntax(pic, senv, "lambda");
  register_core_syntax(pic, senv, "if");
  register_core_syntax(pic, senv, "begin");
  register_core_syntax(pic, senv, "define-syntax");

  return senv;
}

/* once read.c is implemented move there */
static pic_value
pic_macro_include(pic_state *pic)
{
  size_t argc, i;
  pic_value *argv, exprs, body;
  FILE *file;

  pic_get_args(pic, "*", &argc, &argv);

  /* FIXME unhygienic */
  body = pic_list1(pic, pic_symbol_value(pic->sBEGIN));

  for (i = 0; i < argc; ++i) {
    const char *filename;
    if (! pic_str_p(argv[i])) {
      pic_error(pic, "expected string");
    }
    filename = pic_str_cstr(pic_str_ptr(argv[i]));
    file = fopen(filename, "r");
    if (file == NULL) {
      pic_error(pic, "could not open file");
    }
    exprs = pic_parse_file(pic, file);
    if (pic_undef_p(exprs)) {
      pic_error(pic, "parse error");
    }
    body = pic_append(pic, body, exprs);
  }

  return body;
}

static pic_value
pic_macro_gensym(pic_state *pic)
{
  static const char skel[] = ".g";
  pic_sym uniq;

  pic_get_args(pic, "");

  uniq = pic_gensym(pic, pic_intern_cstr(pic, skel));
  return pic_symbol_value(uniq);
}

static pic_value
pic_macro_macroexpand(pic_state *pic)
{
  pic_value expr;

  pic_get_args(pic, "o", &expr);

  return pic_macroexpand(pic, expr);
}

static pic_value
pic_macro_make_sc(pic_state *pic)
{
  pic_value senv, free_vars, expr;
  struct pic_sc *sc;

  pic_get_args(pic, "ooo", &senv, &free_vars, &expr);

  if (! pic_senv_p(senv))
    pic_error(pic, "make-syntactic-closure: senv required");

  /* just ignore free_vars for now */
  sc = sc_new(pic, expr, pic_senv(senv));

  return pic_obj_value(sc);
}

static pic_value
pic_macro_identifier_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(identifier_p(obj));
}

static pic_value
pic_macro_identifier_eq_p(pic_state *pic)
{
  pic_value e, x, f, y;
  struct pic_senv *e1, *e2;

  pic_get_args(pic, "oooo", &e, &x, &f, &y);

  if (! pic_senv_p(e)) {
    pic_error(pic, "unexpected type of argument 1");
  }
  e1 = pic_senv(e);
  if (! pic_senv_p(f)) {
    pic_error(pic, "unexpected type of argument 3");
  }
  e2 = pic_senv(f);

  return pic_bool_value(identifier_eq_p(pic, e1, x, e2, y));
}

static pic_value
er_macro_rename(pic_state *pic)
{
  pic_sym sym;
  struct pic_senv *mac_env;

  pic_get_args(pic, "m", &sym);

  mac_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 1));

  return pic_symbol_value(symbol_rename(pic, sym, mac_env));
}

static pic_value
er_macro_compare(pic_state *pic)
{
  pic_value a, b;
  struct pic_senv *use_env;
  pic_sym m, n;

  pic_get_args(pic, "oo", &a, &b);

  if (! pic_sym_p(a) || ! pic_sym_p(b))
    return pic_false_value();   /* should be an error? */

  use_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  m = symbol_rename(pic, pic_sym(a), use_env);
  n = symbol_rename(pic, pic_sym(b), use_env);

  return pic_bool_value(m == n);
}

static pic_value
er_macro_call(pic_state *pic)
{
  pic_value expr, use_env, mac_env;
  struct pic_proc *rename, *compare, *cb;

  pic_get_args(pic, "ooo", &expr, &use_env, &mac_env);

  if (! pic_senv_p(use_env)) {
    pic_error(pic, "unexpected type of argument 1");
  }
  if (! pic_senv_p(mac_env)) {
    pic_error(pic, "unexpected type of argument 3");
  }

  rename = pic_proc_new(pic, er_macro_rename);
  pic_proc_cv_init(pic, rename, 2);
  pic_proc_cv_set(pic, rename, 0, use_env);
  pic_proc_cv_set(pic, rename, 1, mac_env);

  compare = pic_proc_new(pic, er_macro_compare);
  pic_proc_cv_init(pic, compare, 2);
  pic_proc_cv_set(pic, compare, 0, use_env);
  pic_proc_cv_set(pic, compare, 1, mac_env);

  cb = pic_proc_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  return pic_apply_argv(pic, cb, 3, expr, pic_obj_value(rename), pic_obj_value(compare));
}

static pic_value
pic_macro_er_macro_transformer(pic_state *pic)
{
  struct pic_proc *cb, *proc;

  pic_get_args(pic, "l", &cb);

  proc = pic_proc_new(pic, er_macro_call);
  pic_proc_cv_init(pic, proc, 1);
  pic_proc_cv_set(pic, proc, 0, pic_obj_value(cb));

  return pic_obj_value(proc);
}

static pic_value
ir_macro_inject(pic_state *pic)
{
  pic_sym sym;
  struct pic_senv *use_env;

  pic_get_args(pic, "m", &sym);

  use_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  return pic_symbol_value(symbol_rename(pic, sym, use_env));
}

static pic_value
ir_macro_compare(pic_state *pic)
{
  pic_value a, b;
  struct pic_senv *mac_env;
  pic_sym m, n;

  pic_get_args(pic, "oo", &a, &b);

  if (! pic_sym_p(a) || ! pic_sym_p(b))
    return pic_false_value();   /* should be an error? */

  mac_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 1));

  m = symbol_rename(pic, pic_sym(a), mac_env);
  n = symbol_rename(pic, pic_sym(b), mac_env);

  return pic_bool_value(m == n);
}

static pic_value
ir_macro_wrap(pic_state *pic, pic_value expr, struct pic_senv *use_env, pic_value *assoc)
{
  if (pic_sym_p(expr)) {
    pic_value ren;
    ren = macroexpand(pic, expr, use_env);
    *assoc = pic_acons(pic, ren, expr, *assoc);
    return ren;
  }
  else if (pic_pair_p(expr)) {
    return pic_cons(pic,
                    ir_macro_wrap(pic, pic_car(pic, expr), use_env, assoc),
                    ir_macro_wrap(pic, pic_cdr(pic, expr), use_env, assoc));
  }
  else {
    return expr;
  }
}

static pic_value
ir_macro_unwrap(pic_state *pic, pic_value expr, struct pic_senv *mac_env, pic_value *assoc)
{
  if (pic_sym_p(expr) || pic_macro_p(expr)) {
    pic_value r;
    if (pic_test(r = pic_assq(pic, expr, *assoc))) {
      return pic_cdr(pic, r);
    }
    r = macroexpand(pic, expr, mac_env);
    if (pic_macro_p(r)) {
      return expr;
    }
    else {
      return r;
    }
  }
  else if (pic_pair_p(expr)) {
    return pic_cons(pic,
                    ir_macro_unwrap(pic, pic_car(pic, expr), mac_env, assoc),
                    ir_macro_unwrap(pic, pic_cdr(pic, expr), mac_env, assoc));
  }
  else {
    return expr;
  }
}

static pic_value
ir_macro_call(pic_state *pic)
{
  pic_value expr, use_env, mac_env;
  struct pic_proc *inject, *compare, *cb;
  pic_value assoc = pic_nil_value();

  pic_get_args(pic, "ooo", &expr, &use_env, &mac_env);

  if (! pic_senv_p(use_env)) {
    pic_error(pic, "unexpected type of argument 1");
  }
  if (! pic_senv_p(mac_env)) {
    pic_error(pic, "unexpected type of argument 3");
  }

  inject = pic_proc_new(pic, ir_macro_inject);
  pic_proc_cv_init(pic, inject, 2);
  pic_proc_cv_set(pic, inject, 0, use_env);
  pic_proc_cv_set(pic, inject, 1, mac_env);

  compare = pic_proc_new(pic, ir_macro_compare);
  pic_proc_cv_init(pic, compare, 2);
  pic_proc_cv_set(pic, compare, 0, use_env);
  pic_proc_cv_set(pic, compare, 1, mac_env);

  cb = pic_proc_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  expr = ir_macro_wrap(pic, expr, pic_senv_ptr(use_env), &assoc);
  expr = pic_apply_argv(pic, cb, 3, expr, pic_obj_value(inject), pic_obj_value(compare));
  expr = ir_macro_unwrap(pic, expr, pic_senv_ptr(mac_env), &assoc);
  return expr;
}

static pic_value
pic_macro_ir_macro_transformer(pic_state *pic)
{
  struct pic_proc *cb, *proc;

  pic_get_args(pic, "l", &cb);

  proc = pic_proc_new(pic, ir_macro_call);
  pic_proc_cv_init(pic, proc, 1);
  pic_proc_cv_set(pic, proc, 0, pic_obj_value(cb));

  return pic_obj_value(proc);
}

void
pic_init_macro(pic_state *pic)
{
  pic_defmacro(pic, "include", pic_proc_new(pic, pic_macro_include));

  pic_deflibrary ("(picrin macro)") {

    /* export define-macro syntax */
    xh_put_int(pic->lib->senv->name, pic->sDEFINE_MACRO, pic->sDEFINE_MACRO);
    pic_export(pic, pic->sDEFINE_MACRO);

    pic_defun(pic, "gensym", pic_macro_gensym);
    pic_defun(pic, "macroexpand", pic_macro_macroexpand);
    pic_defun(pic, "make-syntactic-closure", pic_macro_make_sc);
    pic_defun(pic, "identifier?", pic_macro_identifier_p);
    pic_defun(pic, "identifier=?", pic_macro_identifier_eq_p);
    pic_defun(pic, "er-macro-transformer", pic_macro_er_macro_transformer);
    pic_defun(pic, "ir-macro-transformer", pic_macro_ir_macro_transformer);
  }
}
