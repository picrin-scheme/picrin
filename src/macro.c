/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/macro.h"
#include "picrin/lib.h"

static pic_value macroexpand(pic_state *, pic_value, struct pic_senv *);
static pic_value macroexpand_list(pic_state *, pic_value, struct pic_senv *);

static struct pic_senv *
new_senv(pic_state *pic, struct pic_senv *up)
{
  struct pic_senv *senv;

  senv = (struct pic_senv *)pic_obj_alloc(pic, sizeof(struct pic_senv), PIC_TT_SENV);
  senv->up = up;
  senv->name = xh_new_int();

  return senv;
}

static struct pic_senv *
new_local_senv(pic_state *pic, pic_value formals, struct pic_senv *up)
{
  struct pic_senv *senv;
  pic_value a;
  pic_sym sym;

  senv = new_senv(pic, up);

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
    if (it.e->val >= 0) {
      printf("* importing %s as %s\n",
             pic_symbol_name(pic, (long)it.e->key),
             pic_symbol_name(pic, it.e->val));
    }
    else {
      printf("* importing %s\n", pic_symbol_name(pic, (long)it.e->key));
    }
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
    pic_error(pic, "symbol not defined");
  }
  xh_put_int(pic->lib->exports, (long)e->key, e->val);
}

static void
defsyntax(pic_state *pic, pic_sym sym, struct pic_proc *macro, struct pic_senv *mac_env)
{
  struct pic_macro *mac;
  pic_sym uniq;

  mac = macro_new(pic, macro, mac_env);

  uniq = pic_gensym(pic, sym);
  xh_put_int(pic->lib->senv->name, sym, uniq);
  xh_put_int(pic->macros, uniq, (long)mac);
}

static void
defmacro(pic_state *pic, const char *name, struct pic_proc *macro)
{
  defsyntax(pic, pic_intern_cstr(pic, name), macro, NULL);
}

void
pic_defmacro(pic_state *pic, const char *name, struct pic_proc *macro)
{
  defmacro(pic, name, macro);

  /* auto export! */
  pic_export(pic, pic_intern_cstr(pic, name));
}

static pic_sym
symbol_rename(pic_state *pic, pic_sym sym, struct pic_senv *senv)
{
  xh_entry *e;
  pic_sym uniq;

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
  uniq = pic_gensym(pic, sym);
  xh_put_int(senv->name, sym, uniq);
  return uniq;
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

        /* proceed expressions in new library */
        pic_in_library(pic, pic_cadr(pic, expr));
        {
          int ai = pic_gc_arena_preserve(pic);
          struct pic_proc *proc;

          pic_for_each (v, pic_cddr(pic, expr)) {
            proc = pic_compile(pic, v);
            if (proc == NULL) {
              abort();
            }
            pic_apply_argv(pic, proc, 0);
            if (pic_undef_p(v)) {
              abort();
            }
            pic_gc_arena_restore(pic, ai);
          }
        }
        pic_in_library(pic, prev->name);

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
	struct pic_proc *proc;

	if (pic_length(pic, expr) != 3) {
	  pic_error(pic, "syntax error");
	}

	var = pic_cadr(pic, expr);
	if (! pic_sym_p(var)) {
	  pic_error(pic, "syntax error");
	}

	val = pic_cadr(pic, pic_cdr(pic, expr));
	proc = pic_compile(pic, val);
	if (pic->err) {
	  printf("macroexpand error: %s\n", pic_errmsg(pic));
	  abort();
	}
	v = pic_apply(pic, proc, pic_nil_value());
	if (pic->err) {
	  printf("macroexpand error: %s\n", pic_errmsg(pic));
	  abort();
	}
	assert(pic_proc_p(v));
	defsyntax(pic, pic_sym(var), pic_proc_ptr(v), senv);

	pic_gc_arena_restore(pic, ai);
	return pic_none_value();
      }

      else if (tag == pic->sDEFINE_MACRO) {
	pic_value var, val;
	struct pic_proc *proc;

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

	proc = pic_compile(pic, val);
	if (pic->err) {
	  printf("macroexpand error: %s\n", pic_errmsg(pic));
	  abort();
	}
	v = pic_apply(pic, proc, pic_nil_value());
	if (pic->err) {
	  printf("macroexpand error: %s\n", pic_errmsg(pic));
	  abort();
	}
	assert(pic_proc_p(v));
	defmacro(pic, pic_symbol_name(pic, pic_sym(var)), pic_proc_ptr(v));

	pic_gc_arena_restore(pic, ai);
	return pic_none_value();
      }

      else if (tag == pic->sLAMBDA) {
	struct pic_senv *in = new_local_senv(pic, pic_cadr(pic, expr), senv);

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
	  struct pic_senv *in = new_local_senv(pic, pic_cdr(pic, formals), senv);
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
        pic_value v;
        struct pic_macro *mac;

        mac = (struct pic_macro *)e->val;
	if (mac->senv == NULL) { /* legacy macro */
	  v = pic_apply(pic, mac->proc, pic_cdr(pic, expr));
	  if (pic->err) {
	    printf("macroexpand error: %s\n", pic_errmsg(pic));
	    abort();
	  }
	}
	else {
	  v = pic_apply_argv(pic, mac->proc, 3, expr, pic_obj_value(senv), pic_obj_value(mac->senv));
	  if (pic->err) {
	    printf("macroexpand error: %s\n", pic_errmsg(pic));
	    abort();
	  }
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
  /* suppress warnings, never be called */
  abort();
}

static pic_value
macroexpand_list(pic_state *pic, pic_value list, struct pic_senv *senv)
{
  pic_value v;

  if (! pic_pair_p(list))
    return macroexpand(pic, list, senv);

  v = macroexpand(pic, pic_car(pic, list), senv);
  return pic_cons(pic, v, macroexpand_list(pic, pic_cdr(pic, list), senv));
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
  return new_senv(pic, NULL);
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
  register_core_syntax(pic, senv, "define-macro");
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
  int res;

  pic_get_args(pic, "*", &argc, &argv);

  /* FIXME unhygienic */
  body = pic_list(pic, 1, pic_symbol_value(pic->sBEGIN));

  for (i = 0; i < argc; ++i) {
    char *filename;
    if (! pic_str_p(argv[i])) {
      pic_error(pic, "expected string");
    }
    filename = pic_str_ptr(argv[i])->str;
    file = fopen(filename, "r");
    if (file == NULL) {
      pic_error(pic, "could not open file");
    }
    res = pic_parse_file(pic, file, &exprs);
    if (res < 0) {
      pic_error(pic, "parse error");
    }
    body = pic_append(pic, body, exprs);
  }

  return body;
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
  pic_value v;

  pic_get_args(pic, "m", &sym);

  mac_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 1));

  v = macroexpand(pic, pic_symbol_value(sym), mac_env);
  if (pic_macro_p(v)) {
    return pic_symbol_value(sym);
  }
  else {
    return v;
  }
}

static pic_value
er_macro_compare(pic_state *pic)
{
  pic_value a, b;
  struct pic_senv *use_env;

  pic_get_args(pic, "oo", &a, &b);

  if (! pic_sym_p(a) || ! pic_sym_p(b))
    return pic_false_value();   /* should be an error? */

  use_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  a = macroexpand(pic, a, use_env);
  b = macroexpand(pic, b, use_env);

  return pic_bool_value(pic_eq_p(a, b));
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
  pic_value v;

  pic_get_args(pic, "m", &sym);

  use_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  v = macroexpand(pic, pic_symbol_value(sym), use_env);
  if (pic_macro_p(v)) {
    return pic_symbol_value(sym);
  }
  else {
    return v;
  }
}

static pic_value
ir_macro_compare(pic_state *pic)
{
  pic_value a, b;
  struct pic_senv *use_env;

  pic_get_args(pic, "oo", &a, &b);

  if (! pic_sym_p(a) || ! pic_sym_p(b))
    return pic_false_value();   /* should be an error? */

  use_env = pic_senv_ptr(pic_proc_cv_ref(pic, pic_get_proc(pic), 0));

  a = macroexpand(pic, a, use_env);
  b = macroexpand(pic, b, use_env);

  return pic_bool_value(pic_eq_p(a, b));
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
    pic_defun(pic, "make-syntactic-closure", pic_macro_make_sc);
    pic_defun(pic, "identifier?", pic_macro_identifier_p);
    pic_defun(pic, "identifier=?", pic_macro_identifier_eq_p);
    pic_defun(pic, "er-macro-transformer", pic_macro_er_macro_transformer);
    pic_defun(pic, "ir-macro-transformer", pic_macro_ir_macro_transformer);
  }
}
