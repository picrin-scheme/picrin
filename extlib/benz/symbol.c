/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/symbol.h"
#include "picrin/string.h"
#include "picrin/macro.h"

pic_sym *
pic_make_symbol(pic_state *pic, pic_str *str)
{
  pic_sym *sym;

  sym = (pic_sym *)pic_obj_alloc(pic, sizeof(struct pic_symbol), PIC_TT_SYMBOL);
  sym->str = str;
  return sym;
}

pic_sym *
pic_intern(pic_state *pic, pic_str *str)
{
  xh_entry *e;
  pic_sym *sym;
  char *cstr;

  e = xh_get_str(&pic->syms, pic_str_cstr(str));
  if (e) {
    sym = xh_val(e, pic_sym *);
    pic_gc_protect(pic, pic_obj_value(sym));
    return sym;
  }

  cstr = pic_malloc(pic, pic_strlen(str) + 1);
  strcpy(cstr, pic_str_cstr(str));

  sym = pic_make_symbol(pic, str);
  xh_put_str(&pic->syms, cstr, &sym);
  return sym;
}

pic_sym *
pic_intern_cstr(pic_state *pic, const char *str)
{
  return pic_intern(pic, pic_make_str(pic, str, strlen(str)));
}

const char *
pic_symbol_name(pic_state *pic, pic_sym *sym)
{
  PIC_UNUSED(pic);

  return pic_str_cstr(sym->str);
}

static pic_value
pic_symbol_symbol_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_sym_p(v));
}

static pic_value
pic_symbol_symbol_eq_p(pic_state *pic)
{
  size_t argc, i;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    if (! pic_sym_p(argv[i])) {
      return pic_false_value();
    }
    if (! pic_eq_p(argv[i], argv[0])) {
      return pic_false_value();
    }
  }
  return pic_true_value();
}

static pic_value
pic_symbol_symbol_to_string(pic_state *pic)
{
  pic_sym *sym;

  pic_get_args(pic, "m", &sym);

  return pic_obj_value(sym->str);
}

static pic_value
pic_symbol_string_to_symbol(pic_state *pic)
{
  pic_str *str;

  pic_get_args(pic, "s", &str);

  return pic_obj_value(pic_intern(pic, str));
}

/**
 * Identifiers (uninterned symbols)
 */

pic_sym *
pic_gensym(pic_state *pic, pic_sym *base)
{
  return pic_make_symbol(pic, base->str);
}

static bool
pic_interned_p(pic_state *pic, pic_sym *sym)
{
  xh_entry *e;

  e = xh_get_str(&pic->syms, pic_str_cstr(sym->str));
  if (e) {
    return sym == xh_val(e, pic_sym *);
  } else {
    return false;
  }
}

pic_sym *
pic_make_identifier(pic_state *pic, pic_sym *sym, struct pic_senv *senv)
{
  pic_sym *rename;

  while (true) {
    if ((rename = pic_find_rename(pic, senv, sym)) != NULL) {
      return rename;
    }
    if (! senv->up)
      break;
    senv = senv->up;
  }
  if (! pic_interned_p(pic, sym)) {
    return sym;
  }
  else {
    return pic_gensym(pic, sym);
  }
}

bool
pic_identifier_p(pic_state *pic, pic_value obj)
{
  return pic_sym_p(obj) && ! pic_interned_p(pic, pic_sym_ptr(obj));
}

bool
pic_identifier_eq_p(pic_state *pic, struct pic_senv *env1, pic_sym *sym1, struct pic_senv *env2, pic_sym *sym2)
{
  pic_sym *a, *b;

  a = pic_make_identifier(pic, sym1, env1);
  if (a != pic_make_identifier(pic, sym1, env1)) {
    a = sym1;
  }

  b = pic_make_identifier(pic, sym2, env2);
  if (b != pic_make_identifier(pic, sym2, env2)) {
    b = sym2;
  }

  return pic_eq_p(pic_obj_value(a), pic_obj_value(b));
}

static pic_value
pic_symbol_identifier_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic_identifier_p(pic, obj));
}

static pic_value
pic_symbol_make_identifier(pic_state *pic)
{
  pic_value obj;
  pic_sym *sym;

  pic_get_args(pic, "mo", &sym, &obj);

  pic_assert_type(pic, obj, senv);

  return pic_obj_value(pic_make_identifier(pic, sym, pic_senv_ptr(obj)));
}

static pic_value
pic_symbol_identifier_eq_p(pic_state *pic)
{
  pic_sym *sym1, *sym2;
  pic_value env1, env2;

  pic_get_args(pic, "omom", &env1, &sym1, &env2, &sym2);

  pic_assert_type(pic, env1, senv);
  pic_assert_type(pic, env2, senv);

  return pic_bool_value(pic_identifier_eq_p(pic, pic_senv_ptr(env1), sym1, pic_senv_ptr(env2), sym2));
}

void
pic_init_symbol(pic_state *pic)
{
  pic_defun(pic, "symbol?", pic_symbol_symbol_p);
  pic_defun(pic, "symbol->string", pic_symbol_symbol_to_string);
  pic_defun(pic, "string->symbol", pic_symbol_string_to_symbol);
  pic_defun(pic, "symbol=?", pic_symbol_symbol_eq_p);

  pic_defun(pic, "identifier?", pic_symbol_identifier_p);
  pic_defun(pic, "identifier=?", pic_symbol_identifier_eq_p);
  pic_defun(pic, "make-identifier", pic_symbol_make_identifier);
}
