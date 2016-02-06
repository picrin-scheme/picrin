/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

KHASH_DEFINE(s, const char *, pic_sym *, kh_str_hash_func, kh_str_hash_equal)

pic_sym *
pic_intern_str(pic_state *pic, pic_str *str)
{
  return pic_intern(pic, pic_str_cstr(pic, str));
}

pic_sym *
pic_intern(pic_state *pic, const char *cstr)
{
  khash_t(s) *h = &pic->syms;
  pic_sym *sym;
  khiter_t it;
  int ret;
  char *copy;

  it = kh_put(s, h, cstr, &ret);
  if (ret == 0) {               /* if exists */
    sym = kh_val(h, it);
    pic_gc_protect(pic, pic_obj_value(sym));
    return sym;
  }

  copy = pic_malloc(pic, strlen(cstr) + 1);
  strcpy(copy, cstr);
  kh_key(h, it) = copy;

  kh_val(h, it) = pic->sQUOTE; /* insert dummy */

  sym = (pic_sym *)pic_obj_alloc(pic, sizeof(pic_sym), PIC_TT_SYMBOL);
  sym->cstr = copy;
  kh_val(h, it) = sym;

  return sym;
}

pic_id *
pic_make_identifier(pic_state *pic, pic_id *id, struct pic_env *env)
{
  pic_id *nid;

  nid = (pic_id *)pic_obj_alloc(pic, sizeof(pic_id), PIC_TT_ID);
  nid->u.id.id = id;
  nid->u.id.env = env;
  return nid;
}

const char *
pic_symbol_name(pic_state PIC_UNUSED(*pic), pic_sym *sym)
{
  return sym->cstr;
}

const char *
pic_identifier_name(pic_state *pic, pic_id *id)
{
  while (! pic_sym_p(pic_obj_value(id))) {
    id = id->u.id.id;
  }

  return pic_symbol_name(pic, (pic_sym *)id);
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
  int argc, i;
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

  return pic_obj_value(pic_make_str_cstr(pic, sym->cstr));
}

static pic_value
pic_symbol_string_to_symbol(pic_state *pic)
{
  pic_str *str;

  pic_get_args(pic, "s", &str);

  return pic_obj_value(pic_intern_str(pic, str));
}

static pic_value
pic_symbol_identifier_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic_id_p(obj));
}

static pic_value
pic_symbol_make_identifier(pic_state *pic)
{
  pic_value id, env;

  pic_get_args(pic, "oo", &id, &env);

  pic_assert_type(pic, id, id);
  pic_assert_type(pic, env, env);

  return pic_obj_value(pic_make_identifier(pic, pic_id_ptr(id), pic_env_ptr(env)));
}

static pic_value
pic_symbol_identifier_variable(pic_state *pic)
{
  pic_value id;

  pic_get_args(pic, "o", &id);

  pic_assert_type(pic, id, id);

  if (pic_sym_p(id)) {
    pic_errorf(pic, "expected non-symbol identifier, but got symbol ~s", id);
  }

  return pic_obj_value(pic_id_ptr(id)->u.id.id);
}

static pic_value
pic_symbol_identifier_environment(pic_state *pic)
{
  pic_value id;

  pic_get_args(pic, "o", &id);

  pic_assert_type(pic, id, id);

  if (pic_sym_p(id)) {
    pic_errorf(pic, "expected non-symbol identifier, but got symbol ~s", id);
  }

  return pic_obj_value(pic_id_ptr(id)->u.id.env);
}

static pic_value
pic_symbol_identifier_eq_p(pic_state *pic)
{
  int argc, i;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    if (! pic_id_p(argv[i])) {
      return pic_false_value();
    }
    if (! pic_equal_p(pic, argv[i], argv[0])) {
      return pic_false_value();
    }
  }
  return pic_true_value();
}

void
pic_init_symbol(pic_state *pic)
{
  pic_defun(pic, "symbol?", pic_symbol_symbol_p);
  pic_defun(pic, "symbol=?", pic_symbol_symbol_eq_p);
  pic_defun(pic, "symbol->string", pic_symbol_symbol_to_string);
  pic_defun(pic, "string->symbol", pic_symbol_string_to_symbol);

  pic_defun(pic, "make-identifier", pic_symbol_make_identifier);
  pic_defun(pic, "identifier?", pic_symbol_identifier_p);
  pic_defun(pic, "identifier=?", pic_symbol_identifier_eq_p);
  pic_defun(pic, "identifier-variable", pic_symbol_identifier_variable);
  pic_defun(pic, "identifier-environment", pic_symbol_identifier_environment);
}
