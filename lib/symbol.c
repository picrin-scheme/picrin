/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"
#include "state.h"

#define kh_pic_str_hash(a) (pic_str_hash(pic, obj_value(pic, a)))
#define kh_pic_str_cmp(a, b) (pic_str_cmp(pic, obj_value(pic, a), obj_value(pic, b)) == 0)

KHASH_DEFINE(oblist, struct string *, symbol *, kh_pic_str_hash, kh_pic_str_cmp)

pic_value
pic_intern(pic_state *pic, pic_value str)
{
  khash_t(oblist) *h = &pic->oblist;
  symbol *sym;
  int it;
  int ret;

  it = kh_put(oblist, h, pic_str_ptr(pic, str), &ret);
  if (ret == 0) {               /* if exists */
    sym = kh_val(h, it);
    pic_protect(pic, obj_value(pic, sym));
    return obj_value(pic, sym);
  }

  kh_val(h, it) = NULL;         /* dummy */

  sym = (symbol *)pic_obj_alloc(pic, offsetof(symbol, env), PIC_TYPE_SYMBOL);
  sym->u.str = pic_str_ptr(pic, str);
  kh_val(h, it) = sym;

  return obj_value(pic, sym);
}

pic_value
pic_make_identifier(pic_state *pic, pic_value base, pic_value env)
{
  struct identifier *id;

  id = (struct identifier *)pic_obj_alloc(pic, sizeof(struct identifier), PIC_TYPE_ID);
  id->u.id = pic_id_ptr(pic, base);
  id->env = pic_env_ptr(pic, env);

  return obj_value(pic, id);
}

pic_value
pic_sym_name(pic_state *PIC_UNUSED(pic), pic_value sym)
{
  return obj_value(pic, pic_sym_ptr(pic, sym)->u.str);
}

pic_value
pic_id_name(pic_state *pic, pic_value id)
{
  while (! pic_sym_p(pic, id)) {
    id = obj_value(pic, pic_id_ptr(pic, id)->u.id);
  }

  return pic_sym_name(pic, id);
}

static pic_value
pic_symbol_symbol_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_sym_p(pic, v));
}

static pic_value
pic_symbol_symbol_eq_p(pic_state *pic)
{
  int argc, i;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    if (! pic_sym_p(pic, argv[i])) {
      return pic_false_value(pic);
    }
    if (! pic_eq_p(pic, argv[i], argv[0])) {
      return pic_false_value(pic);
    }
  }
  return pic_true_value(pic);
}

static pic_value
pic_symbol_symbol_to_string(pic_state *pic)
{
  pic_value sym;

  pic_get_args(pic, "m", &sym);

  return pic_sym_name(pic, sym);
}

static pic_value
pic_symbol_string_to_symbol(pic_state *pic)
{
  pic_value str;

  pic_get_args(pic, "s", &str);

  return pic_intern(pic, str);
}

static pic_value
pic_symbol_identifier_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic, pic_id_p(pic, obj));
}

static pic_value
pic_symbol_make_identifier(pic_state *pic)
{
  pic_value id, env;

  pic_get_args(pic, "oo", &id, &env);

  TYPE_CHECK(pic, id, id);
  TYPE_CHECK(pic, env, env);

  return pic_make_identifier(pic, id, env);
}

static pic_value
pic_symbol_identifier_base(pic_state *pic)
{
  pic_value id;

  pic_get_args(pic, "o", &id);

  TYPE_CHECK(pic, id, id);

  if (pic_sym_p(pic, id)) {
    pic_error(pic, "non-symbol identifier required", 1, id);
  }

  return obj_value(pic, pic_id_ptr(pic, id)->u.id);
}

static pic_value
pic_symbol_identifier_environment(pic_state *pic)
{
  pic_value id;

  pic_get_args(pic, "o", &id);

  TYPE_CHECK(pic, id, id);

  if (pic_sym_p(pic, id)) {
    pic_error(pic, "non-symbol identifier required", 1, id);
  }

  return obj_value(pic, pic_id_ptr(pic, id)->env);
}

static pic_value
pic_symbol_identifier_eq_p(pic_state *pic)
{
  int argc, i;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  for (i = 0; i < argc; ++i) {
    if (! pic_id_p(pic, argv[i])) {
      return pic_false_value(pic);
    }
    if (! pic_equal_p(pic, argv[i], argv[0])) {
      return pic_false_value(pic);
    }
  }
  return pic_true_value(pic);
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
  pic_defun(pic, "identifier-base", pic_symbol_identifier_base);
  pic_defun(pic, "identifier-environment", pic_symbol_identifier_environment);
}
