/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

KHASH_DEFINE(s, const char *, pic_sym *, kh_str_hash_func, kh_str_hash_equal)

pic_sym *
pic_intern(pic_state *pic, pic_str *str)
{
  return pic_intern_cstr(pic, pic_str_cstr(pic, str));
}

pic_sym *
pic_intern_cstr(pic_state *pic, const char *cstr)
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

const char *
pic_symbol_name(pic_state PIC_UNUSED(*pic), pic_sym *sym)
{
  return sym->cstr;
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

  return pic_obj_value(pic_make_str_cstr(pic, sym->cstr));
}

static pic_value
pic_symbol_string_to_symbol(pic_state *pic)
{
  pic_str *str;

  pic_get_args(pic, "s", &str);

  return pic_obj_value(pic_intern(pic, str));
}

void
pic_init_symbol(pic_state *pic)
{
  pic_defun(pic, "symbol?", pic_symbol_symbol_p);

  pic_defun(pic, "symbol->string", pic_symbol_symbol_to_string);
  pic_defun(pic, "string->symbol", pic_symbol_string_to_symbol);

  pic_defun(pic, "symbol=?", pic_symbol_symbol_eq_p);
}
