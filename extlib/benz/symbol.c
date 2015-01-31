/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/symbol.h"
#include "picrin/string.h"

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

  e = xh_get_str(&pic->syms, pic_str_cstr(pic, str));
  if (e) {
    sym = xh_val(e, pic_sym *);
    pic_gc_protect(pic, pic_obj_value(sym));
    return sym;
  }

  cstr = pic_malloc(pic, pic_str_len(str) + 1);
  strcpy(cstr, pic_str_cstr(pic, str));

  sym = pic_make_symbol(pic, str);
  xh_put_str(&pic->syms, cstr, &sym);
  return sym;
}

pic_sym *
pic_intern_cstr(pic_state *pic, const char *str)
{
  return pic_intern(pic, pic_make_str(pic, str, strlen(str)));
}

pic_sym *
pic_gensym(pic_state *pic, pic_sym *base)
{
  return pic_make_symbol(pic, base->str);
}

bool
pic_interned_p(pic_state *pic, pic_sym *sym)
{
  xh_entry *e;

  e = xh_get_str(&pic->syms, pic_str_cstr(pic, sym->str));
  if (e) {
    return sym == xh_val(e, pic_sym *);
  } else {
    return false;
  }
}

const char *
pic_symbol_name(pic_state *pic, pic_sym *sym)
{
  return pic_str_cstr(pic, sym->str);
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

void
pic_init_symbol(pic_state *pic)
{
  pic_defun(pic, "symbol?", pic_symbol_symbol_p);
  pic_defun(pic, "symbol->string", pic_symbol_symbol_to_string);
  pic_defun(pic, "string->symbol", pic_symbol_string_to_symbol);

  pic_defun(pic, "symbol=?", pic_symbol_symbol_eq_p);
}
