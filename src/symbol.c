/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "picrin.h"

pic_sym
pic_intern_cstr(pic_state *pic, const char *str)
{
  xh_entry *e;
  pic_sym id;

  e = xh_get(pic->syms, str);
  if (e) {
    return e->val;
  }

  str = pic_strdup(pic, str);

  id = pic->sym_cnt++;
  xh_put(pic->syms, str, id);
  xh_put_int(pic->sym_names, id, (long)str);
  return id;
}

pic_sym
pic_gensym(pic_state *pic, pic_sym base)
{
  int id;
  char *str;
  pic_sym uniq;

  id = ++pic->uniq_sym_cnt;
  str = (char *)pic_alloc(pic, strlen(pic_symbol_name(pic, base)) + (int)log10(id) + 3);
  sprintf(str, "%s@%d", pic_symbol_name(pic, base), id);

  /* don't put the symbol to pic->syms to keep it uninterned */
  uniq = pic->sym_cnt++;
  xh_put_int(pic->sym_names, uniq, (long)str);

  return uniq;
}

bool
pic_interned_p(pic_state *pic, pic_sym sym)
{
  return sym == pic_intern_cstr(pic, pic_symbol_name(pic, sym));
}

const char *
pic_symbol_name(pic_state *pic, pic_sym sym)
{
  return (const char *)xh_get_int(pic->sym_names, sym)->val;
}

static pic_value
pic_symbol_symbol_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_sym_p(v));
}

static pic_value
pic_symbol_symbol_to_string(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (! pic_sym_p(v)) {
    pic_error(pic, "symbol->string: expected symbol");
  }

  return pic_obj_value(pic_str_new_cstr(pic, pic_symbol_name(pic, pic_sym(v))));
}

static pic_value
pic_symbol_string_to_symbol(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (! pic_str_p(v)) {
    pic_error(pic, "string->symbol: expected string");
  }

  return pic_symbol_value(pic_intern_cstr(pic, pic_str_ptr(v)->str));
}

void
pic_init_symbol(pic_state *pic)
{
  pic_defun(pic, "symbol?", pic_symbol_symbol_p);
  pic_defun(pic, "symbol->string", pic_symbol_symbol_to_string);
  pic_defun(pic, "string->symbol", pic_symbol_string_to_symbol);
}
