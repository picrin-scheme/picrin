/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "picrin.h"
#include "picrin/string.h"

pic_sym
pic_intern(pic_state *pic, const char *str, size_t len)
{
  char *cstr;
  xh_entry *e;
  pic_sym id;

  cstr = (char *)pic_malloc(pic, len + 1);
  cstr[len] = '\0';
  memcpy(cstr, str, len);

  e = xh_get(&pic->syms, cstr);
  if (e) {
    return xh_val(e, pic_sym);
  }

  id = pic->sym_cnt++;
  xh_put(&pic->syms, cstr, &id);
  xh_put(&pic->sym_names, id, &cstr);
  return id;
}

pic_sym
pic_intern_cstr(pic_state *pic, const char *str)
{
  return pic_intern(pic, str, strlen(str));
}

const char *
pic_symbol_name(pic_state *pic, pic_sym sym)
{
  return xh_val(xh_get(&pic->sym_names, sym), const char *);
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

  return pic_symbol_value(pic_intern_cstr(pic, pic_str_cstr(pic_str_ptr(v))));
}

void
pic_init_symbol(pic_state *pic)
{
  pic_defun(pic, "symbol?", pic_symbol_symbol_p);
  pic_defun(pic, "symbol->string", pic_symbol_symbol_to_string);
  pic_defun(pic, "string->symbol", pic_symbol_string_to_symbol);
}
