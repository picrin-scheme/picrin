/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "picrin.h"
#include "xhash/xhash.h"

pic_sym
pic_intern_cstr(pic_state *pic, const char *str)
{
  xh_entry *e;
  pic_sym id;

  e = xh_get(pic->sym_tbl, str);
  if (e) {
    return e->val;
  }

  str = pic_strdup(pic, str);

  if (pic->slen >= pic->scapa) {

#if DEBUG
    puts("sym_pool realloced");
#endif

    pic->scapa *= 2;
    pic->sym_pool = pic_realloc(pic, pic->sym_pool, sizeof(const char *) * pic->scapa);
  }
  id = pic->slen++;
  pic->sym_pool[id] = str;
  xh_put(pic->sym_tbl, str, id);
  return id;
}

pic_sym
pic_gensym(pic_state *pic, pic_sym base)
{
  int s = ++pic->uniq_sym_count;
  char *str;
  pic_sym uniq;

  str = (char *)pic_alloc(pic, strlen(pic_symbol_name(pic, base)) + (int)log10(s) + 3);
  sprintf(str, "%s@%d", pic_symbol_name(pic, base), s);

  /* don't put the symbol to pic->sym_tbl to keep it uninterned */
  if (pic->slen >= pic->scapa) {
    pic->scapa *= 2;
    pic->sym_pool = pic_realloc(pic, pic->sym_pool, sizeof(const char *) * pic->scapa);
  }
  uniq = pic->slen++;
  pic->sym_pool[uniq] = str;

  return uniq;
}

bool
pic_interned_p(pic_state *pic, pic_sym sym)
{
  assert(sym >= 0);

  return sym == pic_intern_cstr(pic, pic_symbol_name(pic, sym));
}

const char *
pic_symbol_name(pic_state *pic, pic_sym sym)
{
  return pic->sym_pool[sym];
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
