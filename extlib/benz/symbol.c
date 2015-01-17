/**
 * See Copyright Notice in picrin.h
 */

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

  e = xh_get_str(&pic->syms, cstr);
  if (e) {
    return xh_val(e, pic_sym);
  }

  id = pic->sym_cnt++;
  xh_put_str(&pic->syms, cstr, &id);
  xh_put_int(&pic->sym_names, id, &cstr);
  return id;
}

pic_sym
pic_intern_cstr(pic_state *pic, const char *str)
{
  return pic_intern(pic, str, strlen(str));
}

pic_sym
pic_intern_str(pic_state *pic, pic_str *str)
{
  return pic_intern_cstr(pic, pic_str_cstr(str));
}

pic_sym
pic_gensym(pic_state *pic, pic_sym base)
{
  int uid = pic->uniq_sym_cnt++, len;
  char *str, mark;
  pic_sym uniq;

  if (pic_interned_p(pic, base)) {
    mark = '@';
  } else {
    mark = '.';
  }

  len = snprintf(NULL, 0, "%s%c%d", pic_symbol_name(pic, base), mark, uid);
  str = pic_alloc(pic, (size_t)len + 1);
  sprintf(str, "%s%c%d", pic_symbol_name(pic, base), mark, uid);

  /* don't put the symbol to pic->syms to keep it uninterned */
  uniq = pic->sym_cnt++;
  xh_put_int(&pic->sym_names, uniq, &str);

  return uniq;
}

pic_sym
pic_ungensym(pic_state *pic, pic_sym base)
{
  const char *name, *occr;

  if (pic_interned_p(pic, base)) {
    return base;
  }

  name = pic_symbol_name(pic, base);
  if ((occr = strrchr(name, '@')) == NULL) {
    pic_panic(pic, "logic flaw");
  }
  return pic_intern(pic, name, (size_t)(occr - name));
}

bool
pic_interned_p(pic_state *pic, pic_sym sym)
{
  return sym == pic_intern_cstr(pic, pic_symbol_name(pic, sym));
}

const char *
pic_symbol_name(pic_state *pic, pic_sym sym)
{
  return xh_val(xh_get_int(&pic->sym_names, sym), const char *);
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
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (! pic_sym_p(v)) {
    pic_errorf(pic, "symbol->string: expected symbol");
  }

  return pic_obj_value(pic_make_str_cstr(pic, pic_symbol_name(pic, pic_sym(v))));
}

static pic_value
pic_symbol_string_to_symbol(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  if (! pic_str_p(v)) {
    pic_errorf(pic, "string->symbol: expected string");
  }

  return pic_symbol_value(pic_intern_cstr(pic, pic_str_cstr(pic_str_ptr(v))));
}

void
pic_init_symbol(pic_state *pic)
{
  pic_defun(pic, "symbol?", pic_symbol_symbol_p);
  pic_defun(pic, "symbol->string", pic_symbol_symbol_to_string);
  pic_defun(pic, "string->symbol", pic_symbol_string_to_symbol);

  pic_defun(pic, "symbol=?", pic_symbol_symbol_eq_p);
}
