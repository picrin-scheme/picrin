#include <string.h>
#include <stdlib.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/symbol.h"

static int
str_hash(const char *str)
{
  int hash = 0, len, i;

  len = strlen(str);
  for (i = 0; i < len; ++i) {
    hash = hash * 31 + str[i];
  }
  return hash;
}

pic_value
sym_tbl_get(struct sym_tbl *s_tbl, const char *key)
{
  int hash, idx;
  pic_value v, k;
  char *name;

  hash = str_hash(key);
  idx = hash % s_tbl->size;
  for (v = s_tbl->tbl[idx]; ! pic_nil_p(v); v = pic_pair_ptr(v)->cdr) {
    k = pic_pair_ptr(v)->car;

    name = pic_symbol_ptr(k)->name;
    if (strcmp(name, key) == 0) {
      return k;
    }
  }
  return pic_undef_value();
}

pic_value
pic_intern_cstr(pic_state *pic, const char *str)
{
  pic_value v;
  int len, hash, idx;
  char *new_str;
  struct pic_symbol *sym;

  v = sym_tbl_get(pic->sym_tbl, str);
  if (! pic_undef_p(v)) {
    return v;
  }

  /* clone name string */
  len = strlen(str);
  new_str = (char *)pic_alloc(pic, len + 1);
  strncpy(new_str, str, len + 1);

  sym = (struct pic_symbol*)pic_obj_alloc(pic, sizeof(struct pic_symbol), PIC_TT_SYMBOL);
  sym->name = new_str;
  v = pic_obj_value(sym);

  hash = str_hash(str);
  idx = hash % pic->sym_tbl->size;
  pic->sym_tbl->tbl[idx] = pic_cons(pic, v, pic->sym_tbl->tbl[idx]);
  return v;
}
