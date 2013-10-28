#include <string.h>
#include <stdlib.h>

#include "picrin.h"
#include "xhash/xhash.h"

pic_sym
pic_intern_cstr(pic_state *pic, const char *str)
{
  struct xh_entry *e;
  pic_sym id;

  e = xh_get(pic->sym_tbl, str);
  if (e) {
    return e->val;
  }

  if (pic->slen >= pic->scapa) {
    pic->scapa *= 2;
    pic->sym_pool = pic_realloc(pic, pic->sym_pool, sizeof(const char *) * pic->scapa);
  }
  id = pic->slen++;
  pic->sym_pool[id] = strdup(str);
  xh_put(pic->sym_tbl, str, id);
  return id;
}

const char *
pic_symbol_name(pic_state *pic, pic_sym sym)
{
  return pic->sym_pool[sym];
}
