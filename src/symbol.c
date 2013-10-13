#include <string.h>
#include <stdlib.h>

#include "picrin.h"

pic_value
pic_intern_cstr(pic_state *pic, const char *name)
{
  struct pic_symbol *sym;
  size_t len;

  sym = (struct pic_symbol*)pic_obj_alloc(pic, sizeof(struct pic_symbol), PIC_TT_SYMBOL);

  /* clone name string */
  len = strlen(name);
  sym->name = (char *)malloc(len + 1);
  strncpy(sym->name, name, len + 1);

  return pic_obj_value(sym);
}
