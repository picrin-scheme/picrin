/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"

#include <stdio.h>

static pic_value
pic_load_load(pic_state *pic)
{
  pic_value envid, port;
  char *fn;
  FILE *fp;

  pic_get_args(pic, "z|o", &fn, &envid);

  fp = fopen(fn, "r");
  if (fp == NULL) {
    pic_error(pic, "load: could not open file", 1, pic_cstr_value(pic, fn));
  }

  port = pic_fopen(pic, fp, "r");

  pic_load(pic, port);

  pic_fclose(pic, port);

  return pic_undef_value(pic);
}

void
pic_init_load(pic_state *pic)
{
  pic_deflibrary(pic, "scheme.load");

  pic_defun(pic, "load", pic_load_load);
}
