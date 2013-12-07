#include "picrin.h"
#include "picrin/lib.h"
#include "picrin/pair.h"
#include "picrin/macro.h"
#include "xhash/xhash.h"

static pic_value
lib_spec(pic_state *pic, const char *name)
{
  pic_value vs;
  int r;

  r = pic_parse_cstr(pic, name, &vs);
  if (r != 1) {
    pic_error(pic, "invalid library spec given");
  }
  return pic_car(pic, vs);
}

void
pic_make_library(pic_state *pic, const char *name)
{
  struct pic_lib *lib;
  pic_value spec = lib_spec(pic, name);

  lib = (struct pic_lib *)pic_obj_alloc(pic, sizeof(struct pic_lib), PIC_TT_LIB);
  lib->senv = pic_core_syntactic_env(pic);
  lib->exports = xh_new();

  pic->lib_tbl = pic_acons(pic, spec, pic_obj_value(lib), pic->lib_tbl);
}

void
pic_in_library(pic_state *pic, const char *name)
{
  pic_value v, spec = lib_spec(pic, name);

  v = pic_assoc(pic, spec, pic->lib_tbl);
  if (pic_false_p(v)) {
    pic_error(pic, "library not found");
  }

  pic->lib = pic_lib_ptr(pic_cdr(pic, v));
}
