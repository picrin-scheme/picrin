#include "picrin.h"
#include "picrin/dict.h"

struct pic_dict *
pic_attr(pic_state *pic, pic_value obj)
{
  xh_entry *e;

  if (pic_vtype(obj) != PIC_VTYPE_HEAP) {
    pic_errorf(pic, "attribute: expected heap object, but got immediate value ~s", obj);
  }

  e = xh_get_ptr(&pic->attrs, pic_ptr(obj));
  if (e == NULL) {
    struct pic_dict *dict = pic_make_dict(pic);

    e = xh_put_ptr(&pic->attrs, pic_ptr(obj), &dict);

    assert(dict == xh_val(e, struct pic_dict *));
  }
  return xh_val(e, struct pic_dict *);
}

pic_value
pic_attr_ref(pic_state *pic, pic_value obj, const char *key)
{
  return pic_dict_ref(pic, pic_attr(pic, obj), pic_intern_cstr(pic, key));
}

void
pic_attr_set(pic_state *pic, pic_value obj, const char *key, pic_value v)
{
  pic_dict_set(pic, pic_attr(pic, obj), pic_intern_cstr(pic, key), v);
}

static pic_value
pic_attr_attribute(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_obj_value(pic_attr(pic, obj));
}

void
pic_init_attr(pic_state *pic)
{
  pic_defun(pic, "attribute", pic_attr_attribute);
}
