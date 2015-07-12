#include "picrin.h"

struct pic_dict *
pic_attr(pic_state *pic, pic_value obj)
{
  struct pic_dict *dict;

  if (! pic_obj_p(obj)) {
    pic_errorf(pic, "attribute: expected heap object, but got immediate value ~s", obj);
  }

  if (! pic_reg_has(pic, pic->attrs, pic_ptr(obj))) {
    dict = pic_make_dict(pic);

    pic_reg_set(pic, pic->attrs, pic_ptr(obj), pic_obj_value(dict));

    return dict;
  }
  return pic_dict_ptr(pic_reg_ref(pic, pic->attrs, pic_ptr(obj)));
}

pic_value
pic_attr_ref(pic_state *pic, pic_value obj, const char *key)
{
  return pic_dict_ref(pic, pic_attr(pic, obj), pic_intern(pic, key));
}

void
pic_attr_set(pic_state *pic, pic_value obj, const char *key, pic_value v)
{
  pic_dict_set(pic, pic_attr(pic, obj), pic_intern(pic, key), v);
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
