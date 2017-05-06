/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "value.h"
#include "object.h"

KHASH_DEFINE(attr, struct object *, pic_value, kh_ptr_hash_func, kh_ptr_hash_equal)

static pic_value
attr_call(pic_state *pic)
{
  pic_value self, key, val;
  int n;

  n = pic_get_args(pic, "&o|o", &self, &key, &val);

  if (! pic_obj_p(pic, key)) {
    pic_error(pic, "attempted to set a non-object key", 1, key);
  }

  if (n == 1) {
    if (! pic_attr_has(pic, self, key)) {
      return pic_false_value(pic);
    }
    return pic_attr_ref(pic, self, key);
  } else {
    if (pic_false_p(pic, val)) {
      if (pic_attr_has(pic, self, key)) {
        pic_attr_del(pic, self, key);
      }
    } else {
      pic_attr_set(pic, self, key, val);
    }
    return pic_undef_value(pic);
  }
}

pic_value
pic_make_attr(pic_state *pic)
{
  struct attr *attr;

  attr = (struct attr *)pic_obj_alloc(pic, PIC_TYPE_ATTR);
  attr->prev = NULL;
  kh_init(attr, &attr->hash);
  return pic_lambda(pic, attr_call, 1, obj_value(pic, attr));
}

pic_value
pic_attr_ref(pic_state *pic, pic_value attr, pic_value key)
{
  khash_t(attr) *h = &attr_ptr(pic, proc_ptr(pic, attr)->env->regs[0])->hash;
  int it;

  it = kh_get(attr, h, pic_ptr(pic, key));
  if (it == kh_end(h)) {
    pic_error(pic, "element not found for given key", 1, key);
  }
  return kh_val(h, it);
}

void
pic_attr_set(pic_state *pic, pic_value attr, pic_value key, pic_value val)
{
  khash_t(attr) *h = &attr_ptr(pic, proc_ptr(pic, attr)->env->regs[0])->hash;
  int ret;
  int it;

  it = kh_put(attr, h, pic_ptr(pic, key), &ret);
  kh_val(h, it) = val;
}

bool
pic_attr_has(pic_state *pic, pic_value attr, pic_value key)
{
  khash_t(attr) *h = &attr_ptr(pic, proc_ptr(pic, attr)->env->regs[0])->hash;

  return kh_get(attr, h, pic_ptr(pic, key)) != kh_end(h);
}

void
pic_attr_del(pic_state *pic, pic_value attr, pic_value key)
{
  khash_t(attr) *h = &attr_ptr(pic, proc_ptr(pic, attr)->env->regs[0])->hash;
  int it;

  it = kh_get(attr, h, pic_ptr(pic, key));
  if (it == kh_end(h)) {
    pic_error(pic, "element not found for given key", 1, key);
  }
  kh_del(attr, h, it);
}

static pic_value
pic_attr_make_attribute(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_make_attr(pic);
}

void
pic_init_attr(pic_state *pic)
{
  pic_defun(pic, "make-attribute", pic_attr_make_attribute);
}
