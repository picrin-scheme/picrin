/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"

KHASH_DEFINE(weak, struct object *, pic_value, kh_ptr_hash_func, kh_ptr_hash_equal)

static pic_value
weak_call(pic_state *pic)
{
  pic_value self, key, val;
  int n;

  n = pic_get_args(pic, "&o|o", &self, &key, &val);

  if (! obj_p(pic, key)) {
    pic_error(pic, "attempted to set a non-object key", 1, key);
  }

  if (n == 1) {
    if (! pic_weak_has(pic, self, key)) {
      return pic_false_value(pic);
    }
    return pic_weak_ref(pic, self, key);
  } else {
    if (pic_false_p(pic, val)) {
      if (pic_weak_has(pic, self, key)) {
        pic_weak_del(pic, self, key);
      }
    } else {
      pic_weak_set(pic, self, key, val);
    }
    return pic_undef_value(pic);
  }
}

pic_value
pic_make_weak(pic_state *pic)
{
  struct weak *weak;

  weak = (struct weak *)pic_obj_alloc(pic, PIC_TYPE_WEAK);
  weak->prev = NULL;
  kh_init(weak, &weak->hash);
  return pic_lambda(pic, weak_call, 1, obj_value(pic, weak));
}

pic_value
pic_weak_ref(pic_state *pic, pic_value weak, pic_value key)
{
  khash_t(weak) *h = &weak_ptr(pic, proc_ptr(pic, weak)->fp->regs[0])->hash;
  int it;

  it = kh_get(weak, h, obj_ptr(pic, key));
  if (it == kh_end(h)) {
    pic_error(pic, "element not found for given key", 1, key);
  }
  return kh_val(h, it);
}

void
pic_weak_set(pic_state *pic, pic_value weak, pic_value key, pic_value val)
{
  khash_t(weak) *h = &weak_ptr(pic, proc_ptr(pic, weak)->fp->regs[0])->hash;
  int ret;
  int it;

  it = kh_put(weak, h, obj_ptr(pic, key), &ret);
  kh_val(h, it) = val;
}

bool
pic_weak_has(pic_state *pic, pic_value weak, pic_value key)
{
  khash_t(weak) *h = &weak_ptr(pic, proc_ptr(pic, weak)->fp->regs[0])->hash;

  return kh_get(weak, h, obj_ptr(pic, key)) != kh_end(h);
}

void
pic_weak_del(pic_state *pic, pic_value weak, pic_value key)
{
  khash_t(weak) *h = &weak_ptr(pic, proc_ptr(pic, weak)->fp->regs[0])->hash;
  int it;

  it = kh_get(weak, h, obj_ptr(pic, key));
  if (it == kh_end(h)) {
    pic_error(pic, "element not found for given key", 1, key);
  }
  kh_del(weak, h, it);
}

static pic_value
pic_weak_make_ephemeron_table(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_make_weak(pic);
}

void
pic_init_weak(pic_state *pic)
{
  pic_defun(pic, "make-ephemeron-table", pic_weak_make_ephemeron_table);
}
