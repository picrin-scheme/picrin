/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

KHASH_DEFINE(weak, void *, pic_value, kh_ptr_hash_func, kh_ptr_hash_equal)

struct pic_weak *
pic_make_weak(pic_state *pic)
{
  struct pic_weak *weak;

  weak = (struct pic_weak *)pic_obj_alloc(pic, sizeof(struct pic_weak), PIC_TT_WEAK);
  weak->prev = NULL;
  kh_init(weak, &weak->hash);

  return weak;
}

pic_value
pic_weak_ref(pic_state *pic, struct pic_weak *weak, void *key)
{
  khash_t(weak) *h = &weak->hash;
  khiter_t it;

  it = kh_get(weak, h, key);
  if (it == kh_end(h)) {
    pic_errorf(pic, "element not found for a key: ~s", pic_obj_value(key));
  }
  return kh_val(h, it);
}

void *
pic_weak_rev_ref(pic_state *pic, struct pic_weak *weak, pic_value val)
{
  khash_t(weak) *h = &weak->hash;

  if (h->n_buckets) {
    khint_t i = 0;
    while ((i < h->n_buckets) && (ac_iseither(h->flags, i) || !pic_eq_p(h->vals[i], val))) {
      i += 1;
    }
    if (i < h->n_buckets) return kh_key(h, i);
  }
  pic_errorf(pic, "key not found for an element: ~s", val);
  return NULL;
}

void
pic_weak_set(pic_state PIC_UNUSED(*pic), struct pic_weak *weak, void *key, pic_value val)
{
  khash_t(weak) *h = &weak->hash;
  int ret;
  khiter_t it;

  it = kh_put(weak, h, key, &ret);
  kh_val(h, it) = val;
}

bool
pic_weak_has(pic_state PIC_UNUSED(*pic), struct pic_weak *weak, void *key)
{
  return kh_get(weak, &weak->hash, key) != kh_end(&weak->hash);
}

void
pic_weak_del(pic_state *pic, struct pic_weak *weak, void *key)
{
  khash_t(weak) *h = &weak->hash;
  khiter_t it;

  it = kh_get(weak, h, key);
  if (it == kh_end(h)) {
    pic_errorf(pic, "no slot named ~s found in register", pic_obj_value(key));
  }
  kh_del(weak, h, it);
}


static pic_value
weak_get(pic_state *pic, struct pic_weak *weak, void *key)
{
  if (! pic_weak_has(pic, weak, key)) {
    return pic_false_value();
  }
  return pic_cons(pic, pic_obj_value(key), pic_weak_ref(pic, weak, key));
}

static pic_value
weak_set(pic_state *pic, struct pic_weak *weak, void *key, pic_value val)
{
  if (pic_undef_p(val)) {
    if (pic_weak_has(pic, weak, key)) {
      pic_weak_del(pic, weak, key);
    }
  } else {
    pic_weak_set(pic, weak, key, val);
  }

  return pic_undef_value();
}

static pic_value
weak_call(pic_state *pic)
{
  struct pic_proc *self;
  struct pic_weak *weak;
  pic_value key, val;
  int n;

  n = pic_get_args(pic, "&o|o", &self, &key, &val);

  if (! pic_obj_p(key)) {
    pic_errorf(pic, "attempted to set a non-object key '~s' in a register", key);
  }

  weak = pic_weak_ptr(pic_proc_env_ref(pic, self, "weak"));

  if (n == 1) {
    return weak_get(pic, weak, pic_obj_ptr(key));
  } else {
    return weak_set(pic, weak, pic_obj_ptr(key), val);
  }
}

static pic_value
pic_weak_make_register(pic_state *pic)
{
  struct pic_weak *weak;
  struct pic_proc *proc;

  pic_get_args(pic, "");

  weak = pic_make_weak(pic);

  proc = pic_make_proc(pic, weak_call);

  pic_proc_env_set(pic, proc, "weak", pic_obj_value(weak));

  return pic_obj_value(proc);
}

void
pic_init_weak(pic_state *pic)
{
  pic_defun(pic, "make-register", pic_weak_make_register);
}
