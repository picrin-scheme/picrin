/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"

KHASH_DEFINE(dict, symbol *, pic_value, kh_ptr_hash_func, kh_ptr_hash_equal)

pic_value
pic_make_dict(pic_state *pic)
{
  struct dict *dict;

  dict = (struct dict *)pic_obj_alloc(pic, sizeof(struct dict), PIC_TYPE_DICT);
  kh_init(dict, &dict->hash);
  return obj_value(pic, dict);
}

pic_value
pic_dict_ref(pic_state *pic, pic_value dict, pic_value key)
{
  khash_t(dict) *h = &pic_dict_ptr(pic, dict)->hash;
  int it;

  it = kh_get(dict, h, pic_sym_ptr(pic, key));
  if (it == kh_end(h)) {
    pic_error(pic, "element not found for given key", 1, key);
  }
  return kh_val(h, it);
}

void
pic_dict_set(pic_state *pic, pic_value dict, pic_value key, pic_value val)
{
  khash_t(dict) *h = &pic_dict_ptr(pic, dict)->hash;
  int ret;
  int it;

  it = kh_put(dict, h, pic_sym_ptr(pic, key), &ret);
  kh_val(h, it) = val;
}

int
pic_dict_size(pic_state *PIC_UNUSED(pic), pic_value dict)
{
  return kh_size(&pic_dict_ptr(pic, dict)->hash);
}

bool
pic_dict_has(pic_state *pic, pic_value dict, pic_value key)
{
  khash_t(dict) *h = &pic_dict_ptr(pic, dict)->hash;

  return kh_get(dict, h, pic_sym_ptr(pic, key)) != kh_end(h);
}

void
pic_dict_del(pic_state *pic, pic_value dict, pic_value key)
{
  khash_t(dict) *h = &pic_dict_ptr(pic, dict)->hash;
  int it;

  it = kh_get(dict, h, pic_sym_ptr(pic, key));
  if (it == kh_end(h)) {
    pic_error(pic, "element not found for given key", 1, key);
  }
  kh_del(dict, h, it);
}

bool
pic_dict_next(pic_state *PIC_UNUSED(pic), pic_value dict, int *iter, pic_value *key, pic_value *val)
{
  khash_t(dict) *h = &pic_dict_ptr(pic, dict)->hash;
  int it = *iter;

  for (it = *iter; it != kh_end(h); ++it) {
    if (kh_exist(h, it)) {
      if (key) *key = obj_value(pic, kh_key(h, it));
      if (val) *val = kh_val(h, it);
      *iter = ++it;
      return true;
    }
  }
  return false;
}

static pic_value
pic_dict_make_dictionary(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_make_dict(pic);
}

static pic_value
pic_dict_dictionary(pic_state *pic)
{
  pic_value dict, *argv;
  int argc, i;

  pic_get_args(pic, "*", &argc, &argv);

  dict = pic_make_dict(pic);

  for (i = 0; i < argc; i += 2) {
    TYPE_CHECK(pic, argv[i], sym);
    pic_dict_set(pic, dict, argv[i], argv[i+1]);
  }

  return dict;
}

static pic_value
pic_dict_dictionary_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic, pic_dict_p(pic, obj));
}

static pic_value
pic_dict_dictionary_has_p(pic_state *pic)
{
  pic_value dict, key;

  pic_get_args(pic, "dm", &dict, &key);

  return pic_bool_value(pic, pic_dict_has(pic, dict, key));
}

static pic_value
pic_dict_dictionary_ref(pic_state *pic)
{
  pic_value dict, key;

  pic_get_args(pic, "dm", &dict, &key);

  return pic_dict_ref(pic, dict, key);
}

static pic_value
pic_dict_dictionary_set(pic_state *pic)
{
  pic_value dict, key, val;

  pic_get_args(pic, "dmo", &dict, &key, &val);

  pic_dict_set(pic, dict, key, val);
  return pic_undef_value(pic);
}

static pic_value
pic_dict_dictionary_delete(pic_state *pic)
{
  pic_value dict, key;

  pic_get_args(pic, "dm", &dict, &key);

  pic_dict_del(pic, dict, key);
  return pic_undef_value(pic);
}

static pic_value
pic_dict_dictionary_size(pic_state *pic)
{
  pic_value dict;

  pic_get_args(pic, "d", &dict);

  return pic_int_value(pic, pic_dict_size(pic, dict));
}

static pic_value
pic_dict_dictionary_map(pic_state *pic)
{
  pic_value dict, proc, key, ret = pic_nil_value(pic);
  int it = 0;
  size_t ai;

  pic_get_args(pic, "ld", &proc, &dict);

  ai = pic_enter(pic);
  while (pic_dict_next(pic, dict, &it, &key, NULL)) {
    pic_push(pic, pic_call(pic, proc, 1, key), ret);
    pic_leave(pic, ai);
    pic_protect(pic, ret);
  }
  return pic_reverse(pic, ret);
}

static pic_value
pic_dict_dictionary_for_each(pic_state *pic)
{
  pic_value dict, proc, key;
  int it = 0;
  size_t ai;

  pic_get_args(pic, "ld", &proc, &dict);

  ai = pic_enter(pic);
  while (pic_dict_next(pic, dict, &it, &key, NULL)) {
    pic_call(pic, proc, 1, key);
    pic_leave(pic, ai);
  }

  return pic_undef_value(pic);
}

static pic_value
pic_dict_dictionary_to_alist(pic_state *pic)
{
  pic_value dict, key, val, alist = pic_nil_value(pic);
  int it = 0;

  pic_get_args(pic, "d", &dict);

  while (pic_dict_next(pic, dict, &it, &key, &val)) {
    pic_push(pic, pic_cons(pic, key, val), alist);
  }

  return alist;
}

static pic_value
pic_dict_alist_to_dictionary(pic_state *pic)
{
  pic_value dict, alist, e, it;

  pic_get_args(pic, "o", &alist);

  dict = pic_make_dict(pic);

  pic_for_each (e, pic_reverse(pic, alist), it) {
    TYPE_CHECK(pic, pic_car(pic, e), sym);
    pic_dict_set(pic, dict, pic_car(pic, e), pic_cdr(pic, e));
  }

  return dict;
}

static pic_value
pic_dict_dictionary_to_plist(pic_state *pic)
{
  pic_value dict, key, val, plist = pic_nil_value(pic);
  int it = 0;

  pic_get_args(pic, "d", &dict);

  while (pic_dict_next(pic, dict, &it, &key, &val)) {
    pic_push(pic, val, plist);
    pic_push(pic, key, plist);
  }

  return plist;
}

static pic_value
pic_dict_plist_to_dictionary(pic_state *pic)
{
  pic_value dict, plist, e;

  pic_get_args(pic, "o", &plist);

  dict = pic_make_dict(pic);

  for (e = pic_reverse(pic, plist); ! pic_nil_p(pic, e); e = pic_cddr(pic, e)) {
    TYPE_CHECK(pic, pic_cadr(pic, e), sym);
    pic_dict_set(pic, dict, pic_cadr(pic, e), pic_car(pic, e));
  }

  return dict;
}

void
pic_init_dict(pic_state *pic)
{
  pic_defun(pic, "make-dictionary", pic_dict_make_dictionary);
  pic_defun(pic, "dictionary?", pic_dict_dictionary_p);
  pic_defun(pic, "dictionary", pic_dict_dictionary);
  pic_defun(pic, "dictionary-has?", pic_dict_dictionary_has_p);
  pic_defun(pic, "dictionary-ref", pic_dict_dictionary_ref);
  pic_defun(pic, "dictionary-set!", pic_dict_dictionary_set);
  pic_defun(pic, "dictionary-delete!", pic_dict_dictionary_delete);
  pic_defun(pic, "dictionary-size", pic_dict_dictionary_size);
  pic_defun(pic, "dictionary-map", pic_dict_dictionary_map);
  pic_defun(pic, "dictionary-for-each", pic_dict_dictionary_for_each);
  pic_defun(pic, "dictionary->alist", pic_dict_dictionary_to_alist);
  pic_defun(pic, "alist->dictionary", pic_dict_alist_to_dictionary);
  pic_defun(pic, "dictionary->plist", pic_dict_dictionary_to_plist);
  pic_defun(pic, "plist->dictionary", pic_dict_plist_to_dictionary);
}
