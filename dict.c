/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/dict.h"
#include "picrin/cont.h"
#include "picrin/pair.h"

static int
xh_value_hash(const void *key, void *data)
{
  union { double f; int i; } u;
  pic_value val = *(pic_value *)key;
  int hash;

  UNUSED(data);

  switch (pic_vtype(val)) {
  default:
    hash = 0;
    break;
  case PIC_VTYPE_SYMBOL:
    hash = pic_sym(val);
    break;
  case PIC_VTYPE_FLOAT:
    u.f = pic_float(val);
    hash = u.i;
    break;
  case PIC_VTYPE_INT:
    hash = pic_int(val);
    break;
  case PIC_VTYPE_HEAP:
    hash = (int)pic_ptr(val);
    break;
  }

  return hash + pic_vtype(val);
}

static int
xh_value_equal(const void *key1, const void *key2, void *pic)
{
  return pic_equal_p(pic, *(pic_value *)key1, *(pic_value *)key2);
}

static void
xh_init_value(pic_state *pic, xhash *x)
{
  xh_init_(x, sizeof(pic_value), sizeof(pic_value), xh_value_hash, xh_value_equal, pic);
}

static inline xh_entry *
xh_get_value(xhash *x, pic_value key)
{
  return xh_get_(x, &key);
}

static inline xh_entry *
xh_put_value(xhash *x, pic_value key, void *val)
{
  return xh_put_(x, &key, val);
}

static inline void
xh_del_value(xhash *x, pic_value key)
{
  xh_del_(x, &key);
}

struct pic_dict *
pic_make_dict(pic_state *pic)
{
  struct pic_dict *dict;

  dict = (struct pic_dict *)pic_obj_alloc(pic, sizeof(struct pic_dict), PIC_TT_DICT);
  xh_init_value(pic, &dict->hash);

  return dict;
}

pic_value
pic_dict_ref(pic_state *pic, struct pic_dict *dict, pic_value key)
{
  xh_entry *e;

  e = xh_get_value(&dict->hash, key);
  if (! e) {
    pic_errorf(pic, "element not found for a key: ~s", key);
  }
  return xh_val(e, pic_value);
}

void
pic_dict_set(pic_state *pic, struct pic_dict *dict, pic_value key, pic_value val)
{
  UNUSED(pic);

  xh_put_value(&dict->hash, key, &val);
}

size_t
pic_dict_size(pic_state *pic, struct pic_dict *dict)
{
  UNUSED(pic);

  return dict->hash.count;
}

bool
pic_dict_has(pic_state *pic, struct pic_dict *dict, pic_value key)
{
  UNUSED(pic);

  return xh_get_value(&dict->hash, key) != NULL;
}

void
pic_dict_del(pic_state *pic, struct pic_dict *dict, pic_value key)
{
  if (xh_get_value(&dict->hash, key) == NULL) {
    pic_errorf(pic, "no slot named ~s found in dictionary", key);
  }

  xh_del_value(&dict->hash, key);
}

static pic_value
pic_dict_make_dictionary(pic_state *pic)
{
  struct pic_dict *dict;

  pic_get_args(pic, "");

  dict = pic_make_dict(pic);

  return pic_obj_value(dict);
}

static pic_value
pic_dict_dictionary(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value *argv;
  size_t argc, i;

  pic_get_args(pic, "*", &argc, &argv);

  dict = pic_make_dict(pic);

  for (i = 0; i < argc; i += 2) {
    pic_dict_set(pic, dict, argv[i], argv[i+1]);
  }

  return pic_obj_value(dict);
}

static pic_value
pic_dict_dictionary_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic_dict_p(obj));
}

static pic_value
pic_dict_dictionary_ref(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value key;

  pic_get_args(pic, "do", &dict, &key);

  if (pic_dict_has(pic, dict, key)) {
    return pic_values2(pic, pic_dict_ref(pic, dict, key), pic_true_value());
  } else {
    return pic_values2(pic, pic_none_value(), pic_false_value());
  }
}

static pic_value
pic_dict_dictionary_set(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value key, val;

  pic_get_args(pic, "doo", &dict, &key, &val);

  pic_dict_set(pic, dict, key, val);

  return pic_none_value();
}

static pic_value
pic_dict_dictionary_del(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value key;

  pic_get_args(pic, "do", &dict, &key);

  pic_dict_del(pic, dict, key);

  return pic_none_value();
}

static pic_value
pic_dict_dictionary_size(pic_state *pic)
{
  struct pic_dict *dict;

  pic_get_args(pic, "d", &dict);

  return pic_int_value(pic_dict_size(pic, dict));
}

static pic_value
pic_dict_dictionary_to_alist(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value item, alist = pic_nil_value();
  xh_entry *it;

  pic_get_args(pic, "d", &dict);

  for (it = xh_begin(&dict->hash); it != NULL; it = xh_next(it)) {
    item = pic_cons(pic, xh_key(it, pic_value), xh_val(it, pic_value));
    pic_push(pic, item, alist);
  }

  return pic_reverse(pic, alist);
}

static pic_value
pic_dict_alist_to_dictionary(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value alist, e;

  pic_get_args(pic, "o", &alist);

  dict = pic_make_dict(pic);

  pic_for_each (e, pic_reverse(pic, alist)) {
    pic_dict_set(pic, dict, pic_car(pic, e), pic_cdr(pic, e));
  }

  return pic_obj_value(dict);
}

static pic_value
pic_dict_dictionary_to_plist(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value plist = pic_nil_value();
  xh_entry *it;

  pic_get_args(pic, "d", &dict);

  for (it = xh_begin(&dict->hash); it != NULL; it = xh_next(it)) {
    pic_push(pic, xh_key(it, pic_value), plist);
    pic_push(pic, xh_val(it, pic_value), plist);
  }

  return pic_reverse(pic, plist);
}

static pic_value
pic_dict_plist_to_dictionary(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value plist, e;

  pic_get_args(pic, "o", &plist);

  dict = pic_make_dict(pic);

  for (e = pic_reverse(pic, plist); ! pic_nil_p(e); e = pic_cddr(pic, e)) {
    pic_dict_set(pic, dict, pic_cadr(pic, e), pic_car(pic, e));
  }

  return pic_obj_value(dict);
}

void
pic_init_dict(pic_state *pic)
{
  pic_defun(pic, "make-dictionary", pic_dict_make_dictionary);
  pic_defun(pic, "dictionary?", pic_dict_dictionary_p);
  pic_defun(pic, "dictionary", pic_dict_dictionary);
  pic_defun(pic, "dictionary-ref", pic_dict_dictionary_ref);
  pic_defun(pic, "dictionary-set!", pic_dict_dictionary_set);
  pic_defun(pic, "dictionary-delete!", pic_dict_dictionary_del);
  pic_defun(pic, "dictionary-size", pic_dict_dictionary_size);
  pic_defun(pic, "dictionary->alist", pic_dict_dictionary_to_alist);
  pic_defun(pic, "alist->dictionary", pic_dict_alist_to_dictionary);
  pic_defun(pic, "dictionary->plist", pic_dict_dictionary_to_plist);
  pic_defun(pic, "plist->dictionary", pic_dict_plist_to_dictionary);
}
