/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/dict.h"
#include "picrin/cont.h"

struct pic_dict *
pic_dict_new(pic_state *pic)
{
  struct pic_dict *dict;

  dict = (struct pic_dict *)pic_obj_alloc(pic, sizeof(struct pic_dict), PIC_TT_DICT);
  xh_init_int(&dict->hash, sizeof(pic_value));

  return dict;
}

pic_value
pic_dict_ref(pic_state *pic, struct pic_dict *dict, pic_sym key)
{
  xh_entry *e;

  e = xh_get_int(&dict->hash, key);
  if (! e) {
    pic_errorf(pic, "element not found for a key: ~s", pic_sym_value(key));
  }
  return xh_val(e, pic_value);
}

void
pic_dict_set(pic_state *pic, struct pic_dict *dict, pic_sym key, pic_value val)
{
  UNUSED(pic);

  xh_put_int(&dict->hash, key, &val);
}

size_t
pic_dict_size(pic_state *pic, struct pic_dict *dict)
{
  UNUSED(pic);

  return dict->hash.count;
}

bool
pic_dict_has(pic_state *pic, struct pic_dict *dict, pic_sym key)
{
  UNUSED(pic);

  return xh_get_int(&dict->hash, key) != NULL;
}

void
pic_dict_del(pic_state *pic, struct pic_dict *dict, pic_sym key)
{
  if (xh_get_int(&dict->hash, key) == NULL) {
    pic_errorf(pic, "no slot named ~s found in dictionary", pic_sym_value(key));
  }

  xh_del_int(&dict->hash, key);
}

static pic_value
pic_dict_dict(pic_state *pic)
{
  struct pic_dict *dict;

  pic_get_args(pic, "");

  dict = pic_dict_new(pic);

  return pic_obj_value(dict);
}

static pic_value
pic_dict_dict_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic_dict_p(obj));
}

static pic_value
pic_dict_dict_ref(pic_state *pic)
{
  struct pic_dict *dict;
  pic_sym key;

  pic_get_args(pic, "dm", &dict, &key);

  if (pic_dict_has(pic, dict, key)) {
    return pic_values2(pic, pic_dict_ref(pic, dict, key), pic_true_value());
  } else {
    return pic_values2(pic, pic_none_value(), pic_false_value());
  }
}

static pic_value
pic_dict_dict_set(pic_state *pic)
{
  struct pic_dict *dict;
  pic_sym key;
  pic_value val;

  pic_get_args(pic, "dmo", &dict, &key, &val);

  pic_dict_set(pic, dict, key, val);

  return pic_none_value();
}

static pic_value
pic_dict_dict_del(pic_state *pic)
{
  struct pic_dict *dict;
  pic_sym key;

  pic_get_args(pic, "dm", &dict, &key);

  pic_dict_del(pic, dict, key);

  return pic_none_value();
}

static pic_value
pic_dict_dict_size(pic_state *pic)
{
  struct pic_dict *dict;

  pic_get_args(pic, "d", &dict);

  return pic_int_value(pic_dict_size(pic, dict));
}

static pic_value
pic_dict_dict_for_each(pic_state *pic)
{
  struct pic_proc *proc;
  struct pic_dict *dict;
  xh_iter it;

  pic_get_args(pic, "ld", &proc, &dict);

  xh_begin(&it, &dict->hash);
  while (xh_next(&it)) {
    pic_apply2(pic, proc, pic_sym_value(xh_key(it.e, pic_sym)), xh_val(it.e, pic_value));
  }

  return pic_none_value();
}

void
pic_init_dict(pic_state *pic)
{
  pic_deflibrary (pic, "(picrin dictionary)") {
    pic_defun(pic, "make-dictionary", pic_dict_dict);
    pic_defun(pic, "dictionary?", pic_dict_dict_p);
    pic_defun(pic, "dictionary-ref", pic_dict_dict_ref);
    pic_defun(pic, "dictionary-set!", pic_dict_dict_set);
    pic_defun(pic, "dictionary-delete", pic_dict_dict_del);
    pic_defun(pic, "dictionary-size", pic_dict_dict_size);
    pic_defun(pic, "dictionary-for-each", pic_dict_dict_for_each);
  }
}
