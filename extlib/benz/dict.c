/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/dict.h"
#include "picrin/cont.h"
#include "picrin/pair.h"
#include "picrin/error.h"

struct pic_dict *
pic_make_dict(pic_state *pic)
{
  struct pic_dict *dict;

  dict = (struct pic_dict *)pic_obj_alloc(pic, sizeof(struct pic_dict), PIC_TT_DICT);
  xh_init_ptr(&dict->hash, sizeof(pic_value));

  return dict;
}

pic_value
pic_dict_ref(pic_state *pic, struct pic_dict *dict, pic_sym key)
{
  xh_entry *e;

  e = xh_get_ptr(&dict->hash, key);
  if (! e) {
    pic_errorf(pic, "element not found for a key: ~s", pic_obj_value(key));
  }
  return xh_val(e, pic_value);
}

void
pic_dict_set(pic_state *pic, struct pic_dict *dict, pic_sym key, pic_value val)
{
  PIC_UNUSED(pic);

  xh_put_ptr(&dict->hash, key, &val);
}

size_t
pic_dict_size(pic_state *pic, struct pic_dict *dict)
{
  PIC_UNUSED(pic);

  return dict->hash.count;
}

bool
pic_dict_has(pic_state *pic, struct pic_dict *dict, pic_sym key)
{
  PIC_UNUSED(pic);

  return xh_get_ptr(&dict->hash, key) != NULL;
}

void
pic_dict_del(pic_state *pic, struct pic_dict *dict, pic_sym key)
{
  if (xh_get_ptr(&dict->hash, key) == NULL) {
    pic_errorf(pic, "no slot named ~s found in dictionary", pic_obj_value(key));
  }

  xh_del_ptr(&dict->hash, key);
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
    pic_assert_type(pic, argv[i], sym);
    pic_dict_set(pic, dict, pic_sym(argv[i]), argv[i+1]);
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
  pic_sym key;

  pic_get_args(pic, "dm", &dict, &key);

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
  pic_sym key;
  pic_value val;

  pic_get_args(pic, "dmo", &dict, &key, &val);

  pic_dict_set(pic, dict, key, val);

  return pic_none_value();
}

static pic_value
pic_dict_dictionary_del(pic_state *pic)
{
  struct pic_dict *dict;
  pic_sym key;

  pic_get_args(pic, "dm", &dict, &key);

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
pic_dict_dictionary_map(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc, i;
  pic_value *args;
  pic_value arg, ret;
  xh_entry **it;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  it = pic_alloc(pic, argc * sizeof(xh_entry));
  for (i = 0; i < argc; ++i) {
    if (! pic_dict_p(args[i])) {
      pic_free(pic, it);
      pic_errorf(pic, "expected dict, but got %s", pic_type_repr(pic_type(args[i])));
    }
    it[i] = xh_begin(&pic_dict_ptr(args[i])->hash);
  }

  pic_try {
    ret = pic_nil_value();
    do {
      arg = pic_nil_value();
      for (i = 0; i < argc; ++i) {
        if (it[i] == NULL) {
          break;
        }
        pic_push(pic, pic_obj_value(xh_key(it[i], pic_sym)), arg);
        it[i] = xh_next(it[i]);
      }
      if (i != argc) {
        break;
      }
      pic_push(pic, pic_apply(pic, proc, pic_reverse(pic, arg)), ret);
    } while (1);
  }
  pic_catch {
    pic_free(pic, it);
    pic_raise(pic, pic->err);
  }

  pic_free(pic, it);

  return pic_reverse(pic, ret);
}

static pic_value
pic_dict_dictionary_for_each(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc, i;
  pic_value *args;
  pic_value arg;
  xh_entry **it;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  it = pic_alloc(pic, argc * sizeof(xh_entry));
  for (i = 0; i < argc; ++i) {
    if (! pic_dict_p(args[i])) {
      pic_free(pic, it);
      pic_errorf(pic, "expected dict, but got %s", pic_type_repr(pic_type(args[i])));
    }
    it[i] = xh_begin(&pic_dict_ptr(args[i])->hash);
  }

  pic_try {
    do {
      arg = pic_nil_value();
      for (i = 0; i < argc; ++i) {
        if (it[i] == NULL) {
          break;
        }
        pic_push(pic, pic_obj_value(xh_key(it[i], pic_sym)), arg);
        it[i] = xh_next(it[i]);
      }
      if (i != argc) {
        break;
      }
      pic_void(pic_apply(pic, proc, pic_reverse(pic, arg)));
    } while (1);
  }
  pic_catch {
    pic_free(pic, it);
    pic_raise(pic, pic->err);
  }

  pic_free(pic, it);

  return pic_none_value();
}

static pic_value
pic_dict_dictionary_to_alist(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value item, alist = pic_nil_value();
  xh_entry *it;

  pic_get_args(pic, "d", &dict);

  for (it = xh_begin(&dict->hash); it != NULL; it = xh_next(it)) {
    item = pic_cons(pic, pic_obj_value(xh_key(it, pic_sym)), xh_val(it, pic_value));
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
    pic_assert_type(pic, pic_car(pic, e), sym);
    pic_dict_set(pic, dict, pic_sym(pic_car(pic, e)), pic_cdr(pic, e));
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
    pic_push(pic, pic_obj_value(xh_key(it, pic_sym)), plist);
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
    pic_assert_type(pic, pic_cadr(pic, e), sym);
    pic_dict_set(pic, dict, pic_sym(pic_cadr(pic, e)), pic_car(pic, e));
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
  pic_defun(pic, "dictionary-map", pic_dict_dictionary_map);
  pic_defun(pic, "dictionary-for-each", pic_dict_dictionary_for_each);
  pic_defun(pic, "dictionary->alist", pic_dict_dictionary_to_alist);
  pic_defun(pic, "alist->dictionary", pic_dict_alist_to_dictionary);
  pic_defun(pic, "dictionary->plist", pic_dict_dictionary_to_plist);
  pic_defun(pic, "plist->dictionary", pic_dict_plist_to_dictionary);
}
