/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

KHASH_DEFINE(dict, pic_sym *, pic_value, kh_ptr_hash_func, kh_ptr_hash_equal)

struct pic_dict *
pic_make_dict(pic_state *pic)
{
  struct pic_dict *dict;

  dict = (struct pic_dict *)pic_obj_alloc(pic, sizeof(struct pic_dict), PIC_TT_DICT);
  kh_init(dict, &dict->hash);

  return dict;
}

pic_value
pic_dict_ref(pic_state *pic, struct pic_dict *dict, pic_sym *key)
{
  khash_t(dict) *h = &dict->hash;
  khiter_t it;

  it = kh_get(dict, h, key);
  if (it == kh_end(h)) {
    pic_errorf(pic, "element not found for a key: ~s", pic_obj_value(key));
  }
  return kh_val(h, it);
}

void
pic_dict_set(pic_state PIC_UNUSED(*pic), struct pic_dict *dict, pic_sym *key, pic_value val)
{
  khash_t(dict) *h = &dict->hash;
  int ret;
  khiter_t it;

  it = kh_put(dict, h, key, &ret);
  kh_val(h, it) = val;
}

size_t
pic_dict_size(pic_state PIC_UNUSED(*pic), struct pic_dict *dict)
{
  return kh_size(&dict->hash);
}

bool
pic_dict_has(pic_state PIC_UNUSED(*pic), struct pic_dict *dict, pic_sym *key)
{
  return kh_get(dict, &dict->hash, key) != kh_end(&dict->hash);
}

void
pic_dict_del(pic_state *pic, struct pic_dict *dict, pic_sym *key)
{
  khash_t(dict) *h = &dict->hash;
  khiter_t it;

  it = kh_get(dict, h, key);
  if (it == kh_end(h)) {
    pic_errorf(pic, "no slot named ~s found in dictionary", pic_obj_value(key));
  }
  kh_del(dict, h, it);
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
    pic_dict_set(pic, dict, pic_sym_ptr(argv[i]), argv[i+1]);
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
  pic_sym *key;

  pic_get_args(pic, "dm", &dict, &key);

  if (! pic_dict_has(pic, dict, key)) {
    return pic_undef_value();
  }
  return pic_dict_ref(pic, dict, key);
}

static pic_value
pic_dict_dictionary_set(pic_state *pic)
{
  struct pic_dict *dict;
  pic_sym *key;
  pic_value val;

  pic_get_args(pic, "dmo", &dict, &key, &val);

  if (pic_undef_p(val)) {
    if (pic_dict_has(pic, dict, key)) {
      pic_dict_del(pic, dict, key);
    }
  }
  else {
    pic_dict_set(pic, dict, key, val);
  }
  return pic_undef_value();
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
  pic_value arg_list, ret = pic_nil_value();

  pic_get_args(pic, "l*", &proc, &argc, &args);

  if (argc != 0) {
    khiter_t it[argc];
    khash_t(dict) *kh[argc];

    for (i = 0; i < argc; ++i) {
      if (! pic_dict_p(args[i])) {
        pic_errorf(pic, "expected dict, but got %s", pic_type_repr(pic_type(args[i])));
      }
      kh[i] = &pic_dict_ptr(args[i])->hash;
      it[i] = kh_begin(kh[i]);
    }

    do {
      arg_list = pic_nil_value();
      for (i = 0; i < argc; ++i) {
        while (it[i] != kh_end(kh[i])) { /* find next available */
          if (kh_exist(kh[i], it[i]))
            break;
          it[i]++;
        }
        if (it[i] == kh_end(kh[i])) {
          break;
        }
        pic_push(pic, pic_obj_value(kh_key(kh[i], it[i]++)), arg_list);
      }
      if (i != argc) {
        break;
      }
      pic_push(pic, pic_apply(pic, proc, pic_reverse(pic, arg_list)), ret);
    } while (1);
  }

  return pic_reverse(pic, ret);
}

static pic_value
pic_dict_dictionary_for_each(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc, i;
  pic_value *args;
  pic_value arg_list;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  if (argc != 0) {
    khiter_t it[argc];
    khash_t(dict) *kh[argc];

    for (i = 0; i < argc; ++i) {
      if (! pic_dict_p(args[i])) {
        pic_errorf(pic, "expected dict, but got %s", pic_type_repr(pic_type(args[i])));
      }
      kh[i] = &pic_dict_ptr(args[i])->hash;
      it[i] = kh_begin(kh[i]);
    }

    do {
      arg_list = pic_nil_value();
      for (i = 0; i < argc; ++i) {
        while (it[i] != kh_end(kh[i])) { /* find next available */
          if (kh_exist(kh[i], it[i]))
            break;
          it[i]++;
        }
        if (it[i] == kh_end(kh[i])) {
          break;
        }
        pic_push(pic, pic_obj_value(kh_key(kh[i], it[i]++)), arg_list);
      }
      if (i != argc) {
        break;
      }
      pic_void(pic_apply(pic, proc, pic_reverse(pic, arg_list)));
    } while (1);
  }

  return pic_undef_value();
}

static pic_value
pic_dict_dictionary_to_alist(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value item, alist = pic_nil_value();
  pic_sym *sym;
  khiter_t it;

  pic_get_args(pic, "d", &dict);

  pic_dict_for_each (sym, dict, it) {
    item = pic_cons(pic, pic_obj_value(sym), pic_dict_ref(pic, dict, sym));
    pic_push(pic, item, alist);
  }

  return pic_reverse(pic, alist);
}

static pic_value
pic_dict_alist_to_dictionary(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value alist, e, it;

  pic_get_args(pic, "o", &alist);

  dict = pic_make_dict(pic);

  pic_for_each (e, pic_reverse(pic, alist), it) {
    pic_assert_type(pic, pic_car(pic, e), sym);
    pic_dict_set(pic, dict, pic_sym_ptr(pic_car(pic, e)), pic_cdr(pic, e));
  }

  return pic_obj_value(dict);
}

static pic_value
pic_dict_dictionary_to_plist(pic_state *pic)
{
  struct pic_dict *dict;
  pic_value plist = pic_nil_value();
  pic_sym *sym;
  khiter_t it;

  pic_get_args(pic, "d", &dict);

  pic_dict_for_each (sym, dict, it) {
    pic_push(pic, pic_obj_value(sym), plist);
    pic_push(pic, pic_dict_ref(pic, dict, sym), plist);
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
    pic_dict_set(pic, dict, pic_sym_ptr(pic_cadr(pic, e)), pic_car(pic, e));
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
  pic_defun(pic, "dictionary-size", pic_dict_dictionary_size);
  pic_defun(pic, "dictionary-map", pic_dict_dictionary_map);
  pic_defun(pic, "dictionary-for-each", pic_dict_dictionary_for_each);
  pic_defun(pic, "dictionary->alist", pic_dict_dictionary_to_alist);
  pic_defun(pic, "alist->dictionary", pic_dict_alist_to_dictionary);
  pic_defun(pic, "dictionary->plist", pic_dict_dictionary_to_plist);
  pic_defun(pic, "plist->dictionary", pic_dict_plist_to_dictionary);
}
