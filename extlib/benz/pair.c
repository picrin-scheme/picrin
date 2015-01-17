/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"

pic_value
pic_cons(pic_state *pic, pic_value car, pic_value cdr)
{
  struct pic_pair *pair;

  pair = (struct pic_pair *)pic_obj_alloc(pic, sizeof(struct pic_pair), PIC_TT_PAIR);
  pair->car = car;
  pair->cdr = cdr;

  return pic_obj_value(pair);
}

void
pic_set_car(pic_state *pic, pic_value obj, pic_value val)
{
  struct pic_pair *pair;

  if (! pic_pair_p(obj)) {
    pic_errorf(pic, "pair required");
  }
  pair = pic_pair_ptr(obj);

  pair->car = val;
}

void
pic_set_cdr(pic_state *pic, pic_value obj, pic_value val)
{
  struct pic_pair *pair;

  if (! pic_pair_p(obj)) {
    pic_errorf(pic, "pair required");
  }
  pair = pic_pair_ptr(obj);

  pair->cdr = val;
}

bool
pic_list_p(pic_value obj)
{
  pic_value local, rapid;
  int i;

  /* Floyd's cycle-finding algorithm. */

  local = rapid = obj;
  while (true) {

    /* advance rapid fast-forward; runs 2x faster than local */
    for (i = 0; i < 2; ++i) {
      if (pic_pair_p(rapid)) {
        rapid = pic_pair_ptr(rapid)->cdr;
      }
      else {
        return pic_nil_p(rapid);
      }
    }

    /* advance local */
    local = pic_pair_ptr(local)->cdr;

    if (pic_eq_p(local, rapid)) {
      return false;
    }
  }
}

pic_value
pic_list1(pic_state *pic, pic_value obj1)
{
  return pic_cons(pic, obj1, pic_nil_value());
}

pic_value
pic_list2(pic_state *pic, pic_value obj1, pic_value obj2)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = pic_cons(pic, obj1, pic_list1(pic, obj2));

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, val);
  return val;
}

pic_value
pic_list3(pic_state *pic, pic_value obj1, pic_value obj2, pic_value obj3)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = pic_cons(pic, obj1, pic_list2(pic, obj2, obj3));

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, val);
  return val;
}

pic_value
pic_list4(pic_state *pic, pic_value obj1, pic_value obj2, pic_value obj3, pic_value obj4)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = pic_cons(pic, obj1, pic_list3(pic, obj2, obj3, obj4));

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, val);
  return val;
}

pic_value
pic_list5(pic_state *pic, pic_value obj1, pic_value obj2, pic_value obj3, pic_value obj4, pic_value obj5)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = pic_cons(pic, obj1, pic_list4(pic, obj2, obj3, obj4, obj5));

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, val);
  return val;
}

pic_value
pic_list6(pic_state *pic, pic_value obj1, pic_value obj2, pic_value obj3, pic_value obj4, pic_value obj5, pic_value obj6)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = pic_cons(pic, obj1, pic_list5(pic, obj2, obj3, obj4, obj5, obj6));

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, val);
  return val;
}

pic_value
pic_list7(pic_state *pic, pic_value obj1, pic_value obj2, pic_value obj3, pic_value obj4, pic_value obj5, pic_value obj6, pic_value obj7)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = pic_cons(pic, obj1, pic_list6(pic, obj2, obj3, obj4, obj5, obj6, obj7));

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, val);
  return val;
}

pic_value
pic_list_by_array(pic_state *pic, size_t c, pic_value *vs)
{
  pic_value v;

  v = pic_nil_value();
  while (c--) {
    v = pic_cons(pic, vs[c], v);
  }
  return v;
}

pic_value
pic_make_list(pic_state *pic, size_t k, pic_value fill)
{
  pic_value list;
  size_t i;

  list = pic_nil_value();
  for (i = 0; i < k; ++i) {
    list = pic_cons(pic, fill, list);
  }

  return list;
}

size_t
pic_length(pic_state *pic, pic_value obj)
{
  size_t c = 0;

  if (! pic_list_p(obj)) {
    pic_errorf(pic, "length: expected list, but got ~s", obj);
  }

  while (! pic_nil_p(obj)) {
    obj = pic_cdr(pic, obj);
    ++c;
  }

  return c;
}

pic_value
pic_reverse(pic_state *pic, pic_value list)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value v, acc;

  acc = pic_nil_value();
  pic_for_each(v, list) {
    acc = pic_cons(pic, v, acc);

    pic_gc_arena_restore(pic, ai);
    pic_gc_protect(pic, acc);
  }
  return acc;
}

pic_value
pic_append(pic_state *pic, pic_value xs, pic_value ys)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value x;

  xs = pic_reverse(pic, xs);
  pic_for_each (x, xs) {
    ys = pic_cons(pic, x, ys);

    pic_gc_arena_restore(pic, ai);
    pic_gc_protect(pic, xs);
    pic_gc_protect(pic, ys);
  }
  return ys;
}

pic_value
pic_memq(pic_state *pic, pic_value key, pic_value list)
{
 enter:

  if (pic_nil_p(list))
    return pic_false_value();

  if (pic_eq_p(key, pic_car(pic, list)))
    return list;

  list = pic_cdr(pic, list);
  goto enter;
}

pic_value
pic_memv(pic_state *pic, pic_value key, pic_value list)
{
 enter:

  if (pic_nil_p(list))
    return pic_false_value();

  if (pic_eqv_p(key, pic_car(pic, list)))
    return list;

  list = pic_cdr(pic, list);
  goto enter;
}

pic_value
pic_member(pic_state *pic, pic_value key, pic_value list, struct pic_proc *compar)
{
 enter:

  if (pic_nil_p(list))
    return pic_false_value();

  if (compar == NULL) {
    if (pic_equal_p(pic, key, pic_car(pic, list)))
      return list;
  } else {
    if (pic_test(pic_apply2(pic, compar, key, pic_car(pic, list))))
      return list;
  }

  list = pic_cdr(pic, list);
  goto enter;
}

pic_value
pic_assq(pic_state *pic, pic_value key, pic_value assoc)
{
  pic_value cell;

 enter:

  if (pic_nil_p(assoc))
    return pic_false_value();

  cell = pic_car(pic, assoc);
  if (pic_eq_p(key, pic_car(pic, cell)))
    return cell;

  assoc = pic_cdr(pic, assoc);
  goto enter;
}

pic_value
pic_assv(pic_state *pic, pic_value key, pic_value assoc)
{
  pic_value cell;

 enter:

  if (pic_nil_p(assoc))
    return pic_false_value();

  cell = pic_car(pic, assoc);
  if (pic_eqv_p(key, pic_car(pic, cell)))
    return cell;

  assoc = pic_cdr(pic, assoc);
  goto enter;
}

pic_value
pic_assoc(pic_state *pic, pic_value key, pic_value assoc, struct pic_proc *compar)
{
  pic_value cell;

 enter:

  if (pic_nil_p(assoc))
    return pic_false_value();

  cell = pic_car(pic, assoc);
  if (compar == NULL) {
    if (pic_equal_p(pic, key, pic_car(pic, cell)))
      return cell;
  } else {
    if (pic_test(pic_apply2(pic, compar, key, pic_car(pic, cell))))
      return cell;
  }

  assoc = pic_cdr(pic, assoc);
  goto enter;
}

pic_value
pic_acons(pic_state *pic, pic_value key, pic_value val, pic_value assoc)
{
  return pic_cons(pic, pic_cons(pic, key, val), assoc);
}

pic_value
pic_caar(pic_state *pic, pic_value v)
{
  return pic_car(pic, pic_car(pic, v));
}

pic_value
pic_cadr(pic_state *pic, pic_value v)
{
  return pic_car(pic, pic_cdr(pic, v));
}

pic_value
pic_cdar(pic_state *pic, pic_value v)
{
  return pic_cdr(pic, pic_car(pic, v));
}

pic_value
pic_cddr(pic_state *pic, pic_value v)
{
  return pic_cdr(pic, pic_cdr(pic, v));
}

pic_value
pic_list_tail(pic_state *pic, pic_value list, size_t i)
{
  while (i-- > 0) {
    list = pic_cdr(pic, list);
  }
  return list;
}

pic_value
pic_list_ref(pic_state *pic, pic_value list, size_t i)
{
  return pic_car(pic, pic_list_tail(pic, list, i));
}

void
pic_list_set(pic_state *pic, pic_value list, size_t i, pic_value obj)
{
  pic_pair_ptr(pic_list_tail(pic, list, i))->car = obj;
}

pic_value
pic_list_copy(pic_state *pic, pic_value obj)
{
  if (pic_pair_p(obj)) {
    return pic_cons(pic, pic_car(pic, obj), pic_list_copy(pic, pic_cdr(pic, obj)));
  }
  else {
    return obj;
  }
}

static pic_value
pic_pair_pair_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_pair_p(v));
}

static pic_value
pic_pair_cons(pic_state *pic)
{
  pic_value v,w;

  pic_get_args(pic, "oo", &v, &w);

  return pic_cons(pic, v, w);
}

static pic_value
pic_pair_car(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_car(pic, v);
}

static pic_value
pic_pair_cdr(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_cdr(pic, v);
}

static pic_value
pic_pair_caar(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_caar(pic, v);
}

static pic_value
pic_pair_cadr(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_cadr(pic, v);
}

static pic_value
pic_pair_cdar(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_cdar(pic, v);
}

static pic_value
pic_pair_cddr(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_cddr(pic, v);
}

static pic_value
pic_pair_set_car(pic_state *pic)
{
  pic_value v,w;

  pic_get_args(pic, "oo", &v, &w);

  pic_set_car(pic, v, w);

  return pic_none_value();
}

static pic_value
pic_pair_set_cdr(pic_state *pic)
{
  pic_value v,w;

  pic_get_args(pic, "oo", &v, &w);

  pic_set_cdr(pic, v, w);

  return pic_none_value();
}

static pic_value
pic_pair_null_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_nil_p(v));
}

static pic_value
pic_pair_list_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_list_p(v));
}

static pic_value
pic_pair_make_list(pic_state *pic)
{
  size_t i;
  pic_value fill = pic_none_value();

  pic_get_args(pic, "k|o", &i, &fill);

  return pic_make_list(pic, i, fill);
}

static pic_value
pic_pair_list(pic_state *pic)
{
  size_t argc;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  return pic_list_by_array(pic, argc, argv);
}

static pic_value
pic_pair_length(pic_state *pic)
{
  pic_value list;

  pic_get_args(pic, "o", &list);

  return pic_size_value(pic_length(pic, list));
}

static pic_value
pic_pair_append(pic_state *pic)
{
  size_t argc;
  pic_value *args, list;

  pic_get_args(pic, "*", &argc, &args);

  if (argc == 0) {
    return pic_nil_value();
  }

  list = args[--argc];

  while (argc-- > 0) {
    list = pic_append(pic, args[argc], list);
  }
  return list;
}

static pic_value
pic_pair_reverse(pic_state *pic)
{
  pic_value list;

  pic_get_args(pic, "o", &list);

  return pic_reverse(pic, list);
}

static pic_value
pic_pair_list_tail(pic_state *pic)
{
  pic_value list;
  size_t i;

  pic_get_args(pic, "ok", &list, &i);

  return pic_list_tail(pic, list, i);
}

static pic_value
pic_pair_list_ref(pic_state *pic)
{
  pic_value list;
  size_t i;

  pic_get_args(pic, "ok", &list, &i);

  return pic_list_ref(pic, list, i);
}

static pic_value
pic_pair_list_set(pic_state *pic)
{
  pic_value list, obj;
  size_t i;

  pic_get_args(pic, "oko", &list, &i, &obj);

  pic_list_set(pic, list, i, obj);

  return pic_none_value();
}

static pic_value
pic_pair_list_copy(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_list_copy(pic, obj);
}

static pic_value
pic_pair_map(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc, i;
  pic_value *args;
  pic_value arg, ret;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  ret = pic_nil_value();
  do {
    arg = pic_nil_value();
    for (i = 0; i < argc; ++i) {
      if (! pic_pair_p(args[i])) {
        break;
      }
      pic_push(pic, pic_car(pic, args[i]), arg);
      args[i] = pic_cdr(pic, args[i]);
    }
    if (i != argc) {
      break;
    }
    pic_push(pic, pic_apply(pic, proc, pic_reverse(pic, arg)), ret);
  } while (1);

  return pic_reverse(pic, ret);
}

static pic_value
pic_pair_for_each(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc, i;
  pic_value *args;
  pic_value arg;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  do {
    arg = pic_nil_value();
    for (i = 0; i < argc; ++i) {
      if (! pic_pair_p(args[i])) {
        break;
      }
      pic_push(pic, pic_car(pic, args[i]), arg);
      args[i] = pic_cdr(pic, args[i]);
    }
    if (i != argc) {
      break;
    }
    pic_apply(pic, proc, pic_reverse(pic, arg));
  } while (1);

  return pic_none_value();
}

static pic_value
pic_pair_memq(pic_state *pic)
{
  pic_value key, list;

  pic_get_args(pic, "oo", &key, &list);

  return pic_memq(pic, key, list);
}

static pic_value
pic_pair_memv(pic_state *pic)
{
  pic_value key, list;

  pic_get_args(pic, "oo", &key, &list);

  return pic_memv(pic, key, list);
}

static pic_value
pic_pair_member(pic_state *pic)
{
  struct pic_proc *proc = NULL;
  pic_value key, list;

  pic_get_args(pic, "oo|l", &key, &list, &proc);

  return pic_member(pic, key, list, proc);
}

static pic_value
pic_pair_assq(pic_state *pic)
{
  pic_value key, list;

  pic_get_args(pic, "oo", &key, &list);

  return pic_assq(pic, key, list);
}

static pic_value
pic_pair_assv(pic_state *pic)
{
  pic_value key, list;

  pic_get_args(pic, "oo", &key, &list);

  return pic_assv(pic, key, list);
}

static pic_value
pic_pair_assoc(pic_state *pic)
{
  struct pic_proc *proc = NULL;
  pic_value key, list;

  pic_get_args(pic, "oo|l", &key, &list, &proc);

  return pic_assoc(pic, key, list, proc);
}

void
pic_init_pair(pic_state *pic)
{
  pic_defun(pic, "pair?", pic_pair_pair_p);
  pic_defun(pic, "cons", pic_pair_cons);
  pic_defun(pic, "car", pic_pair_car);
  pic_defun(pic, "cdr", pic_pair_cdr);
  pic_defun(pic, "set-car!", pic_pair_set_car);
  pic_defun(pic, "set-cdr!", pic_pair_set_cdr);
  pic_defun(pic, "null?", pic_pair_null_p);

  pic_defun(pic, "caar", pic_pair_caar);
  pic_defun(pic, "cadr", pic_pair_cadr);
  pic_defun(pic, "cdar", pic_pair_cdar);
  pic_defun(pic, "cddr", pic_pair_cddr);
  pic_defun(pic, "list?", pic_pair_list_p);
  pic_defun(pic, "make-list", pic_pair_make_list);
  pic_defun(pic, "list", pic_pair_list);
  pic_defun(pic, "length", pic_pair_length);
  pic_defun(pic, "append", pic_pair_append);
  pic_defun(pic, "reverse", pic_pair_reverse);
  pic_defun(pic, "list-tail", pic_pair_list_tail);
  pic_defun(pic, "list-ref", pic_pair_list_ref);
  pic_defun(pic, "list-set!", pic_pair_list_set);
  pic_defun(pic, "list-copy", pic_pair_list_copy);
  pic_defun(pic, "map", pic_pair_map);
  pic_defun(pic, "for-each", pic_pair_for_each);
  pic_defun(pic, "memq", pic_pair_memq);
  pic_defun(pic, "memv", pic_pair_memv);
  pic_defun(pic, "member", pic_pair_member);
  pic_defun(pic, "assq", pic_pair_assq);
  pic_defun(pic, "assv", pic_pair_assv);
  pic_defun(pic, "assoc", pic_pair_assoc);
}
