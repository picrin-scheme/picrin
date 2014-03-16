/**
 * See Copyright Notice in picrin.h
 */

#include <stdarg.h>

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

pic_value
pic_car(pic_state *pic, pic_value obj)
{
  struct pic_pair *pair;

  if (! pic_pair_p(obj)) {
    pic_error(pic, "pair required");
  }
  pair = pic_pair_ptr(obj);

  return pair->car;
}

pic_value
pic_cdr(pic_state *pic, pic_value obj)
{
  struct pic_pair *pair;

  if (! pic_pair_p(obj)) {
    pic_error(pic, "pair required");
  }
  pair = pic_pair_ptr(obj);

  return pair->cdr;
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
  int ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = pic_cons(pic, obj1, pic_list1(pic, obj2));

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, val);
  return val;
}

pic_value
pic_list3(pic_state *pic, pic_value obj1, pic_value obj2, pic_value obj3)
{
  int ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = pic_cons(pic, obj1, pic_list2(pic, obj2, obj3));

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, val);
  return val;
}

pic_value
pic_list4(pic_state *pic, pic_value obj1, pic_value obj2, pic_value obj3, pic_value obj4)
{
  int ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = pic_cons(pic, obj1, pic_list3(pic, obj2, obj3, obj4));

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, val);
  return val;
}

pic_value
pic_list5(pic_state *pic, pic_value obj1, pic_value obj2, pic_value obj3, pic_value obj4, pic_value obj5)
{
  int ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = pic_cons(pic, obj1, pic_list4(pic, obj2, obj3, obj4, obj5));

  pic_gc_arena_restore(pic, ai);
  pic_gc_protect(pic, val);
  return val;
}

pic_value
pic_list6(pic_state *pic, pic_value obj1, pic_value obj2, pic_value obj3, pic_value obj4, pic_value obj5, pic_value obj6)
{
  int ai = pic_gc_arena_preserve(pic);
  pic_value val;

  val = pic_cons(pic, obj1, pic_list5(pic, obj2, obj3, obj4, obj5, obj6));

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
pic_make_list(pic_state *pic, int k, pic_value fill)
{
  pic_value list;
  int i;

  list = pic_nil_value();
  for (i = 0; i < k; ++i) {
    list = pic_cons(pic, fill, list);
  }

  return list;
}

int
pic_length(pic_state *pic, pic_value obj)
{
  int c = 0;

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
  int ai = pic_gc_arena_preserve(pic);
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
  int ai = pic_gc_arena_preserve(pic);
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
pic_assoc(pic_state *pic, pic_value key, pic_value assoc)
{
  pic_value cell;

 enter:

  if (pic_nil_p(assoc))
    return pic_false_value();

  cell = pic_car(pic, assoc);
  if (pic_equal_p(pic, key, pic_car(pic, cell)))
    return cell;

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
pic_list_tail(pic_state *pic, pic_value list, int i)
{
  while (i-- > 0) {
    list = pic_cdr(pic, list);
  }
  return list;
}

pic_value
pic_list_ref(pic_state *pic, pic_value list, int i)
{
  return pic_car(pic, pic_list_tail(pic, list, i));
}

void
pic_list_set(pic_state *pic, pic_value list, int i, pic_value obj)
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

  if (! pic_pair_p(v))
    pic_error(pic, "pair expected");

  pic_pair_ptr(v)->car = w;
  return pic_none_value();
}

static pic_value
pic_pair_set_cdr(pic_state *pic)
{
  pic_value v,w;

  pic_get_args(pic, "oo", &v, &w);

  if (! pic_pair_p(v))
    pic_error(pic, "pair expected");

  pic_pair_ptr(v)->cdr = w;
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
  int i;
  pic_value fill = pic_none_value();

  pic_get_args(pic, "i|o", &i, &fill);

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

  return pic_int_value(pic_length(pic, list));
}

static pic_value
pic_pair_append(pic_state *pic)
{
  size_t argc;
  pic_value *args, list;

  pic_get_args(pic, "*", &argc, &args);

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
  int i;

  pic_get_args(pic, "oi", &list, &i);

  return pic_list_tail(pic, list, i);
}

static pic_value
pic_pair_list_ref(pic_state *pic)
{
  pic_value list;
  int i;

  pic_get_args(pic, "oi", &list, &i);

  return pic_list_ref(pic, list, i);
}

static pic_value
pic_pair_list_set(pic_state *pic)
{
  pic_value list, obj;
  int i;

  pic_get_args(pic, "oio", &list, &i, &obj);

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

void
pic_init_pair(pic_state *pic)
{
  pic_defun(pic, "pair?", pic_pair_pair_p);
  pic_defun(pic, "cons", pic_pair_cons);
  pic_defun(pic, "car", pic_pair_car);
  pic_defun(pic, "cdr", pic_pair_cdr);
  pic_defun(pic, "set-car!", pic_pair_set_car);
  pic_defun(pic, "set-cdr!", pic_pair_set_cdr);
  pic_defun(pic, "caar", pic_pair_caar);
  pic_defun(pic, "cadr", pic_pair_cadr);
  pic_defun(pic, "cdar", pic_pair_cdar);
  pic_defun(pic, "cddr", pic_pair_cddr);
  pic_defun(pic, "null?", pic_pair_null_p);
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
}
