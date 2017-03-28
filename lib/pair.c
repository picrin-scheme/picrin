/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"

pic_value
pic_cons(pic_state *pic, pic_value car, pic_value cdr)
{
  struct pair *pair;

  pair = (struct pair *)pic_obj_alloc(pic, sizeof(struct pair), PIC_TYPE_PAIR);
  pair->car = car;
  pair->cdr = cdr;

  return pic_obj_value(pair);
}

pic_value
pic_car(pic_state *pic, pic_value obj)
{
  if (! pic_pair_p(pic, obj)) {
    pic_error(pic, "car: pair required", 1, obj);
  }
  return pic_pair_ptr(pic, obj)->car;
}

pic_value
pic_cdr(pic_state *pic, pic_value obj)
{
  if (! pic_pair_p(pic, obj)) {
    pic_error(pic, "cdr: pair required", 1, obj);
  }
  return pic_pair_ptr(pic, obj)->cdr;
}

void
pic_set_car(pic_state *pic, pic_value obj, pic_value val)
{
  if (! pic_pair_p(pic, obj)) {
    pic_error(pic, "pair required", 0);
  }
  pic_pair_ptr(pic, obj)->car = val;
}

void
pic_set_cdr(pic_state *pic, pic_value obj, pic_value val)
{
  if (! pic_pair_p(pic, obj)) {
    pic_error(pic, "pair required", 0);
  }
  pic_pair_ptr(pic, obj)->cdr = val;
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

bool
pic_list_p(pic_state *pic, pic_value obj)
{
  pic_value local, rapid;
  int i;

  /* Floyd's cycle-finding algorithm. */

  local = rapid = obj;
  while (true) {

    /* advance rapid fast-forward; runs 2x faster than local */
    for (i = 0; i < 2; ++i) {
      if (pic_pair_p(pic, rapid)) {
        rapid = pic_pair_ptr(pic, rapid)->cdr;
      }
      else {
        return pic_nil_p(pic, rapid);
      }
    }

    /* advance local */
    local = pic_pair_ptr(pic, local)->cdr;

    if (pic_eq_p(pic, local, rapid)) {
      return false;
    }
  }
}

pic_value
pic_make_list(pic_state *pic, int n, pic_value *argv)
{
  pic_value list;
  int i;

  list = pic_nil_value(pic);
  for (i = n - 1; i >= 0; --i) {
    list = pic_cons(pic, argv[i], list);
  }
  return list;
}

pic_value
pic_list(pic_state *pic, int n, ...)
{
  va_list ap;
  pic_value list;

  va_start(ap, n);
  list = pic_vlist(pic, n, ap);
  va_end(ap);
  return list;
}

pic_value
pic_vlist(pic_state *pic, int n, va_list ap)
{
  pic_value *argv = pic_alloca(pic, sizeof(pic_value) * n);
  int i;

  for (i = 0; i < n; ++i) {
    argv[i] = va_arg(ap, pic_value);
  }
  return pic_make_list(pic, n, argv);
}

pic_value
pic_list_ref(pic_state *pic, pic_value list, int i)
{
  return pic_car(pic, pic_list_tail(pic, list, i));
}

void
pic_list_set(pic_state *pic, pic_value list, int i, pic_value obj)
{
  pic_pair_ptr(pic, pic_list_tail(pic, list, i))->car = obj;
}

pic_value
pic_list_tail(pic_state *pic, pic_value list, int i)
{
  while (i-- > 0) {
    list = pic_cdr(pic, list);
  }
  return list;
}

int
pic_length(pic_state *pic, pic_value obj)
{
  int c = 0;

  while (! pic_nil_p(pic, obj)) {
    obj = pic_cdr(pic, obj);
    ++c;
  }
  return c;
}

pic_value
pic_reverse(pic_state *pic, pic_value list)
{
  size_t ai = pic_enter(pic);
  pic_value v, acc, it;

  acc = pic_nil_value(pic);
  pic_for_each(v, list, it) {
    acc = pic_cons(pic, v, acc);

    pic_leave(pic, ai);
    pic_protect(pic, acc);
  }
  return acc;
}

pic_value
pic_append(pic_state *pic, pic_value xs, pic_value ys)
{
  size_t ai = pic_enter(pic);
  pic_value x, it;

  xs = pic_reverse(pic, xs);
  pic_for_each (x, xs, it) {
    ys = pic_cons(pic, x, ys);

    pic_leave(pic, ai);
    pic_protect(pic, xs);
    pic_protect(pic, ys);
  }
  return ys;
}

static pic_value
pic_pair_pair_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_pair_p(pic, v));
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

  return pic_undef_value(pic);
}

static pic_value
pic_pair_set_cdr(pic_state *pic)
{
  pic_value v,w;

  pic_get_args(pic, "oo", &v, &w);

  pic_set_cdr(pic, v, w);

  return pic_undef_value(pic);
}

static pic_value
pic_pair_null_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_nil_p(pic, v));
}

static pic_value
pic_pair_list_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_list_p(pic, v));
}

static pic_value
pic_pair_make_list(pic_state *pic)
{
  int k, i;
  pic_value list, fill = pic_undef_value(pic);

  pic_get_args(pic, "i|o", &k, &fill);

  list = pic_nil_value(pic);
  for (i = 0; i < k; ++i) {
    list = pic_cons(pic, fill, list);
  }
  return list;
}

static pic_value
pic_pair_list(pic_state *pic)
{
  int argc;
  pic_value *argv;

  pic_get_args(pic, "*", &argc, &argv);

  return pic_make_list(pic, argc, argv);
}

static pic_value
pic_pair_length(pic_state *pic)
{
  pic_value list;

  pic_get_args(pic, "o", &list);

  return pic_int_value(pic, pic_length(pic, list));
}

static pic_value
pic_pair_append(pic_state *pic)
{
  int argc;
  pic_value *args, list;

  pic_get_args(pic, "*", &argc, &args);

  if (argc == 0) {
    return pic_nil_value(pic);
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

  return pic_undef_value(pic);
}

static pic_value
pic_pair_list_copy(pic_state *pic)
{
  pic_value list, head, tail, tmp;

  pic_get_args(pic, "o", &list);

  head = tail = pic_nil_value(pic);

  while (pic_pair_p(pic, list)) {
    tmp = pic_list(pic, 1, pic_car(pic, list));
    if (! pic_nil_p(pic, tail)) {
      pic_set_cdr(pic, tail, tmp);
    }
    tail = tmp;
    if (pic_nil_p(pic, head)) {
      head = tail;
    }
    list = pic_cdr(pic, list);
  }
  if (pic_nil_p(pic, tail)) {
    return list;
  }
  pic_set_cdr(pic, tail, list);
  return head;
}

static pic_value
pic_pair_map(pic_state *pic)
{
  int argc, i;
  pic_value proc, *args, *arg_list, ret;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  if (argc == 0)
    pic_error(pic, "map: wrong number of arguments (1 for at least 2)", 0);

  arg_list = pic_alloca(pic, sizeof(pic_value) * argc);

  ret = pic_nil_value(pic);
  do {
    for (i = 0; i < argc; ++i) {
      if (! pic_pair_p(pic, args[i])) {
        break;
      }
      arg_list[i] = pic_car(pic, args[i]);
      args[i] = pic_cdr(pic, args[i]);
    }

    if (i != argc) {
      break;
    }
    pic_push(pic, pic_apply(pic, proc, i, arg_list), ret);
  } while (1);

  return pic_reverse(pic, ret);
}

static pic_value
pic_pair_for_each(pic_state *pic)
{
  int argc, i;
  pic_value proc, *args, *arg_list;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  arg_list = pic_alloca(pic, sizeof(pic_value) * argc);

  do {
    for (i = 0; i < argc; ++i) {
      if (! pic_pair_p(pic, args[i])) {
        break;
      }
      arg_list[i] = pic_car(pic, args[i]);
      args[i] = pic_cdr(pic, args[i]);
    }
    if (i != argc) {
      break;
    }
    pic_apply(pic, proc, i, arg_list);
  } while (1);

  return pic_undef_value(pic);
}

static pic_value
pic_pair_memq(pic_state *pic)
{
  pic_value key, list;

  pic_get_args(pic, "oo", &key, &list);

  while (! pic_nil_p(pic, list)) {
    if (pic_eq_p(pic, key, pic_car(pic, list))) {
      return list;
    }
    list = pic_cdr(pic, list);
  }
  return pic_false_value(pic);
}

static pic_value
pic_pair_memv(pic_state *pic)
{
  pic_value key, list;

  pic_get_args(pic, "oo", &key, &list);

  while (! pic_nil_p(pic, list)) {
    if (pic_eqv_p(pic, key, pic_car(pic, list))) {
      return list;
    }
    list = pic_cdr(pic, list);
  }
  return pic_false_value(pic);
}

static pic_value
pic_pair_member(pic_state *pic)
{
  pic_value key, list, proc;
  int n;

  n = pic_get_args(pic, "oo|l", &key, &list, &proc);

  while (! pic_nil_p(pic, list)) {
    if (n == 2) {
      if (pic_equal_p(pic, key, pic_car(pic, list)))
        return list;
    } else {
      if (! pic_false_p(pic, pic_call(pic, proc, 2, key, pic_car(pic, list))))
        return list;
    }
    list = pic_cdr(pic, list);
  }
  return pic_false_value(pic);
}

static pic_value
pic_pair_assq(pic_state *pic)
{
  pic_value key, alist, cell;

  pic_get_args(pic, "oo", &key, &alist);

  while (! pic_nil_p(pic, alist)) {
    cell = pic_car(pic, alist);
    if (pic_eq_p(pic, key, pic_car(pic, cell))) {
      return cell;
    }
    alist = pic_cdr(pic, alist);
  }
  return pic_false_value(pic);
}

static pic_value
pic_pair_assv(pic_state *pic)
{
  pic_value key, alist, cell;

  pic_get_args(pic, "oo", &key, &alist);

  while (! pic_nil_p(pic, alist)) {
    cell = pic_car(pic, alist);
    if (pic_eqv_p(pic, key, pic_car(pic, cell))) {
      return cell;
    }
    alist = pic_cdr(pic, alist);
  }
  return pic_false_value(pic);
}

static pic_value
pic_pair_assoc(pic_state *pic)
{
  pic_value key, alist, proc, cell;
  int n;

  n = pic_get_args(pic, "oo|l", &key, &alist, &proc);

  while (! pic_nil_p(pic, alist)) {
    cell = pic_car(pic, alist);
    if (n == 2) {
      if (pic_equal_p(pic, key, pic_car(pic, cell)))
        return cell;
    } else {
      if (! pic_false_p(pic, pic_call(pic, proc, 2, key, pic_car(pic, cell))))
        return cell;
    }
    alist = pic_cdr(pic, alist);
  }
  return pic_false_value(pic);
}

void
pic_init_pair(pic_state *pic)
{
  pic_defun(pic, "pair?", pic_pair_pair_p);
  pic_defun(pic, "cons", pic_pair_cons);
  pic_defun(pic, "car", pic_pair_car);
  pic_defun(pic, "cdr", pic_pair_cdr);
  pic_defun(pic, "null?", pic_pair_null_p);

  pic_defun(pic, "set-car!", pic_pair_set_car);
  pic_defun(pic, "set-cdr!", pic_pair_set_cdr);

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
