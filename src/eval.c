#include "picrin.h"

static pic_value
pic_assq(pic_state *pic, pic_value key, pic_value assoc)
{
  pic_value cell;

 enter:

  if (pic_nil_p(assoc))
    return assoc;

  cell = pic_car(pic, assoc);
  if (pic_eq_p(pic, key, pic_car(pic, cell)))
    return cell;

  assoc = pic_cdr(pic, assoc);
  goto enter;
}

static pic_value
pic_env_lookup(pic_state *pic, pic_value sym, struct pic_env *env)
{
  pic_value v;

 enter:

  v = pic_assq(pic, sym, env->assoc);
  if (! pic_nil_p(v)) {
    return pic_cdr(pic, v);
  }
  if (env->parent) {
    env = env->parent;
    goto enter;
  }

  return pic_nil_value();
}

static void
pic_env_define(pic_state *pic, pic_value sym, pic_value obj, struct pic_env *env)
{
  env->assoc = pic_cons(pic, pic_cons(pic, sym, obj), env->assoc);
}

pic_value
pic_eval(pic_state *pic, pic_value obj, struct pic_env *env)
{
  pic_value sDEFINE = pic_intern_cstr(pic, "define");
  pic_value sQUOTE = pic_intern_cstr(pic, "quote");

  while (1) {
    switch (pic_type(obj)) {
    case PIC_TT_SYMBOL: {
      return pic_env_lookup(pic, obj, env);
    }
    case PIC_TT_PAIR: {
      pic_value proc;

      proc = pic_car(pic, obj);
      if (pic_eq_p(pic, proc, sQUOTE)) {
	return pic_car(pic, pic_cdr(pic, obj));
      }
      else if (pic_eq_p(pic, proc, sDEFINE)) {
	pic_value sym, data;

	sym = pic_car(pic, pic_cdr(pic, obj));
	data = pic_car(pic, pic_cdr(pic, pic_cdr(pic, obj)));
	pic_env_define(pic, sym, pic_eval(pic, data, env), env);

	return pic_nil_value();
      }
      else {
	/* not implemented */
      }
    }
    case PIC_TT_INT:
    case PIC_TT_NIL: {
      return obj;
    }
    default:
      return pic_nil_value();
    }
  }
}
