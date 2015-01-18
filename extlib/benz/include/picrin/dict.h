/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_DICT_H
#define PICRIN_DICT_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_dict {
  PIC_OBJECT_HEADER
  xhash hash;
};

#define pic_dict_p(v) (pic_type(v) == PIC_TT_DICT)
#define pic_dict_ptr(v) ((struct pic_dict *)pic_ptr(v))

struct pic_dict *pic_make_dict(pic_state *);

#define pic_dict_for_each(sym, dict)                    \
  pic_dict_for_each_helper_((sym), PIC_GENSYM(tmp), (dict))
#define pic_dict_for_each_helper_(var, tmp, dict)                       \
  for (xh_entry *tmp = xh_begin(&dict->hash);                           \
       (tmp && ((var = xh_key(tmp, pic_sym)), 1));                      \
       tmp = xh_next(tmp))

pic_value pic_dict_ref(pic_state *, struct pic_dict *, pic_sym);
void pic_dict_set(pic_state *, struct pic_dict *, pic_sym, pic_value);
void pic_dict_del(pic_state *, struct pic_dict *, pic_sym);
size_t pic_dict_size(pic_state *, struct pic_dict *);
bool pic_dict_has(pic_state *, struct pic_dict *, pic_sym);

#if defined(__cplusplus)
}
#endif

#endif
