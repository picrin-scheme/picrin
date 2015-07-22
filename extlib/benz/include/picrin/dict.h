/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_DICT_H
#define PICRIN_DICT_H

#if defined(__cplusplus)
extern "C" {
#endif

KHASH_DECLARE(dict, pic_sym *, pic_value)

struct pic_dict {
  PIC_OBJECT_HEADER
  khash_t(dict) hash;
};

#define pic_dict_p(v) (pic_type(v) == PIC_TT_DICT)
#define pic_dict_ptr(v) ((struct pic_dict *)pic_ptr(v))

struct pic_dict *pic_make_dict(pic_state *);

#define pic_dict_for_each(sym, dict, it)        \
  pic_dict_for_each_help(sym, (&(dict)->hash), it)
#define pic_dict_for_each_help(sym, h, it)        \
  for (it = kh_begin(h); it != kh_end(h); ++it)   \
    if ((sym = kh_key(h, it)), kh_exist(h, it))

pic_value pic_dict_ref(pic_state *, struct pic_dict *, pic_sym *);
void pic_dict_set(pic_state *, struct pic_dict *, pic_sym *, pic_value);
void pic_dict_del(pic_state *, struct pic_dict *, pic_sym *);
size_t pic_dict_size(pic_state *, struct pic_dict *);
bool pic_dict_has(pic_state *, struct pic_dict *, pic_sym *);

#if defined(__cplusplus)
}
#endif

#endif
