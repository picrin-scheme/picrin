/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_PAIR_H__
#define PICRIN_PAIR_H__

#if defined(__cplusplus)
extern "C" {
#endif

pic_value pic_cons(pic_state *, pic_value, pic_value);
pic_value pic_car(pic_state *, pic_value);
pic_value pic_cdr(pic_state *, pic_value);

bool pic_list_p(pic_state *, pic_value);
pic_value pic_list(pic_state *, size_t, ...);
pic_value pic_list_from_array(pic_state *, size_t, pic_value *);
pic_value pic_make_list(pic_state *, int, pic_value);

#define PIC_FOR_EACH_HELPER(var, tmp, list)                             \
  for (pic_value tmp = (list);                                          \
       pic_nil_p(tmp) ? false : ((var = pic_car(pic, tmp)), true);      \
       tmp = pic_cdr(pic, tmp))

#define pic_for_each(var, list)                                      \
  PIC_FOR_EACH_HELPER(var, GENSYM(pic_for_each_tmp__), list)         \

int pic_length(pic_state *, pic_value);
pic_value pic_reverse(pic_state *, pic_value);
pic_value pic_append(pic_state *, pic_value, pic_value);

pic_value pic_assq(pic_state *, pic_value key, pic_value assoc);
pic_value pic_assoc(pic_state *, pic_value key, pic_value assoc);
pic_value pic_acons(pic_state *, pic_value key, pic_value val, pic_value assoc);

pic_value pic_caar(pic_state *, pic_value);
pic_value pic_cadr(pic_state *, pic_value);
pic_value pic_cdar(pic_state *, pic_value);
pic_value pic_cddr(pic_state *, pic_value);

pic_value pic_list_tail(pic_state *, pic_value ,int);
pic_value pic_list_ref(pic_state *, pic_value, int);
void pic_list_set(pic_state *, pic_value, int, pic_value);
pic_value pic_list_copy(pic_state *, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
