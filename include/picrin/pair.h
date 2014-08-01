/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_PAIR_H__
#define PICRIN_PAIR_H__

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_pair {
  PIC_OBJECT_HEADER
  pic_value car;
  pic_value cdr;
};

#define pic_pair_p(v) (pic_type(v) == PIC_TT_PAIR)
#define pic_pair_ptr(o) ((struct pic_pair *)pic_ptr(o))

pic_value pic_cons(pic_state *, pic_value, pic_value);
pic_value pic_car(pic_state *, pic_value);
pic_value pic_cdr(pic_state *, pic_value);
void pic_set_car(pic_state *, pic_value, pic_value);
void pic_set_cdr(pic_state *, pic_value, pic_value);

bool pic_list_p(pic_value);
pic_value pic_list1(pic_state *, pic_value);
pic_value pic_list2(pic_state *, pic_value, pic_value);
pic_value pic_list3(pic_state *, pic_value, pic_value, pic_value);
pic_value pic_list4(pic_state *, pic_value, pic_value, pic_value, pic_value);
pic_value pic_list5(pic_state *, pic_value, pic_value, pic_value, pic_value, pic_value);
pic_value pic_list6(pic_state *, pic_value, pic_value, pic_value, pic_value, pic_value, pic_value);
pic_value pic_list7(pic_state *, pic_value, pic_value, pic_value, pic_value, pic_value, pic_value, pic_value);
pic_value pic_list_by_array(pic_state *, size_t, pic_value *);
pic_value pic_make_list(pic_state *, int, pic_value);

#define pic_for_each(var, list)                                 \
  pic_for_each_helper__(var, GENSYM(tmp), list)
#define pic_for_each_helper__(var, tmp, list)                           \
  for (pic_value tmp = (list);                                          \
       pic_null_p(tmp) ? false : ((var = pic_car(pic, tmp)), true);     \
       tmp = pic_cdr(pic, tmp))

#define pic_push(pic, item, place) (place = pic_cons(pic, item, place))
#define pic_pop(pic, place) (place = pic_cdr(pic, place))

int pic_length(pic_state *, pic_value);
pic_value pic_reverse(pic_state *, pic_value);
pic_value pic_append(pic_state *, pic_value, pic_value);

pic_value pic_memq(pic_state *, pic_value key, pic_value list);
pic_value pic_memv(pic_state *, pic_value key, pic_value list);
pic_value pic_member(pic_state *, pic_value key, pic_value list, struct pic_proc * /* = NULL */);

pic_value pic_assq(pic_state *, pic_value key, pic_value assoc);
pic_value pic_assv(pic_state *, pic_value key, pic_value assoc);
pic_value pic_assoc(pic_state *, pic_value key, pic_value assoc, struct pic_proc * /* = NULL */);

pic_value pic_acons(pic_state *, pic_value key, pic_value val, pic_value assoc);

pic_value pic_caar(pic_state *, pic_value);
pic_value pic_cadr(pic_state *, pic_value);
pic_value pic_cdar(pic_state *, pic_value);
pic_value pic_cddr(pic_state *, pic_value);

pic_value pic_list_tail(pic_state *, pic_value, int);
pic_value pic_list_ref(pic_state *, pic_value, int);
void pic_list_set(pic_state *, pic_value, int, pic_value);
pic_value pic_list_copy(pic_state *, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
