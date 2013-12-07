#ifndef PAIR_H__
#define PAIR_H__

pic_value pic_cons(pic_state *, pic_value, pic_value);
pic_value pic_car(pic_state *, pic_value);
pic_value pic_cdr(pic_state *, pic_value);

bool pic_list_p(pic_state *, pic_value);
pic_value pic_list(pic_state *, size_t, ...);
pic_value pic_list_from_array(pic_state *, size_t, pic_value *);

int pic_length(pic_state *, pic_value);
pic_value pic_reverse(pic_state *, pic_value);

pic_value pic_assq(pic_state *, pic_value key, pic_value assoc);
pic_value pic_assoc(pic_state *, pic_value key, pic_value assoc);
pic_value pic_acons(pic_state *, pic_value key, pic_value val, pic_value assoc);

pic_value pic_caar(pic_state *, pic_value);
pic_value pic_cadr(pic_state *, pic_value);
pic_value pic_cdar(pic_state *, pic_value);
pic_value pic_cddr(pic_state *, pic_value);

#endif
