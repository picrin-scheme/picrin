#ifndef PAIR_H__
#define PAIR_H__

pic_value pic_cons(pic_state *, pic_value, pic_value);
pic_value pic_car(pic_state *, pic_value);
pic_value pic_cdr(pic_state *, pic_value);

pic_value pic_list(pic_state *, size_t, ...);

pic_value pic_assq(pic_state *, pic_value key, pic_value assoc);

#endif
