/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_CONT_H
#define PICRIN_CONT_H

#if defined(__cplusplus)
extern "C" {
#endif

pic_value pic_values0(pic_state *);
pic_value pic_values1(pic_state *, pic_value);
pic_value pic_values2(pic_state *, pic_value, pic_value);
pic_value pic_values3(pic_state *, pic_value, pic_value, pic_value);
pic_value pic_values4(pic_state *, pic_value, pic_value, pic_value, pic_value);
pic_value pic_values5(pic_state *, pic_value, pic_value, pic_value, pic_value, pic_value);
pic_value pic_values_by_array(pic_state *, size_t, pic_value *);
pic_value pic_values_by_list(pic_state *, pic_value);
size_t pic_receive(pic_state *, size_t, pic_value *);

pic_value pic_callcc(pic_state *, struct pic_proc *);

#if defined(__cplusplus)
}
#endif

#endif
