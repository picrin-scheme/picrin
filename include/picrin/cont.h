/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_CONT_H
#define PICRIN_CONT_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_cont {
  PIC_OBJECT_HEADER
  jmp_buf jmp;

  struct pic_winder *wind;

  char *stk_pos, *stk_ptr;
  ptrdiff_t stk_len;

  pic_value *st_ptr;
  size_t sp_offset, st_len;

  pic_callinfo *ci_ptr;
  size_t ci_offset, ci_len;

  pic_code *ip;

  struct pic_object **arena;
  size_t arena_size;
  int arena_idx;

  struct pic_jmpbuf *try_jmps;
  size_t try_jmp_idx, try_jmp_size;

  pic_value results;
};

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
void pic_wind(pic_state *, struct pic_winder *, struct pic_winder *);

#if defined(__cplusplus)
}
#endif

#endif
