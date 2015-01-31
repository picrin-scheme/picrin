/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_CONT_H
#define PICRIN_CONT_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_escape {
  bool valid;

  struct pic_winder *wind;

  ptrdiff_t sp_offset;
  ptrdiff_t ci_offset;
  ptrdiff_t xp_offset;
  size_t arena_idx;

  pic_code *ip;

  pic_value results;

  char jmp[1];
};

void pic_save_point(pic_state *, struct pic_escape *);
void pic_load_point(pic_state *, struct pic_escape *);

struct pic_proc *pic_make_econt(pic_state *, struct pic_escape *);

void pic_wind(pic_state *, struct pic_winder *, struct pic_winder *);
pic_value pic_dynamic_wind(pic_state *, struct pic_proc *, struct pic_proc *, struct pic_proc *);

pic_value pic_values0(pic_state *);
pic_value pic_values1(pic_state *, pic_value);
pic_value pic_values2(pic_state *, pic_value, pic_value);
pic_value pic_values3(pic_state *, pic_value, pic_value, pic_value);
pic_value pic_values4(pic_state *, pic_value, pic_value, pic_value, pic_value);
pic_value pic_values5(pic_state *, pic_value, pic_value, pic_value, pic_value, pic_value);
pic_value pic_values_by_array(pic_state *, size_t, pic_value *);
pic_value pic_values_by_list(pic_state *, pic_value);
size_t pic_receive(pic_state *, size_t, pic_value *);

pic_value pic_escape(pic_state *, struct pic_proc *);

#if defined(__cplusplus)
}
#endif

#endif
