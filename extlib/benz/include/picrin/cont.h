/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_CONT_H
#define PICRIN_CONT_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_cont {
  PIC_JMPBUF jmp;

  int id;

  pic_checkpoint *cp;
  ptrdiff_t sp_offset;
  ptrdiff_t ci_offset;
  ptrdiff_t xp_offset;
  size_t arena_idx;
  pic_value ptable;
  pic_code *ip;

  int retc;
  pic_value *retv;

  struct pic_cont *prev;
};

void pic_save_point(pic_state *, struct pic_cont *);
void pic_load_point(pic_state *, struct pic_cont *);

pic_value pic_make_cont(pic_state *, struct pic_cont *);

void pic_wind(pic_state *, pic_checkpoint *, pic_checkpoint *);

#if defined(__cplusplus)
}
#endif

#endif
