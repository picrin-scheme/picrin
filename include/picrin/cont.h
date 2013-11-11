#ifndef CONT_H__
#define CONT_H__

struct pic_cont {
  PIC_OBJECT_HEADER
  jmp_buf jmp;

  size_t stk_len;
  pic_value *stk_pos, *stk_ptr;

  pic_value *sp, *st_ptr;
  size_t st_len;

  pic_callinfo *ci, *ci_ptr;
  size_t ci_len;

  struct pic_object **arena;
  int arena_idx;

  pic_value result;
};

#endif
