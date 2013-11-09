#ifndef CONT_H__
#define CONT_H__

struct pic_cont {
  PIC_OBJECT_HEADER
  jmp_buf jmp;

  size_t stk_len;
  pic_value *stk_pos, *stk_ptr;

  pic_value *sp;
  pic_value *stbase, *stend;

  pic_callinfo *ci;
  pic_callinfo *cibase, *ciend;

  struct pic_object **arena;
  int arena_idx;

  pic_value result;
};

#endif
