/**
 * See Copyright Notice in picrin.h
 */

#ifndef CONT_H__
#define CONT_H__

struct pic_cont {
  PIC_OBJECT_HEADER
  jmp_buf jmp;

  struct pic_block *blk;

  size_t stk_len;
  pic_value *stk_pos, *stk_ptr;

  pic_value *st_ptr;
  size_t sp_offset, st_len;

  pic_callinfo *ci_ptr;
  size_t ci_offset, ci_len;

  struct pic_proc **rescue;
  size_t ridx, rlen;

  struct pic_object *arena[PIC_ARENA_SIZE];
  int arena_idx;

  pic_value result;
};

#define PIC_BLK_INCREF(pic,blk) do {		\
    (blk)->refcnt++;				\
  } while (0)

#define PIC_BLK_DECREF(pic,blk) do {			\
    struct pic_block *_a = (blk), *_b;			\
    while (_a) {					\
      if (! --_a->refcnt) {				\
	_b = _a->prev;					\
	pic_free((pic), _a);				\
	_a = _b;					\
      } else {						\
	break;						\
      }							\
    }							\
  } while (0)

#endif
