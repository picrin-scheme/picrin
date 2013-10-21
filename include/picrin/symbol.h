#ifndef SYMBOL_H__
#define SYMBOL_H__

struct sym_tbl {
  pic_value tbl[PIC_SYM_TBL_SIZE];
  size_t size;
};

pic_value sym_tbl_get(struct sym_tbl *, const char *);

#endif
