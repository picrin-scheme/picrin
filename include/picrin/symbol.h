#ifndef SYMBOL_H__
#define SYMBOL_H__

struct sym_tbl {
  pic_value tbl[PIC_SYM_TBL_SIZE];
  size_t size;
};

struct sym_tbl * sym_tbl_new();
pic_value sym_tbl_get(struct sym_tbl *, const char *);

#endif
