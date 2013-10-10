#ifndef VALUE_H__
#define VALUE_H__

enum pic_vtype {
  PIC_VTYPE_HEAP
};

typedef struct {
  enum pic_vtype type;
  union {
    void *data;
  } u;
} pic_value;

enum pic_tt {
  PIC_TT_PAIR,
  PIC_TT_SYMBOL
};

#define PIC_OBJECT_HEADER			\
  enum pic_tt tt;

struct pic_object {
  PIC_OBJECT_HEADER
};

struct pic_pair {
  PIC_OBJECT_HEADER
  pic_value car;
  pic_value cdr;
};

struct pic_symbol {
  PIC_OBJECT_HEADER
  char *name;
};

pic_value pic_obj_value(struct pic_object *obj);

#endif
