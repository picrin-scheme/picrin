#ifndef VALUE_H__
#define VALUE_H__

enum pic_vtype {
  PIC_VTYPE_NIL,
  PIC_VTYPE_INT,
  PIC_VTYPE_UNDEF,
  PIC_VTYPE_HEAP
};

typedef struct {
  enum pic_vtype type;
  union {
    void *data;
    int i;
  } u;
} pic_value;

enum pic_tt {
  /* immediate */
  PIC_TT_NIL,
  PIC_TT_INT,
  PIC_TT_UNDEF,
  /* heap */
  PIC_TT_PAIR,
  PIC_TT_SYMBOL,
  PIC_TT_PROC
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

struct pic_proc;

#define pic_object_ptr(o) ((struct pic_object *)o.u.data)
#define pic_pair_ptr(o) ((struct pic_pair *)o.u.data)
#define pic_symbol_ptr(o) ((struct pic_symbol *)o.u.data)

enum pic_tt pic_type(pic_value);

pic_value pic_nil_value();
pic_value pic_undef_value();
pic_value pic_obj_value(void *);
pic_value pic_int_value(int);

#define pic_int(v) ((v).u.i)

#define pic_nil_p(v) (pic_type(v) == PIC_TT_NIL)

#endif
