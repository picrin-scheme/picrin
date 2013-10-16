#ifndef VALUE_H__
#define VALUE_H__

enum pic_vtype {
  PIC_VTYPE_NIL,
  PIC_VTYPE_TRUE,
  PIC_VTYPE_FALSE,
  PIC_VTYPE_UNDEF,
  PIC_VTYPE_FLOAT,
  PIC_VTYPE_HEAP
};

typedef struct {
  enum pic_vtype type;
  union {
    void *data;
    double f;
  } u;
} pic_value;

enum pic_tt {
  /* immediate */
  PIC_TT_NIL,
  PIC_TT_BOOL,
  PIC_TT_FLOAT,
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
pic_value pic_true_value();
pic_value pic_false_value();
pic_value pic_bool_value(bool);
pic_value pic_undef_value();
pic_value pic_obj_value(void *);
pic_value pic_float_value(double);

#define pic_float(v) ((v).u.f)

#define pic_nil_p(v) ((v).type == PIC_VTYPE_NIL)
#define pic_true_p(v) ((v).type == PIC_VTYPE_TRUE)
#define pic_false_p(v) ((v).type == PIC_VTYPE_FALSE)

#endif
