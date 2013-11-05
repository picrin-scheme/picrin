#ifndef VALUE_H__
#define VALUE_H__

typedef int pic_sym;

enum pic_vtype {
  PIC_VTYPE_NIL,
  PIC_VTYPE_TRUE,
  PIC_VTYPE_FALSE,
  PIC_VTYPE_UNDEF,
  PIC_VTYPE_FLOAT,
  PIC_VTYPE_INT,
  PIC_VTYPE_SYMBOL,
  PIC_VTYPE_CHAR,
  PIC_VTYPE_EOF,
  PIC_VTYPE_HEAP
};

typedef struct {
  enum pic_vtype type;
  union {
    void *data;
    double f;
    int i;
    pic_sym sym;
    char c;
  } u;
} pic_value;

enum pic_tt {
  /* immediate */
  PIC_TT_NIL,
  PIC_TT_BOOL,
  PIC_TT_FLOAT,
  PIC_TT_INT,
  PIC_TT_SYMBOL,
  PIC_TT_CHAR,
  PIC_TT_EOF,
  PIC_TT_UNDEF,
  /* heap */
  PIC_TT_PAIR,
  PIC_TT_STRING,
  PIC_TT_VECTOR,
  PIC_TT_BLOB,
  PIC_TT_PROC,
  PIC_TT_PORT,
  PIC_TT_ENV
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

struct pic_string {
  PIC_OBJECT_HEADER
  char *str;
  size_t len;
};

struct pic_vector {
  PIC_OBJECT_HEADER
  pic_value *data;
  size_t len;
};

struct pic_proc;
struct pic_port;
struct pic_blob;

#define pic_obj_ptr(o) ((struct pic_object *)(o).u.data)
#define pic_pair_ptr(o) ((struct pic_pair *)(o).u.data)
#define pic_str_ptr(o) ((struct pic_string *)(o).u.data)
#define pic_vec_ptr(o) ((struct pic_vector *)(o).u.data)

enum pic_tt pic_type(pic_value);
const char *pic_type_repr(enum pic_tt);

pic_value pic_nil_value();
pic_value pic_true_value();
pic_value pic_false_value();
pic_value pic_bool_value(bool);
pic_value pic_undef_value();
pic_value pic_obj_value(void *);
pic_value pic_float_value(double);
pic_value pic_int_value(int);
pic_value pic_symbol_value(pic_sym);
pic_value pic_char_value(char c);

#define pic_float(v) ((v).u.f)
#define pic_int(v) ((v).u.i)
#define pic_sym(v) ((v).u.sym)
#define pic_char(v) ((v).u.c)

#define pic_nil_p(v) ((v).type == PIC_VTYPE_NIL)
#define pic_true_p(v) ((v).type == PIC_VTYPE_TRUE)
#define pic_false_p(v) ((v).type == PIC_VTYPE_FALSE)
#define pic_undef_p(v) ((v).type == PIC_VTYPE_UNDEF)
#define pic_float_p(v) ((v).type == PIC_VTYPE_FLOAT)
#define pic_int_p(v) ((v).type == PIC_VTYPE_INT)
#define pic_symbol_p(v) ((v).type == PIC_VTYPE_SYMBOL)
#define pic_char_p(v) ((v).type == PIC_VTYPE_CHAR)
#define pic_pair_p(v) (pic_type(v) == PIC_TT_PAIR)
#define pic_str_p(v) (pic_type(v) == PIC_TT_STRING)
#define pic_vec_p(v) (pic_type(v) == PIC_TT_VECTOR)

#endif
