/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_VALUE_H__
#define PICRIN_VALUE_H__

#if defined(__cplusplus)
extern "C" {
#endif

typedef int pic_sym;

/**
 * `undef` values never seen from user-end: that is,
 *  it's used only for repsenting internal special state
 */

enum pic_vtype {
  PIC_VTYPE_NIL = 1,
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

#if PIC_NAN_BOXING

/**
 * value representation by nan-boxing:
 *   float : FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
 *   ptr   : 111111111111TTTT PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP
 *   int   : 1111111111110110 0000000000000000 IIIIIIIIIIIIIIII IIIIIIIIIIIIIIII
 *   sym   : 1111111111110111 0000000000000000 SSSSSSSSSSSSSSSS SSSSSSSSSSSSSSSS
 *   char  : 1111111111111000 0000000000000000 CCCCCCCCCCCCCCCC ................
 */

typedef struct {
  union {
    void *data;
    double f;
    struct {
      union {
	int i;
	pic_sym sym;
	char c;
      };
      unsigned int type_;
    };
  } u;
} pic_value;

#define pic_ptr(v) ((void *)((long long)0xffffffffffff & (long long)(v).u.data))
#define pic_vtype(v) (((unsigned)0xfff00000 >= (v).u.type_) ? PIC_VTYPE_FLOAT : (((v).u.type_ & 0xf0000)>>16))
#define pic_init_value(v,vtype) (((v).u.type_ = ((unsigned int)0xfff00000|(unsigned int)((vtype)<<16))), (v).u.i = 0)

#else

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

#define pic_ptr(v) ((v).u.data)
#define pic_vtype(v) ((v).type)
#define pic_init_value(v,vtype) ((v).type = (vtype), (v).u.data = NULL)

#endif

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
  PIC_TT_ERROR,
  PIC_TT_ENV,
  PIC_TT_CONT,
  PIC_TT_SENV,
  PIC_TT_MACRO,
  PIC_TT_SC,
  PIC_TT_LIB,
  PIC_TT_VAR,
  PIC_TT_IREP
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

#define pic_float(v) ((v).u.f)
#define pic_int(v) ((v).u.i)
#define pic_sym(v) ((v).u.sym)
#define pic_char(v) ((v).u.c)

#define pic_obj_ptr(o) ((struct pic_object *)pic_ptr(o))
#define pic_pair_ptr(o) ((struct pic_pair *)pic_ptr(o))
#define pic_str_ptr(o) ((struct pic_string *)pic_ptr(o))
#define pic_vec_ptr(o) ((struct pic_vector *)pic_ptr(o))

#define pic_nil_p(v) (pic_vtype(v) == PIC_VTYPE_NIL)
#define pic_true_p(v) (pic_vtype(v) == PIC_VTYPE_TRUE)
#define pic_false_p(v) (pic_vtype(v) == PIC_VTYPE_FALSE)
#define pic_undef_p(v) (pic_vtype(v) == PIC_VTYPE_UNDEF)
#define pic_float_p(v) (pic_vtype(v) == PIC_VTYPE_FLOAT)
#define pic_int_p(v) (pic_vtype(v) == PIC_VTYPE_INT)
#define pic_sym_p(v) (pic_vtype(v) == PIC_VTYPE_SYMBOL)
#define pic_char_p(v) (pic_vtype(v) == PIC_VTYPE_CHAR)
#define pic_pair_p(v) (pic_type(v) == PIC_TT_PAIR)
#define pic_str_p(v) (pic_type(v) == PIC_TT_STRING)
#define pic_vec_p(v) (pic_type(v) == PIC_TT_VECTOR)

#define pic_test(v) (! pic_false_p(v))

static inline enum pic_tt pic_type(pic_value);
static inline const char *pic_type_repr(enum pic_tt);

static inline pic_value pic_nil_value();
static inline pic_value pic_true_value();
static inline pic_value pic_false_value();
static inline pic_value pic_bool_value(bool);
static inline pic_value pic_undef_value();
static inline pic_value pic_obj_value(void *);
static inline pic_value pic_float_value(double);
static inline pic_value pic_int_value(int);
static inline pic_value pic_symbol_value(pic_sym);
static inline pic_value pic_char_value(char c);
static inline pic_value pic_none_value();

static inline bool pic_eq_p(pic_value, pic_value);
static inline bool pic_eqv_p(pic_value, pic_value);

static inline enum pic_tt
pic_type(pic_value v)
{
  switch (pic_vtype(v)) {
  case PIC_VTYPE_NIL:
    return PIC_TT_NIL;
  case PIC_VTYPE_TRUE:
    return PIC_TT_BOOL;
  case PIC_VTYPE_FALSE:
    return PIC_TT_BOOL;
  case PIC_VTYPE_UNDEF:
    return PIC_TT_UNDEF;
  case PIC_VTYPE_FLOAT:
    return PIC_TT_FLOAT;
  case PIC_VTYPE_INT:
    return PIC_TT_INT;
  case PIC_VTYPE_SYMBOL:
    return PIC_TT_SYMBOL;
  case PIC_VTYPE_CHAR:
    return PIC_TT_CHAR;
  case PIC_VTYPE_EOF:
    return PIC_TT_EOF;
  case PIC_VTYPE_HEAP:
    return ((struct pic_object *)pic_ptr(v))->tt;
  default:
    return -1;                  /* logic flaw */
  }
}

static inline const char *
pic_type_repr(enum pic_tt tt)
{
  switch (tt) {
  case PIC_TT_NIL:
    return "nil";
  case PIC_TT_BOOL:
    return "boolean";
  case PIC_TT_FLOAT:
    return "float";
  case PIC_TT_INT:
    return "int";
  case PIC_TT_SYMBOL:
    return "symbol";
  case PIC_TT_CHAR:
    return "char";
  case PIC_TT_EOF:
    return "eof";
  case PIC_TT_UNDEF:
    return "undef";
  case PIC_TT_PAIR:
    return "pair";
  case PIC_TT_STRING:
    return "string";
  case PIC_TT_VECTOR:
    return "vector";
  case PIC_TT_BLOB:
    return "blob";
  case PIC_TT_PORT:
    return "port";
  case PIC_TT_ERROR:
    return "error";
  case PIC_TT_ENV:
    return "env";
  case PIC_TT_CONT:
    return "cont";
  case PIC_TT_PROC:
    return "proc";
  case PIC_TT_SC:
    return "sc";
  case PIC_TT_SENV:
    return "senv";
  case PIC_TT_MACRO:
    return "macro";
  case PIC_TT_LIB:
    return "lib";
  case PIC_TT_VAR:
    return "var";
  case PIC_TT_IREP:
    return "irep";
  }
  return 0;                     /* logic flaw */
}

static inline pic_value
pic_nil_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_NIL);
  return v;
}

static inline pic_value
pic_true_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_TRUE);
  return v;
}

static inline pic_value
pic_false_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_FALSE);
  return v;
}

static inline pic_value
pic_bool_value(bool b)
{
  pic_value v;

  pic_init_value(v, b ? PIC_VTYPE_TRUE : PIC_VTYPE_FALSE);
  return v;
}

#if PIC_NAN_BOXING

static inline pic_value
pic_obj_value(void *ptr)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_HEAP);
  v.u.data = (void*)((long long)v.u.data | ((long long)ptr));
  return v;
}

static inline pic_value
pic_float_value(double f)
{
  pic_value v;

  if (f != f) {
    v.u.type_ = 0x7ff80000;
    v.u.i = 0;
  } else {
    v.u.f = f;
  }
  return v;
}

#else

static inline pic_value
pic_obj_value(void *ptr)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_HEAP);
  v.u.data = ptr;
  return v;
}

static inline pic_value
pic_float_value(double f)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_FLOAT);
  v.u.f = f;
  return v;
}

#endif

static inline pic_value
pic_int_value(int i)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_INT);
  v.u.i = i;
  return v;
}

static inline pic_value
pic_symbol_value(pic_sym sym)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_SYMBOL);
  v.u.sym = sym;
  return v;
}

static inline pic_value
pic_char_value(char c)
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_CHAR);
  v.u.c = c;
  return v;
}

static inline pic_value
pic_undef_value()
{
  pic_value v;

  pic_init_value(v, PIC_VTYPE_UNDEF);
  return v;
}

static inline pic_value
pic_none_value()
{
#if PIC_NONE_IS_FALSE
  return pic_false_value();
#else
# error enable PIC_NONE_IS_FALSE
#endif
}

#if PIC_NAN_BOXING

static inline bool
pic_eq_p(pic_value x, pic_value y)
{
  return x.u.data == y.u.data;
}

static inline bool
pic_eqv_p(pic_value x, pic_value y)
{
  return x.u.data == y.u.data;
}

#else

static inline bool
pic_eq_p(pic_value x, pic_value y)
{
  if (pic_type(x) != pic_type(y))
    return false;

  switch (pic_type(x)) {
  case PIC_TT_NIL:
    return true;
  case PIC_TT_SYMBOL:
    return pic_sym(x) == pic_sym(y);
  default:
    return pic_ptr(x) == pic_ptr(y);
  }
}

static inline bool
pic_eqv_p(pic_value x, pic_value y)
{
  if (pic_type(x) != pic_type(y))
    return false;

  switch (pic_type(x)) {
  case PIC_TT_NIL:
    return true;
  case PIC_TT_SYMBOL:
    return pic_sym(x) == pic_sym(y);
  case PIC_TT_FLOAT:
    return pic_float(x) == pic_float(y);
  case PIC_TT_INT:
    return pic_int(x) == pic_int(y);
  default:
    return pic_ptr(x) == pic_ptr(y);
  }
}

#endif

#if defined(__cplusplus)
}
#endif

#endif
