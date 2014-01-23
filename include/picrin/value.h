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
  PIC_TT_SYNTAX,
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

#define pic_obj_ptr(o) ((struct pic_object *)pic_ptr(o))
#define pic_pair_ptr(o) ((struct pic_pair *)pic_ptr(o))
#define pic_str_ptr(o) ((struct pic_string *)pic_ptr(o))
#define pic_vec_ptr(o) ((struct pic_vector *)pic_ptr(o))

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
pic_value pic_none_value();

#define pic_float(v) ((v).u.f)
#define pic_int(v) ((v).u.i)
#define pic_sym(v) ((v).u.sym)
#define pic_char(v) ((v).u.c)

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

/* obsoleted macro(s) */
#define pic_symbol_p pic_sym_p

bool pic_eq_p(pic_value, pic_value);
bool pic_eqv_p(pic_value, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
