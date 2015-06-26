/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

enum pic_pickle_types {
  PIC_PT_POSITIVE_FIXINT, 	/* 0x00 - 0x7f */
  PIC_PT_NEGATIVE_FIXINT,	/* 0xe0 - 0xff */
  PIC_PT_INT8 = 0xd0u,
  PIC_PT_INT16,
  PIC_PT_INT32,
  PIC_PT_INT64,

  PIC_PT_FLOAT32 = 0xcau,
  PIC_PT_FLOAT64,

  PIC_PT_FIXSTR,		/* 0xa0 - 0xbf */
  PIC_PT_STR8 = 0xd9u,
  PIC_PT_STR16,
  PIC_PT_STR32,

  PIC_PT_SYMBOL8 = 0xccu,
  PIC_PT_SYMBOL16,
  PIC_PT_SYMBOL32,
  PIC_PT_SYMBOL64,

  PIC_PT_FIXLIST,		/* 0x80 - 0x8f */
  PIC_PT_LIST16 = 0xdeu,
  PIC_PT_LIST32,

  PIC_PT_NIL = 0xc0u,

  PIC_PT_FALSE = 0xc2u,
  PIC_PT_TRUE = 0xc3u,

  PIC_PT_BYTEVECTOR8 = 0xc4u,
  PIC_PT_BYTEVECTOR16,
  PIC_PT_BYTEVECTOR32,

  PIC_PT_FIXVECTOR,		/* 0x90 - 0x9f */
  PIC_PT_VECTOR16 = 0xdcu,
  PIC_PT_VECTOR32,

  PIC_PT_CHAR = 0xc1u,

  PIC_PT_EXT8 = 0xc7u,
  PIC_PT_EXT16,
  PIC_PT_EXT32,

  PIC_PT_FIXEXT1 = 0xd4,
  PIC_PT_FIXEXT2,
  PIC_PT_FIXEXT4,
  PIC_PT_FIXEXT8,
  PIC_PT_FIXEXT16,
};

#define OB_CHUNK_SIZE 128

struct octet_buffer {
  uint8_t *data;
  size_t size, pointer;
};

static void
ob_init(struct octet_buffer *ob)
{
  ob->data = NULL;
  ob->size = 0;
  ob->pointer = 0;
}

static void
ob_destroy(struct octet_buffer *ob)
{
  free(ob->data);
}

static void
ob_enlarge(struct octet_buffer *ob)
{
  ob->data = (uint8_t *)realloc(ob->data, ob->size + OB_CHUNK_SIZE);
  ob->size += OB_CHUNK_SIZE;
}

static void
ob_pushn(struct octet_buffer *ob, const uint8_t *octets, size_t n)
{
  while (ob->pointer + n > ob->size) {
    ob_enlarge(ob);
  }

  memcpy(ob->data + ob->pointer, octets, n);
  ob->pointer += n;
}

#if __BIG_ENDIAN__
#define DEFINE_OB_PUSHER(type)                                  \
  static void							\
  ob_push_##type(struct octet_buffer *ob, type octets)          \
  {                                                             \
    while (ob->pointer + sizeof(type) > ob->size) {             \
      ob_enlarge(ob);                                           \
    }                                                           \
                                                                \
    memcpy(ob->data + ob->pointer, &octets, sizeof(type));      \
    ob->pointer += sizeof(type);                                \
}
#else
#define DEFINE_OB_PUSHER(type)                                  \
  static void							\
  ob_push_##type(struct octet_buffer *ob, type octets)          \
  {                                                             \
    while (ob->pointer + sizeof(type) > ob->size) {             \
      ob_enlarge(ob);                                           \
    }                                                           \
                                                                \
    ob->pointer += sizeof(type);                                \
    for(size_t i = 0; i < sizeof(type); i++){                   \
      ob->data[--ob->pointer] =					\
	(uint8_t) (octets >> (8 * i)) & 0xff;			\
    }                                                           \
    ob->pointer += sizeof(type);                                \
}
#endif

DEFINE_OB_PUSHER(uint8_t)
DEFINE_OB_PUSHER(uint16_t)
DEFINE_OB_PUSHER(uint32_t)
DEFINE_OB_PUSHER(uint64_t)

#define ob_push ob_push_uint8_t

static void
ob_as_reader(struct octet_buffer *ob)
{
  /* after this function, don't call ob_push* functions */
  ob->pointer = 0;
}

static uint8_t
*ob_readn(struct octet_buffer *ob, size_t len)
{
  assert(ob->size >= ob->pointer + len);
  uint8_t *res;

  res = ob->data + ob->pointer;
  ob->pointer += len;

  return res;
}

#if __BIG_ENDIAN__
#define DEFINE_OB_READER(type)                          \
  static type						\
  ob_read_##type(struct octet_buffer *ob)               \
  {                                                     \
    assert(ob->size >= ob->pointer + sizeof(type));     \
                                                        \
    type i;                                             \
                                                        \
    i = *((type *)(ob->data + ob->pointer));            \
    ob->pointer += sizeof(type);                        \
                                                        \
    return i;                                           \
  }
#else
#define DEFINE_OB_READER(type)						\
  static type								\
  ob_read_##type(struct octet_buffer *ob)				\
  {									\
    assert(ob->size >= ob->pointer + sizeof(type));			\
									\
    union { type u;unsigned char bytes[sizeof(type)];} res;		\
    									\
    for(size_t i = 0; i < sizeof(type); i++){				\
      res.bytes[i] =							\
	*(uint8_t *)(ob->data + ob->pointer + sizeof(type) - 1 - i);	\
    }									\
    ob->pointer += sizeof(type);					\
									\
    return res.u;							\
  }
#endif

DEFINE_OB_READER(uint8_t)
DEFINE_OB_READER(uint16_t)
DEFINE_OB_READER(uint32_t)
DEFINE_OB_READER(uint64_t)
DEFINE_OB_READER(int8_t)
DEFINE_OB_READER(int16_t)
DEFINE_OB_READER(int32_t)
DEFINE_OB_READER(int64_t)

#define ob_read ob_read_uint8_t

#define ob_push_16len(ob, len, base)            \
  if (len < ((uint64_t) 1) << 16) {             \
    ob_push(ob,  base);                         \
    ob_push_uint16_t(ob, (uint16_t) len);       \
  }                                             \
  else if (len < ((uint64_t) 1) << 32) {        \
    ob_push(ob,  base + 1);                     \
    ob_push_uint32_t(ob, (uint32_t) len);       \
  }

#define ob_push_len(ob, len, base)              \
  if (len < ((uint64_t) 1) << 8) {              \
    ob_push(ob, base);                          \
    ob_push(ob, (uint8_t) len);                 \
  }                                             \
  else ob_push_16len(ob, len, base + 1)


#define ob_push_len64(ob, len, base)                   \
  ob_push_len(ob, len, base)                           \
  else {                                               \
    ob_push(ob,  base + 3);                            \
    ob_push_uint64_t(ob, (uint64_t) len);              \
  }

static void pickle(pic_state *, struct octet_buffer *, pic_value);

static void
pickle_int(pic_state *pic, struct octet_buffer *ob, int i)
{
  if ( 0x00 <= i && i <= 0x7f) {
    /* positive fixint 	0xxxxxxx 	0x00 - 0x7f */
    ob_push(ob, (uint8_t) i);
  }
  else if (0xff <= i && i <= 0xe0) {
    /* negative fixint 	111xxxxx 	0xe0 - 0xff */
    ob_push(ob, (uint8_t) 0xe0 | i);
  }
  else if ( -1 << 7 <= i && i <= (int) ((1u << 7) - 1)) {
    /* int 8 	11010000 	0xd0 */
    ob_push(ob, PIC_PT_INT8);
    ob_push_uint8_t(ob, (uint8_t) i);
  }
  else if ( -1 << 15 <= i && i <= (int) ((1u << 15) - 1)) {
    /* int 16 	11010001 	0xd1 */
    ob_push(ob, PIC_PT_INT16);
    ob_push_uint16_t(ob, (uint16_t) i);
  }
  else if ( -1 << 31 <= i && i <= (int) ((1u << 31) - 1)) {
    /* int 32 	11010010 	0xd2 */
    ob_push(ob, PIC_PT_INT32);
    ob_push_uint32_t(ob, (uint32_t) i);
  }
  else {
    /* int 64 	11010011 	0xd3 */
    ob_push(ob, PIC_PT_INT64);
    ob_push_uint64_t(ob, (uint64_t) i);
  }

}

static void
pickle_double(pic_state *pic, struct octet_buffer *ob, double d)
{
  union { uint64_t u; double d;} t;

  /* float 32 	11001010 	0xca */
  /* float 64 	11001011 	0xcb */
  /* picrin has only double */
  t.d = d;

  ob_push(ob, 0xcb);
  ob_push_uint64_t(ob, t.u);
}

static void
pickle_string(pic_state *pic, struct octet_buffer *ob, pic_str *str)
{
  const char *cstr;
  size_t len;

  cstr = pic_str_cstr(pic, str);
  len = pic_str_len(str);

  if (len < 1<<5)
    /* fixstr 	101xxxxx 	0xa0 - 0xbf */
    ob_push(ob, 0xa0 | len);
  else
    /* str 8 	11011001 	0xd9 */
    /* str 16 	11011010 	0xda */
    /* str 32 	11011011 	0xdb */
    ob_push_len(ob, len, PIC_PT_STR8);

  ob_pushn(ob, (const uint8_t *)cstr, len);
}

static void
pickle_symbol(pic_state *pic, struct octet_buffer *ob, pic_sym *sym)
{
  /* symbol 8 	11001100 	0xcc */
  /* symbol 16 	11001101 	0xcd */
  /* symbol 32 	11001110 	0xce */
  /* symbol 64 	11001111 	0xcf */
  ob_push_len64(ob, (uint64_t) sym, (uint8_t) PIC_PT_SYMBOL8);
}

static void
pickle_list(pic_state *pic, struct octet_buffer *ob, pic_value list)
{
  pic_value v, it;
  size_t len;

  len = pic_length(pic, list);
  if (len < 1 << 4)
    /* fixlist 	1000xxxx 	0x80 - 0x8f */
    ob_push(ob, 0x80 | len);
  else
    /* list 16 	11011110 	0xde */
    /* list 32 	11011111 	0xdf */
    ob_push_16len(ob, len, (uint8_t) PIC_PT_LIST16);

  pic_for_each (v, list, it) {
    pickle(pic, ob, v);
  }
}

static void
pickle_nil(pic_state *pic, struct octet_buffer *ob)
{
  ob_push(ob, (uint8_t) PIC_PT_NIL);
}

static void
pickle_bool(pic_state *pic, struct octet_buffer *ob, pic_value b)
{
  /* true 	11000011 	0xc3 */
  /* false 	11000010 	0xc2 */
  ob_push(ob,  pic_true_p(b) ? PIC_PT_TRUE : PIC_PT_FALSE);
}

static void
pickle_blob(pic_state *pic, struct octet_buffer *ob, pic_blob *blob)
{
  size_t len;

  len = blob->len;
  /* bytevector 8 	11000100 	0xc4 */
  /* bytevector 16 	11000101 	0xc5 */
  /* bytevector 32 	11000110 	0xc6 */
  ob_push_len(ob, len, PIC_PT_BYTEVECTOR8);

  ob_pushn(ob, (uint8_t *) blob->data, len);
}

static void
pickle_vector(pic_state *pic, struct octet_buffer *ob, struct pic_vector *vec)
{
  size_t len, i;

  len = vec->len;

  if (len < 1 << 4 )
    /* fixvector 	1001xxxx 	0x90 - 0x9f */
    ob_push(ob, (uint8_t) 0x90 | len);
  else
    /* vector 16 	11011100 	0xdc */
    /* vector 32 	11011101 	0xdd */
    ob_push_16len(ob, len, (uint8_t) PIC_PT_VECTOR16);

  for (i = 0; i < len; i++) {
    pickle(pic, ob, *(vec->data + i));
  }
}

static void
pickle_char(pic_state *pic, struct octet_buffer *ob, pic_value obj)
{
  /* char 	11000001 	0xc1 */
  /* current picrin supports only ascii chars */
  ob_push(ob, (uint8_t) PIC_PT_CHAR);
  ob_push(ob, (uint8_t) pic_char(obj));
}

static void
pickle(pic_state *pic, struct octet_buffer *ob, pic_value obj)
{

  switch(pic_type(obj)){
  case PIC_TT_INT:
    pickle_int(pic, ob, pic_int(obj));
    break;
  case PIC_TT_FLOAT:
    pickle_double(pic, ob, pic_float(obj));
    break;
  case PIC_TT_STRING:
    pickle_string(pic, ob, pic_str_ptr(obj));
    break;
  case PIC_TT_SYMBOL:
    pickle_symbol(pic, ob,  pic_sym_ptr(obj));
    break;
  case PIC_TT_PAIR:
    pickle_list(pic, ob, obj);
    break;
  case PIC_TT_BOOL:
    pickle_bool(pic, ob, obj);
    break;
  case PIC_TT_BLOB:
    pickle_blob(pic, ob, pic_blob_ptr(obj));
    break;
  case PIC_TT_VECTOR:
    pickle_vector(pic, ob, pic_vec_ptr(obj));
    break;
  case PIC_TT_NIL:
    pickle_nil(pic, ob);
    break;
  case PIC_TT_CHAR:
    pickle_char(pic, ob, obj);
    break;
  default:
    /* (never used) 	11000001 	0xc1 */
    /* ext 8 	11000111 	0xc7 */
    /* ext 16 	11001000 	0xc8 */
    /* ext 32 	11001001 	0xc9 */
    /* fixext 1 	11010100 	0xd4 */
    /* fixext 2 	11010101 	0xd5 */
    /* fixext 4 	11010110 	0xd6 */
    /* fixext 8 	11010111 	0xd7 */
    /* fixext 16 	11011000 	0xd8 */
    pic_errorf(pic, "cannot pickle ~a", obj);
  }
}

static pic_value
unpickle(pic_state *pic, struct octet_buffer *ob);

static pic_value
unpickle_list(pic_state *pic, struct octet_buffer *ob, size_t len)
{
  pic_value result;

  result = pic_nil_value();

  while(len --> 0)
    result = pic_cons(pic, unpickle(pic, ob), result);

  return pic_reverse(pic, result);
}

static pic_value
unpickle_vector(pic_state *pic, struct octet_buffer *ob, size_t len)
{
  struct pic_vector *vec;

  vec = pic_make_vec(pic, len);

  for (size_t i = 0; i < len; i++) {
    vec->data[i] = unpickle(pic, ob);
  }

  return pic_obj_value(vec);
}

static pic_value
unpickle_blob(pic_state *pic, struct octet_buffer *ob, size_t len)
{
  struct pic_blob *blob = pic_make_blob(pic, len);

  memcpy(blob->data, ob_readn(ob, len), len);

  return pic_obj_value(blob);
}

static pic_value
unpickle_string(pic_state *pic, struct octet_buffer *ob, size_t len)
{
  return pic_obj_value(pic_make_str(pic, (const char *)ob_readn(ob, len), len));
}

static pic_value
unpickle_float(pic_state *pic, struct octet_buffer *ob, size_t len)
{
  union { uint32_t u; float f;} t32;
  union { uint64_t u; double d;} t64;

  if (len == 32) {
    t32.u = ob_read_uint32_t(ob);
    return pic_float_value((double) t32.f);
  }
  else{
    t64.u = ob_read_uint64_t(ob);
    return pic_float_value(t64.d);
  }
}

static pic_value
unpickle_symbol(pic_state *pic, struct octet_buffer *ob, size_t sym)
{
  /* :FIXME: */

  return pic_undef_value();
}

static pic_value
unpickle_char(pic_state *pic, struct octet_buffer *ob)
{
  /* :TODO: current picrin supports only ascii chars */
  return pic_char_value(ob_read(ob));
}

static pic_value
unpickle_ext(pic_state *pic, struct octet_buffer *ob, size_t len)
{
  ob_readn(ob, len);
  /* :FIXME:  */

  pic_errorf(pic, "cannot unpickle ext");

  return pic_undef_value();
}

static pic_value
unpickle(pic_state *pic, struct octet_buffer *ob)
{
  uint8_t octet;
  octet = ob_read(ob);

  if (octet >= 0xe0u)
    /* negative fixint 	111xxxxx 	0xe0 - 0xff */
    return pic_int_value((int) (char) octet);
  else if (octet <= 0x7fu)
    /* positive fixint 	0xxxxxxx 	0x00 - 0x7f */
    return pic_int_value((int) (char) octet);
  else if (octet <= 0x8fu)
    /* fixlist 	1000xxxx 	0x80 - 0x8f */
    return unpickle_list(pic, ob, (size_t) octet & 0x0fu);
  else if (octet <= 0x9fu)
    /* fixvector 	1001xxxx 	0x90 - 0x9f */
    return unpickle_vector(pic, ob, (size_t) octet & 0x0fu);
  else if (octet <= 0xbfu)
    /* fixstr 	101xxxxx 	0xa0 - 0xbf */
    return unpickle_string(pic, ob, (size_t) octet & 0x1fu);
  else switch((enum pic_pickle_types) octet){
    case PIC_PT_NIL:
      /* nil 	11000000 	0xc0 */
      return pic_nil_value();
    case PIC_PT_CHAR:
      /* char 	11000001 	0xc1 */
      return unpickle_char(pic, ob);
    case PIC_PT_FALSE:
      /* false 	11000010 	0xc2 */
      return pic_false_value();
    case PIC_PT_TRUE:
      /* true 	11000011 	0xc3 */
      return pic_true_value();
    case PIC_PT_BYTEVECTOR8:
      /* bytevector 8 	11000100 	0xc4 */
      return unpickle_blob(pic, ob, ob_read_uint8_t(ob));
    case PIC_PT_BYTEVECTOR16:
      /* bytevector 16 	11000101 	0xc5 */
      return unpickle_blob(pic, ob, ob_read_uint16_t(ob));
    case PIC_PT_BYTEVECTOR32:
      /* bytevector 32 	11000110 	0xc6 */
      return unpickle_blob(pic, ob, ob_read_uint32_t(ob));
    case PIC_PT_EXT8:
      /* ext 8 	11000111 	0xc7 */
      return unpickle_ext(pic, ob, ob_read_uint8_t(ob));
    case PIC_PT_EXT16:
      /* ext 16 	11001000 	0xc8 */
      return unpickle_ext(pic, ob, ob_read_uint16_t(ob));
    case PIC_PT_EXT32:
      /* ext 32 	11001001 	0xc9 */
      return unpickle_ext(pic, ob, ob_read_uint32_t(ob));
    case PIC_PT_FLOAT32:
      /* float 32 	11001010 	0xca */
      return unpickle_float(pic, ob, 32);
    case PIC_PT_FLOAT64:
      /* float 64 	11001011 	0xcb */
      return unpickle_float(pic, ob, 64);
    case PIC_PT_SYMBOL8:
      /* symbol 8 	11001100 	0xcc */
      return unpickle_symbol(pic, ob, ob_read_uint8_t(ob));
    case PIC_PT_SYMBOL16:
      /* symbol 16 	11001101 	0xcd */
      return unpickle_symbol(pic, ob, ob_read_uint16_t(ob));
    case PIC_PT_SYMBOL32:
      /* symbol 32 	11001110 	0xce */
      return unpickle_symbol(pic, ob, ob_read_uint32_t(ob));
    case PIC_PT_SYMBOL64:
      /* symbol 64 	11001111 	0xcf */
      return unpickle_symbol(pic, ob, ob_read_uint64_t(ob));
    case PIC_PT_INT8:
      /* int 8 	11010000 	0xd0 */
      return pic_int_value((int) ob_read_int8_t(ob));
    case PIC_PT_INT16:
      /* int 16 	11010001 	0xd1 */
      return pic_int_value((int) ob_read_int16_t(ob));
    case PIC_PT_INT32:
      /* int 32 	11010010 	0xd2 */
      return pic_int_value((int) ob_read_int32_t(ob));
    case PIC_PT_INT64:
      /* int 64 	11010011 	0xd3 */
      return pic_int_value((int) ob_read_int64_t(ob));
    case PIC_PT_FIXEXT1:
      /* fixext 1 	11010100 	0xd4 */
      return unpickle_ext(pic, ob, 1);
    case PIC_PT_FIXEXT2:
      /* fixext 2 	11010101 	0xd5 */
      return unpickle_ext(pic, ob, 2);
    case PIC_PT_FIXEXT4:
      /* fixext 4 	11010110 	0xd6 */
      return unpickle_ext(pic, ob, 4);
    case PIC_PT_FIXEXT8:
      /* fixext 8 	11010111 	0xd7 */
      return unpickle_ext(pic, ob, 8);
    case PIC_PT_FIXEXT16:
      /* fixext 16 	11011000 	0xd8 */
      return unpickle_ext(pic, ob, 16);
    case PIC_PT_STR8:
      /* str 8 	11011001 	0xd9 */
      return unpickle_string(pic, ob, ob_read_uint8_t(ob));
    case PIC_PT_STR16:
      /* str 16 	11011010 	0xda */
      return unpickle_string(pic, ob, ob_read_uint16_t(ob));
    case PIC_PT_STR32:
      /* str 32 	11011011 	0xdb */
      return unpickle_string(pic, ob, ob_read_uint32_t(ob));
    case PIC_PT_VECTOR16:
      /* vector 16 	11011100 	0xdc */
      return unpickle_vector(pic, ob, ob_read_uint16_t(ob));
    case PIC_PT_VECTOR32:
      /* vector 32 	11011101 	0xdd */
      return unpickle_vector(pic, ob, ob_read_uint32_t(ob));
    case PIC_PT_LIST16:
      /* list 16 	11011110 	0xde */
      return unpickle_list(pic, ob, ob_read_uint16_t(ob));
    case PIC_PT_LIST32:
      /* list 32 	11011111 	0xdf */
      return unpickle_list(pic, ob, ob_read_uint32_t(ob));
    default:
      pic_errorf(pic, "logic flow");
    }
}

static pic_value
pic_pickle(pic_state *pic)
{
  struct octet_buffer ob;
  struct pic_blob *bv;
  pic_value obj;

  ob_init(&ob);

  pic_get_args(pic, "o", &obj);

  pickle(pic, &ob, obj);
  bv = pic_make_blob(pic, ob.pointer);
  memcpy(bv->data, ob.data, ob.pointer);

  ob_destroy(&ob);

  return pic_obj_value(bv);
}

static pic_value
pic_unpickle(pic_state *pic)
{
  struct octet_buffer ob;
  struct pic_blob *blob;

  ob_init(&ob);

  pic_get_args(pic, "b", &blob);

  ob.data = (uint8_t *)blob->data;
  ob.size = blob->len;

  ob_as_reader(&ob);

  return unpickle(pic, &ob);;
}

void
pic_init_pickle(pic_state *pic)
{
  pic_defun(pic, "pickle", pic_pickle);
  pic_defun(pic, "unpickle", pic_unpickle);
}
