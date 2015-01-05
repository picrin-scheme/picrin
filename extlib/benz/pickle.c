/**
 * See Copyright Notice in picrin.h
 */

#include <string.h>

#include "picrin.h"
#include "picrin/blob.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/vector.h"

#define OB_CHUNK_SIZE 128

struct octet_buffer {
  uint8_t *data;
  size_t size, pointer;
};

static inline void
ob_init(struct octet_buffer *ob)
{
  ob->data = NULL;
  ob->size = 0;
  ob->pointer = 0;
}

static inline void
ob_destroy(struct octet_buffer *ob)
{
  free(ob->data);
}

static inline void
ob_enlarge(struct octet_buffer *ob)
{
  ob->data = (uint8_t *)realloc(ob->data, ob->size + OB_CHUNK_SIZE);
  ob->size += OB_CHUNK_SIZE;
}

static inline void
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
  static inline void                                            \
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
  void                                                          \
  ob_push_##type(struct octet_buffer *ob, type octets)          \
  {                                                             \
    while (ob->pointer + sizeof(type) > ob->size) {             \
      ob_enlarge(ob);                                           \
    }                                                           \
                                                                \
    ob->pointer += sizeof(type);                                \
    for(size_t i = 0; i < sizeof(type); i++){                   \
      ob->data[--ob->pointer] = (uint8_t) octets & 0xff;        \
      octets >>= 8;                                             \
    }                                                           \
    ob->pointer += sizeof(type);                                \
}
#endif

DEFINE_OB_PUSHER(uint8_t)
DEFINE_OB_PUSHER(uint16_t)
DEFINE_OB_PUSHER(uint32_t)
DEFINE_OB_PUSHER(uint64_t)
#define ob_push ob_push_uint8_t

static inline void
ob_as_reader(struct octet_buffer *ob)
{
  /* after this function, don't call ob_push* functions */
  ob->pointer = 0;
}

static inline uint8_t
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
  static inline type                                    \
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
#define DEFINE_OB_READER(type)                          \
  static inline type                                    \
  ob_read_##type(struct octet_buffer *ob)               \
  {                                                     \
    assert(ob->size >= ob->pointer + sizeof(type));     \
                                                        \
    type res = 0;                                       \
                                                        \
    for(size_t i = 0; i < sizeof(type); i++){           \
      res <<= 8;                                        \
      res += *(ob->data + ob->pointer++);               \
    }                                                   \
                                                        \
    return res;                                         \
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

void pickle(pic_state *, struct octet_buffer *, pic_value);

void
pickle_int(pic_state *pic, struct octet_buffer *ob, int i)
{
  UNUSED(pic);

  if ( 0x00 <= i && i <= 0x7f) {
    /* positive fixint 	0xxxxxxx 	0x00 - 0x7f */
    ob_push(ob, (uint8_t) i);
  }
  else if (0xff <= i && i <= 0xe0) {
    /* negative fixint 	111xxxxx 	0xe0 - 0xff */
    ob_push(ob, (uint8_t) 0xe0 | i);
  }
  else if ( -1 << 7 <= i && i <= 1 << 7 - 1) {
    /* int 8 	11010000 	0xd0 */
    ob_push(ob, 0xd0);
    ob_push_uint8_t(ob, (uint8_t) i);
  }
  else if ( -1 << 15 <= i && i <= 1 << 15 - 1) {
    /* int 16 	11010001 	0xd1 */
    ob_push(ob, 0xd1);
    ob_push_uint16_t(ob, (uint16_t ) i);
  }
  else if ( -1 << 31 <= i && i <= 1 << 31 - 1) {
    /* int 32 	11010010 	0xd2 */
    ob_push(ob, 0xd2);
    ob_push_uint32_t(ob, (uint32_t ) i);
  }
  else {
    /* int 64 	11010011 	0xd3 */
    ob_push(ob, 0xd3);
    ob_push_uint64_t(ob, (uint64_t ) i);
  }
  
}

void
pickle_double(pic_state *pic, struct octet_buffer *ob, double d)
{
  UNUSED(pic);

  /* float 32 	11001010 	0xca */
  /* float 64 	11001011 	0xcb */
  /* picrin has only double */
  

  ob_push(ob, 0xcb);
  ob_push_uint64_t(ob, *(uint64_t *)&d);  
}

void
pickle_string(pic_state *pic, struct octet_buffer *ob, pic_str *str)
{
  UNUSED(pic);

  const char *cstr;
  size_t len;

  cstr = pic_str_cstr(str);
  len = pic_strlen(str);

  if (len < 1<<5)
    /* fixstr 	101xxxxx 	0xa0 - 0xbf */
    ob_push(ob, 0xa0 | len);
  else
    /* str 8 	11011001 	0xd9 */
    /* str 16 	11011010 	0xda */      
    /* str 32 	11011011 	0xdb */      
    ob_push_len(ob, len, 0xd9);
    
  ob_pushn(ob, (const uint8_t *)cstr, len);
}

void
pickle_symbol(pic_state *pic, struct octet_buffer *ob, pic_sym sym)
{
  UNUSED(pic);

  /* symbol 8 	11001100 	0xcc */
  /* symbol 16 	11001101 	0xcd */
  /* symbol 32 	11001110 	0xce */
  /* symbol 64 	11001111 	0xcf */
  ob_push_len64(ob, sym, (uint8_t) 0xcc);
}

void
pickle_list(pic_state *pic, struct octet_buffer *ob, pic_value list)
{
  pic_value v;
  size_t len;

  len = pic_length(pic, list);
  if (len < 1 << 4)
    /* fixlist 	1000xxxx 	0x80 - 0x8f */
    ob_push(ob, 0x80 | len);
  else
    /* list 16 	11011110 	0xde */
    /* list 32 	11011111 	0xdf */
    ob_push_16len(ob, len, 0xde);
    
  pic_for_each(v, list){
    pickle(pic, ob, v);
  }
  
}

void
pickle_nil(pic_state *pic, struct octet_buffer *ob)
{
  UNUSED(pic);
  ob_push(ob, 0xc0);
}

void
pickle_bool(pic_state *pic, struct octet_buffer *ob, pic_value b)
{
  UNUSED(pic);

  /* true 	11000011 	0xc3 */
  /* false 	11000010 	0xc2 */
  ob_push(ob, (uint8_t) 0xc2 + (uint8_t) pic_true_p(b));
}

void
pickle_blob(pic_state *pic, struct octet_buffer *ob, pic_blob *blob)
{
  UNUSED(pic);

  size_t len;

  len = blob->len;
  /* bytevector 8 	11000100 	0xc4 */
  /* bytevector 16 	11000101 	0xc5 */
  /* bytevector 32 	11000110 	0xc6 */
  ob_push_len(ob, len, 0xc4);

  ob_pushn(ob, (uint8_t *) blob->data, len);  
}

void
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
    ob_push_16len(ob, len, (uint16_t) 0xdc);

  for (i = 0; i < len; i++) {
    pickle(pic, ob, *(vec->data + i));
  }
}

void
pickle_char(pic_state *pic, struct octet_buffer *ob, pic_value obj)
{
  UNUSED(pic);

  /* char 	11000001 	0xc1 */
  /* current picrin supports only ascii chars */
  ob_push(ob, (uint8_t) 0xc1);
  ob_push(ob, (uint8_t) pic_char(obj));
}

void
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
    pickle_symbol(pic, ob,  pic_sym(obj));
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

  UNUSED(pic);

  if (len == 32) {
    uint32_t i = ob_read_uint32_t(ob);
    float f;
    memcpy(&f, &i , sizeof(float));
    return pic_float_value((double) f);
  }
  else{
    uint64_t i = ob_read_uint64_t(ob);
    double d;
    memcpy(&d, &i, sizeof(double));
    return pic_float_value(d);
  }
}

static pic_value
unpickle_symbol(pic_state *pic, struct octet_buffer *ob, size_t sym)
{
  UNUSED(pic);
  UNUSED(ob);
  UNUSED(sym);

  return pic_none_value();
}

static pic_value
unpickle_char(pic_state *pic, struct octet_buffer *ob)
{
  UNUSED(pic);

  /* current picrin supports only ascii chars */
  return pic_char_value(ob_read(ob));
}

static pic_value
unpickle_ext(pic_state *pic, struct octet_buffer *ob, size_t len)
{
  UNUSED(pic);
  UNUSED(ob);
  UNUSED(len);
  UNUSED(ob_readn(ob, len));

  return pic_none_value();
}

static pic_value
unpickle(pic_state *pic, struct octet_buffer *ob)
{
  uint8_t octet;

  octet = ob_read(ob);
  
  if (octet >= 0xe0)
    /* negative fixint 	111xxxxx 	0xe0 - 0xff */
    return pic_int_value((int) (char) octet);
  else if (octet <= 0x7f)
    /* positive fixint 	0xxxxxxx 	0x00 - 0x7f */
    return pic_int_value((int) (char) octet);
  else if (octet <= 0x8f)
    /* fixlist 	1000xxxx 	0x80 - 0x8f */
    return unpickle_list(pic, ob, (size_t) octet & 0x0f);
  else if (octet <= 0x9f)
    /* fixvector 	1001xxxx 	0x90 - 0x9f */
    return unpickle_vector(pic, ob, (size_t) octet & 0x0f);
  else if (octet <= 0xbf)
    /* fixstr 	101xxxxx 	0xa0 - 0xbf */
    return unpickle_string(pic, ob, (size_t) octet & 0x1f);
  else switch(octet){
    case 0xc0:
      /* nil 	11000000 	0xc0 */
    return pic_nil_value();
    case 0xc1:
      /* char 	11000001 	0xc1 */
      return unpickle_char(pic, ob);
    case 0xc2:
      /* false 	11000010 	0xc2 */
      return pic_false_value();
    case 0xc3:
      /* true 	11000011 	0xc3 */
      return pic_true_value();
    case 0xc4:
      /* bytevector 8 	11000100 	0xc4 */
      return unpickle_blob(pic, ob, ob_read_uint8_t(ob));
    case 0xc5:
      /* bytevector 16 	11000101 	0xc5 */
      return unpickle_blob(pic, ob, ob_read_uint16_t(ob));
    case 0xc6:
      /* bytevector 32 	11000110 	0xc6 */
      return unpickle_blob(pic, ob, ob_read_uint32_t(ob));
    case 0xc7:
      /* ext 8 	11000111 	0xc7 */
      return unpickle_ext(pic, ob, ob_read_uint8_t(ob));
    case 0xc8:
      /* ext 16 	11001000 	0xc8 */
      return unpickle_ext(pic, ob, ob_read_uint16_t(ob));
    case 0xc9:
      /* ext 32 	11001001 	0xc9 */
      return unpickle_ext(pic, ob, ob_read_uint32_t(ob));
    case 0xca:
      /* float 32 	11001010 	0xca */
      return unpickle_float(pic, ob, 32);
    case 0xcb:
      /* float 64 	11001011 	0xcb */
      return unpickle_float(pic, ob, 64);
    case 0xcc:
      /* symbol 8 	11001100 	0xcc */
      return unpickle_symbol(pic, ob, ob_read_uint8_t(ob));
    case 0xcd:
      /* symbol 16 	11001101 	0xcd */
      return unpickle_symbol(pic, ob, ob_read_uint16_t(ob));
    case 0xce:
      /* symbol 32 	11001110 	0xce */
      return unpickle_symbol(pic, ob, ob_read_uint32_t(ob));
    case 0xcf:
      /* symbol 64 	11001111 	0xcf */
      return unpickle_symbol(pic, ob, ob_read_uint64_t(ob));
    case 0xd0:
      /* int 8 	11010000 	0xd0 */
      return pic_int_value((int) ob_read_int8_t(ob));
    case 0xd1:
      /* int 16 	11010001 	0xd1 */
      return pic_int_value((int) ob_read_int16_t(ob));
    case 0xd2:
      /* int 32 	11010010 	0xd2 */
      return pic_int_value((int) ob_read_int32_t(ob));
    case 0xd3:
      /* int 64 	11010011 	0xd3 */
      return pic_int_value((int) ob_read_int64_t(ob));
    case 0xd4:
      /* fixext 1 	11010100 	0xd4 */
      return unpickle_ext(pic, ob, 1);
    case 0xd5:
      /* fixext 2 	11010101 	0xd5 */
      return unpickle_ext(pic, ob, 2);
    case 0xd6:
      /* fixext 4 	11010110 	0xd6 */
      return unpickle_ext(pic, ob, 4);
    case 0xd7:
      /* fixext 8 	11010111 	0xd7 */
      return unpickle_ext(pic, ob, 8);
    case 0xd8:
      /* fixext 16 	11011000 	0xd8 */
      return unpickle_ext(pic, ob, 16);
    case 0xd9:
      /* str 8 	11011001 	0xd9 */
      return unpickle_string(pic, ob, ob_read_uint8_t(ob));
    case 0xda:
      /* str 16 	11011010 	0xda */
      return unpickle_string(pic, ob, ob_read_uint16_t(ob));
    case 0xdb:
      /* str 32 	11011011 	0xdb */
      return unpickle_string(pic, ob, ob_read_uint32_t(ob));
    case 0xdc:
      /* vector 16 	11011100 	0xdc */
      return unpickle_vector(pic, ob, ob_read_uint16_t(ob));
    case 0xdd:
      /* vector 32 	11011101 	0xdd */
      return unpickle_vector(pic, ob, ob_read_uint32_t(ob));
    case 0xde:
      /* list 16 	11011110 	0xde */
      return unpickle_list(pic, ob, ob_read_uint16_t(ob));
    case 0xdf:
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

  return unpickle(pic, &ob);
}

void
pic_init_pickle(pic_state *pic)
{
  pic_defun(pic, "pickle", pic_pickle);
  pic_defun(pic, "unpickle", pic_unpickle);
}
