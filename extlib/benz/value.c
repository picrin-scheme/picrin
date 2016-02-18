/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

int
pic_type(pic_state PIC_UNUSED(*pic), pic_value v)
{
  int tt = pic_vtype(pic, v);

  if (tt < PIC_IVAL_END) {
    return tt;
  }
  return ((struct pic_basic *)pic_obj_ptr(v))->tt;
}

const char *
pic_typename(pic_state *pic, int type)
{
  switch (type) {
  case PIC_TYPE_NIL:
    return "null";
  case PIC_TYPE_TRUE:
  case PIC_TYPE_FALSE:
    return "boolean";
  case PIC_TYPE_FLOAT:
    return "float";
  case PIC_TYPE_INT:
    return "int";
  case PIC_TYPE_SYMBOL:
    return "symbol";
  case PIC_TYPE_CHAR:
    return "char";
  case PIC_TYPE_EOF:
    return "eof-object";
  case PIC_TYPE_UNDEF:
    return "undefined";
  case PIC_TYPE_INVALID:
    return "invalid";
  case PIC_TYPE_PAIR:
    return "pair";
  case PIC_TYPE_STRING:
    return "string";
  case PIC_TYPE_VECTOR:
    return "vector";
  case PIC_TYPE_BLOB:
    return "bytevector";
  case PIC_TYPE_PORT:
    return "port";
  case PIC_TYPE_ERROR:
    return "error";
  case PIC_TYPE_ID:
    return "identifier";
  case PIC_TYPE_CXT:
    return "context";
  case PIC_TYPE_PROC:
    return "procedure";
  case PIC_TYPE_ENV:
    return "environment";
  case PIC_TYPE_DATA:
    return "data";
  case PIC_TYPE_DICT:
    return "dictionary";
  case PIC_TYPE_WEAK:
    return "ephemeron";
  case PIC_TYPE_RECORD:
    return "record";
  case PIC_TYPE_CP:
    return "checkpoint";
  default:
    pic_errorf(pic, "pic_typename: invalid type given %d", type);
  }
}

void *
pic_data(pic_state PIC_UNUSED(*pic), struct pic_data *data)
{
  return data->data;
}

struct pic_data *
pic_data_value(pic_state *pic, void *userdata, const pic_data_type *type)
{
  struct pic_data *data;

  data = (struct pic_data *)pic_obj_alloc(pic, sizeof(struct pic_data), PIC_TYPE_DATA);
  data->type = type;
  data->data = userdata;

  return data;
}
