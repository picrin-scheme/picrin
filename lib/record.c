/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"

pic_value
pic_make_record(pic_state *pic, pic_value type, pic_value datum)
{
  struct record *rec;

  rec = (struct record *)pic_obj_alloc(pic, sizeof(struct record), PIC_TYPE_RECORD);
  rec->type = sym_ptr(pic, type);
  rec->datum = datum;

  return obj_value(pic, rec);
}

pic_value
pic_record_type(pic_state *pic, pic_value rec)
{
  return obj_value(pic, rec_ptr(pic, rec)->type);
}

pic_value
pic_record_datum(pic_state *pic, pic_value rec)
{
  return rec_ptr(pic, rec)->datum;
}

static pic_value
pic_rec_make_record(pic_state *pic)
{
  pic_value type, datum;

  pic_get_args(pic, "mo", &type, &datum);

  return pic_make_record(pic, type, datum);
}

static pic_value
pic_rec_record_p(pic_state *pic)
{
  pic_value rec;

  pic_get_args(pic, "o", &rec);

  return pic_bool_value(pic, pic_rec_p(pic, rec));
}

static pic_value
pic_rec_record_type(pic_state *pic)
{
  pic_value rec;

  pic_get_args(pic, "r", &rec);

  return pic_record_type(pic, rec);
}

static pic_value
pic_rec_record_datum(pic_state *pic)
{
  pic_value rec;

  pic_get_args(pic, "r", &rec);

  return pic_record_datum(pic, rec);
}

void
pic_init_record(pic_state *pic)
{
  pic_defun(pic, "make-record", pic_rec_make_record);
  pic_defun(pic, "record?", pic_rec_record_p);
  pic_defun(pic, "record-type", pic_rec_record_type);
  pic_defun(pic, "record-datum", pic_rec_record_datum);
}
