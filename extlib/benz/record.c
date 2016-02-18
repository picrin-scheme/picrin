/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

struct pic_record *
pic_make_rec(pic_state *pic, pic_value type, pic_value datum)
{
  struct pic_record *rec;

  rec = (struct pic_record *)pic_obj_alloc(pic, sizeof(struct pic_record), PIC_TT_RECORD);
  rec->type = type;
  rec->datum = datum;

  return rec;
}

pic_value
pic_rec_type(pic_state PIC_UNUSED(*pic), struct pic_record *rec)
{
  return rec->type;
}

pic_value
pic_rec_datum(pic_state PIC_UNUSED(*pic), struct pic_record *rec)
{
  return rec->datum;
}

static pic_value
pic_rec_make_record(pic_state *pic)
{
  pic_value type, datum;

  pic_get_args(pic, "oo", &type, &datum);

  return pic_obj_value(pic_make_rec(pic, type, datum));
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
  struct pic_record *rec;

  pic_get_args(pic, "r", &rec);

  return pic_rec_type(pic, rec);
}

static pic_value
pic_rec_record_datum(pic_state *pic)
{
  struct pic_record *rec;

  pic_get_args(pic, "r", &rec);

  return pic_rec_datum(pic, rec);
}

void
pic_init_record(pic_state *pic)
{
  pic_defun(pic, "make-record", pic_rec_make_record);
  pic_defun(pic, "record?", pic_rec_record_p);
  pic_defun(pic, "record-type", pic_rec_record_type);
  pic_defun(pic, "record-datum", pic_rec_record_datum);
}
