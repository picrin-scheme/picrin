/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/record.h"

struct pic_record *
pic_record_new(pic_state *pic, pic_value rectype)
{
  struct pic_record *rec;

  rec = (struct pic_record *)pic_obj_alloc(pic, sizeof(struct pic_record), PIC_TT_RECORD);
  xh_init_int(&rec->hash, sizeof(pic_value));

  pic_record_set(pic, rec, pic_intern_cstr(pic, "@@type"), rectype);

  return rec;
}

pic_value
pic_record_type(pic_state *pic, struct pic_record *rec)
{
  return pic_record_ref(pic, rec, pic_intern_cstr(pic, "@@type"));
}

pic_value
pic_record_ref(pic_state *pic, struct pic_record *rec, pic_sym slot)
{
  xh_entry *e;

  e = xh_get_int(&rec->hash, slot);
  if (! e) {
    pic_errorf(pic, "slot named ~s is not found for record: ~s", pic_sym_value(slot), rec);
  }
  return xh_val(e, pic_value);
}

void
pic_record_set(pic_state *pic, struct pic_record *rec, pic_sym slot, pic_value val)
{
  UNUSED(pic);

  xh_put_int(&rec->hash, slot, &val);
}

static pic_value
pic_record_make_record(pic_state *pic)
{
  struct pic_record * rec;
  pic_value rectype;

  pic_get_args(pic, "o", &rectype);

  rec = pic_record_new(pic, rectype);

  return pic_obj_value(rec);
}

static pic_value
pic_record_record_p(pic_state *pic)
{
  pic_value rec;

  pic_get_args(pic, "o", &rec);

  return pic_bool_value(pic_record_p(rec));
}

static pic_value
pic_record_record_type(pic_state *pic)
{
  struct pic_record *rec;

  pic_get_args(pic, "r", &rec);

  return pic_record_type(pic, rec);
}

static pic_value
pic_record_record_ref(pic_state *pic)
{
  struct pic_record *rec;
  pic_sym slot;

  pic_get_args(pic, "rm", &rec, &slot);

  return pic_record_ref(pic, rec, slot);
}

static pic_value
pic_record_record_set(pic_state *pic)
{
  struct pic_record *rec;
  pic_sym slot;
  pic_value val;

  pic_get_args(pic, "rmo", &rec, &slot, &val);

  pic_record_set(pic, rec, slot, val);

  return pic_none_value();
}

void
pic_init_record(pic_state *pic)
{
  pic_deflibrary (pic, "(picrin record)") {
    pic_defun(pic, "make-record", pic_record_make_record);
    pic_defun(pic, "record?", pic_record_record_p);
    pic_defun(pic, "record-type", pic_record_record_type);
    pic_defun(pic, "record-ref", pic_record_record_ref);
    pic_defun(pic, "record-set!", pic_record_record_set);
  }
}
