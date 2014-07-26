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
  rec->rectype = rectype;
  xh_init_int(&rec->hash, sizeof(pic_value));

  return rec;
}

bool
pic_record_of(pic_state *pic, struct pic_record *rec, pic_value rectype) {
  UNUSED(pic);

  return pic_eq_p(rec->rectype, rectype);
}

pic_value
pic_record_ref(pic_state *pic, struct pic_record *rec, pic_value rectype, pic_sym slotname)
{
  xh_entry *e;

  if (! pic_eq_p(rec->rectype, rectype)) {
    pic_errorf(pic, "value is not record of ~s", rectype);
  }

  e = xh_get_int(&rec->hash, slotname);
  if (! e) {
    pic_errorf(pic, "slot named ~s is not found for record: ~s", pic_sym_value(slotname), rectype);
  }
  return xh_val(e, pic_value);
}


void
pic_record_set(pic_state *pic, struct pic_record *rec, pic_value rectype, pic_sym slotname, pic_value val)
{
  if (! pic_eq_p(rec->rectype, rectype)) {
    pic_errorf(pic, "value is not record of ~s", rectype);
  }

  xh_put_int(&rec->hash, slotname, &val);
}

static pic_value
pic_record_record(pic_state *pic)
{
  struct pic_record * rec;
  pic_value rectype;

  pic_get_args(pic, "o", &rectype);

  rec = pic_record_new(pic, rectype);

  return pic_obj_value(rec);
}

static pic_value
pic_record_record_of(pic_state *pic)
{
  struct pic_record *rec;
  pic_value rectype;

  pic_get_args(pic, "ro", &rec, &rectype);

  return pic_bool_value(pic_record_of(pic, rec, rectype));
}

static pic_value
pic_record_record_ref(pic_state *pic)
{
  struct pic_record *rec;
  pic_value rectype;
  pic_sym slotname;

  pic_get_args(pic, "rom", &rec, &rectype, &slotname);

  return pic_record_ref(pic, rec, rectype, slotname);
}

static pic_value
pic_record_record_set(pic_state *pic)
{
  struct pic_record *rec;
  pic_value rectype;
  pic_sym slotname;
  pic_value val;

  pic_get_args(pic, "romo", &rec, &rectype, &slotname, &val);

  pic_record_set(pic, rec, rectype, slotname, val);

  return pic_none_value();
}

void
pic_init_record(pic_state *pic)
{
  pic_deflibrary ("(picrin record-primitive)") {
    pic_defun(pic, "make-record", pic_record_record);
    pic_defun(pic, "record-of?", pic_record_record_of);
    pic_defun(pic, "record-ref", pic_record_record_ref);
    pic_defun(pic, "record-set!", pic_record_record_set);
  }
}
