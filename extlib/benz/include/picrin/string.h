/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_STRING_H
#define PICRIN_STRING_H

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_string {
  PIC_OBJECT_HEADER
  struct pic_rope *rope;
};

void pic_rope_incref(pic_state *, struct pic_rope *);
void pic_rope_decref(pic_state *, struct pic_rope *);

#define pic_str_ptr(o) ((struct pic_string *)pic_obj_ptr(o))

struct pic_string *pic_make_str(pic_state *, const char *, int);
#define pic_make_cstr(pic, cstr) pic_make_str(pic, (cstr), strlen(cstr))
#define pic_make_lit(pic, lit) pic_make_str(pic, "" lit, -((int)sizeof lit - 1))

const char *pic_str_cstr(pic_state *, struct pic_string *);

struct pic_string *pic_format(pic_state *, const char *, ...);
struct pic_string *pic_vformat(pic_state *, const char *, va_list);

#if defined(__cplusplus)
}
#endif

#endif
