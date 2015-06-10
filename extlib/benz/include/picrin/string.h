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

#define pic_str_p(v) (pic_type(v) == PIC_TT_STRING)
#define pic_str_ptr(o) ((struct pic_string *)pic_ptr(o))

pic_str *pic_make_str(pic_state *, const char * /* nullable */, size_t);
pic_str *pic_make_str_cstr(pic_state *, const char *);
pic_str *pic_make_str_fill(pic_state *, size_t, char);

char pic_str_ref(pic_state *, pic_str *, size_t);
size_t pic_str_len(pic_str *);
pic_str *pic_str_cat(pic_state *, pic_str *, pic_str *);
pic_str *pic_str_sub(pic_state *, pic_str *, size_t, size_t);
int pic_str_cmp(pic_state *, pic_str *, pic_str *);
const char *pic_str_cstr(pic_state *, pic_str *);

pic_str *pic_format(pic_state *, const char *, ...);
pic_str *pic_vformat(pic_state *, const char *, va_list);
void pic_vfformat(pic_state *, xFILE *, const char *, va_list);

pic_value pic_xformat(pic_state *, const char *, ...);
pic_value pic_xvformat(pic_state *, const char *, va_list);
pic_value pic_xvfformat(pic_state *, xFILE *, const char *, va_list);

#if defined(__cplusplus)
}
#endif

#endif
