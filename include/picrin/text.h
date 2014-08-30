/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_TEXT_H__
#define PICRIN_TEXT_H__

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_text {
  PIC_OBJECT_HEADER
  char *data;
  size_t len;
  size_t cap; /* capacity */
};

#define pic_text_p(v) (pic_type(v) == PIC_TT_TEXT)
#define pic_text_ptr(o) ((struct pic_text *)pic_ptr(o))

struct pic_text *pic_text_new(pic_state *, size_t , const char * /* nullable */, size_t);
const char *pic_text_cstr(struct pic_text *);
size_t pic_text_len(struct pic_text *);
size_t pic_text_capacity(struct pic_text *);


#if defined(__cplusplus)
}
#endif

#endif
