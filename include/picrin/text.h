/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_TEXT_H__
#define PICRIN_TEXT_H__

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_transient {
  PIC_OBJECT_HEADER
  char *data;
  size_t len;
  size_t cap; /* capacity */
};

#define pic_trans_p(v) (pic_type(v) == PIC_TT_TRANSIENT)
#define pic_trans_ptr(o) ((struct pic_transient *)pic_ptr(o))

struct pic_transient *pic_trans_new(pic_state *, size_t , const char * /* nullable */, size_t);
const char *pic_trans_cstr(struct pic_transient *);
size_t pic_trans_len(struct pic_transient *);
size_t pic_trans_capacity(struct pic_transient *);


#if defined(__cplusplus)
}
#endif

#endif
