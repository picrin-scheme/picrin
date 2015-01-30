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

struct pic_chunk {
  char *str;
  int refcnt;
  size_t len;
  char autofree, zeroterm;
};

#define XR_CHUNK_INCREF(c) do {                 \
    (c)->refcnt++;                              \
  } while (0)

#define XR_CHUNK_DECREF(c) do {                 \
    struct pic_chunk *c__ = (c);                \
    if (! --c__->refcnt) {                      \
      if (c__->autofree)                        \
        free(c__->str);                         \
      free(c__);                                \
    }                                           \
  } while (0)

struct pic_rope {
  int refcnt;
  size_t weight;
  struct pic_chunk *chunk;
  size_t offset;
  struct pic_rope *left, *right;
};

PIC_INLINE void
XROPE_INCREF(struct pic_rope *x) {
  x->refcnt++;
}

PIC_INLINE void
XROPE_DECREF(struct pic_rope *x) {
  if (! --x->refcnt) {
    if (x->chunk) {
      XR_CHUNK_DECREF(x->chunk);
      free(x);
      return;
    }
    XROPE_DECREF(x->left);
    XROPE_DECREF(x->right);
    free(x);
  }
}

#define pic_str_p(v) (pic_type(v) == PIC_TT_STRING)
#define pic_str_ptr(o) ((struct pic_string *)pic_ptr(o))

pic_str *pic_make_str(pic_state *, const char * /* nullable */, size_t);
pic_str *pic_make_str_cstr(pic_state *, const char *);
pic_str *pic_make_str_fill(pic_state *, size_t, char);

size_t pic_strlen(pic_str *);
char pic_str_ref(pic_state *, pic_str *, size_t);

pic_str *pic_strcat(pic_state *, pic_str *, pic_str *);
pic_str *pic_substr(pic_state *, pic_str *, size_t, size_t);
int pic_strcmp(pic_str *, pic_str *);

const char *pic_str_cstr(pic_str *);

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
