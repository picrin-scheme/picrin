#ifndef STRING_H__
#define STRING_H__

struct pic_string {
  PIC_OBJECT_HEADER
  const char *str;
  size_t len;
};

#define pic_str_ptr(v) ((struct pic_string *)v.u.data)

pic_value pic_str_new_cstr(pic_state *, const char *);

#endif
