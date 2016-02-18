/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_DICT_H
#define PICRIN_DICT_H

#if defined(__cplusplus)
extern "C" {
#endif

KHASH_DECLARE(dict, pic_sym *, pic_value)

struct pic_dict {
  PIC_OBJECT_HEADER
  khash_t(dict) hash;
};

#define pic_dict_ptr(v) ((struct pic_dict *)pic_obj_ptr(v))

#if defined(__cplusplus)
}
#endif

#endif
