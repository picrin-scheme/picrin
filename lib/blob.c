/**
 * See Copyright Notice in picrin.h
 */

#include <picrin.h>
#include "value.h"
#include "object.h"

static void dump1(unsigned char c, unsigned char *buf, int *len) {
  if (buf) {
    buf[*len] = c;
  }
  *len = *len + 1;
}

static void dump4(unsigned long n, unsigned char *buf, int *len) {
  assert(sizeof(long) * CHAR_BIT <= 32 || n <= 0xfffffffful);

  dump1((n & 0xff), buf, len);
  dump1((n & 0xff00) >> 8, buf, len);
  dump1((n & 0xff0000) >> 16, buf, len);
  dump1((n & 0xff000000) >> 24, buf, len);
}

static void dump_obj(pic_state *pic, pic_value obj, unsigned char *buf, int *len);

#define IREP_FLAGS_MASK (IREP_VARG)

static void
dump_irep(pic_state *pic, struct irep *irep, unsigned char *buf, int *len)
{
  size_t i;
  dump1(irep->argc, buf, len);
  dump1(irep->flags & IREP_FLAGS_MASK, buf, len);
  dump1(irep->frame_size, buf, len);
  dump1(irep->irepc, buf, len);
  dump1(irep->objc, buf, len);
  dump4(irep->codec, buf, len);
  for (i = 0; i < irep->objc; ++i) {
    dump_obj(pic, irep->obj[i], buf, len);
  }
  for (i = 0; i < irep->codec; ++i) {
    dump1(irep->code[i], buf, len);
  }
  for (i = 0; i < irep->irepc; ++i) {
    dump_irep(pic, irep->irep[i], buf, len);
  }
}

static void
dump_obj(pic_state *pic, pic_value obj, unsigned char *buf, int *len)
{
  if (pic_int_p(pic, obj)) {
    dump1(0x00, buf, len);
    dump4(pic_int(pic, obj), buf, len);
  } else if (pic_str_p(pic, obj)) {
    int l, i;
    const char *str = pic_str(pic, obj, &l);
    dump1(0x01, buf, len);
    dump4(l, buf, len);
    for (i = 0; i < l; ++i) {
      dump1(str[i], buf, len);
    }
    dump1(0, buf, len);
  } else if (pic_sym_p(pic, obj)) {
    int l, i;
    const char *str = pic_str(pic, pic_sym_name(pic, obj), &l);
    dump1(0x02, buf, len);
    dump4(l, buf, len);
    for (i = 0; i < l; ++i) {
      dump1(str[i], buf, len);
    }
    dump1(0, buf, len);
  } else if (pic_proc_p(pic, obj)) {
    if (pic_proc_func_p(pic, obj)) {
      pic_error(pic, "dump: c function procedure serialization unsupported", 1, obj);
    }
    if (proc_ptr(pic, obj)->env) {
      pic_error(pic, "dump: local procedure serialization unsupported", 1, obj);
    }
    dump1(0x03, buf, len);
    dump_irep(pic, proc_ptr(pic, obj)->u.irep, buf, len);
  } else if (pic_char_p(pic, obj)) {
    dump1(0x04, buf, len);
    dump1(pic_char(pic, obj), buf, len);
  } else {
    pic_error(pic, "dump: unsupported object", 1, obj);
  }
}

pic_value
pic_serialize(pic_state *pic, pic_value obj)
{
  int len = 0;
  pic_value blob;
  dump_obj(pic, obj, NULL, &len);
  blob = pic_blob_value(pic, NULL, len);
  len = 0;
  dump_obj(pic, obj, pic_blob(pic, blob, NULL), &len);
  return blob;
}

static void loadn(pic_state *pic, unsigned char *dst, size_t size, const unsigned char **buf, const unsigned char *end) {
  if (*buf + size > end) {
    pic_error(pic, "malformed bytevector", 0);
  }
  memcpy(dst, *buf, size);
  *buf = *buf + size;
}

static unsigned char load1(pic_state *pic, const unsigned char **buf, const unsigned char *end) {
  unsigned char c;
  loadn(pic, &c, 1, buf, end);
  return c;
}

static unsigned long load4(pic_state *pic, const unsigned char **buf, const unsigned char *end) {
  unsigned long x = load1(pic, buf, end);
  x += load1(pic, buf, end) << 8;
  x += load1(pic, buf, end) << 16;
  x += load1(pic, buf, end) << 24;
  return x;
}

static pic_value load_obj(pic_state *pic, const unsigned char **buf, const unsigned char *end);

static struct irep *
load_irep(pic_state *pic, const unsigned char **buf, const unsigned char *end)
{
  unsigned char argc, flags, frame_size, irepc, objc;
  size_t codec, i;
  pic_value *obj;
  unsigned char *code;
  struct irep **irep, *ir;
  size_t ai = pic_enter(pic);

  argc = load1(pic, buf, end);
  flags = load1(pic, buf, end);
  frame_size = load1(pic, buf, end);
  irepc = load1(pic, buf, end);
  objc = load1(pic, buf, end);
  codec = load4(pic, buf, end);
  obj = pic_malloc(pic, sizeof(pic_value) * objc);
  for (i = 0; i < objc; ++i) {
    obj[i] = load_obj(pic, buf, end);
  }
  code = pic_malloc(pic, codec); /* TODO */
  loadn(pic, code, codec, buf, end);
  irep = pic_malloc(pic, sizeof(struct irep *) * irepc);
  for (i = 0; i < irepc; ++i) {
    irep[i] = load_irep(pic, buf, end);
  }
  ir = (struct irep *) pic_obj_alloc(pic, PIC_TYPE_IREP);
  ir->argc = argc;
  ir->flags = flags;
  ir->frame_size = frame_size;
  ir->irepc = irepc;
  ir->objc = objc;
  ir->codec = codec;
  ir->obj = obj;
  ir->code = code;
  ir->irep = irep;
  pic_leave(pic, ai);
  pic_protect(pic, obj_value(pic, ir));
  return ir;
}

static pic_value
load_obj(pic_state *pic, const unsigned char **buf, const unsigned char *end)
{
  int type, l;
  pic_value obj;
  char *dat, c;
  struct irep *irep;
  struct proc *proc;
  type = load1(pic, buf, end);
  switch (type) {
  case 0x00:
    return pic_int_value(pic, load4(pic, buf, end));
  case 0x01:
    l = load4(pic, buf, end);
    dat = pic_malloc(pic, l + 1); /* TODO */
    loadn(pic, (unsigned char *) dat, l + 1, buf, end);
    obj = pic_str_value(pic, dat, l);
    pic_free(pic, dat);
    return obj;
  case 0x02:
    l = load4(pic, buf, end);
    dat = pic_malloc(pic, l + 1); /* TODO */
    loadn(pic, (unsigned char *) dat, l + 1, buf, end);
    obj = pic_intern_str(pic, dat, l);
    pic_free(pic, dat);
    return obj;
  case 0x03:
    irep = load_irep(pic, buf, end);
    proc = (struct proc *)pic_obj_alloc(pic, PIC_TYPE_PROC_IREP);
    proc->u.irep = irep;
    proc->env = NULL;
    return obj_value(pic, proc);
  case 0x04:
    c = load1(pic, buf, end);
    return pic_char_value(pic, c);
  default:
    pic_error(pic, "load: unsupported object", 1, pic_int_value(pic, type));
  }
}

pic_value
pic_deserialize(pic_state *pic, pic_value blob)
{
  int len;
  const unsigned char *buf = pic_blob(pic, blob, &len);
  return load_obj(pic, &buf, buf + len);
}

pic_value
pic_blob_value(pic_state *pic, const unsigned char *buf, int len)
{
  struct blob *bv;

  bv = (struct blob *)pic_obj_alloc(pic, PIC_TYPE_BLOB);
  bv->data = pic_malloc(pic, len);
  bv->len = len;
  if (buf) {
    memcpy(bv->data, buf, len);
  }
  return obj_value(pic, bv);
}

unsigned char *
pic_blob(pic_state *pic, pic_value blob, int *len)
{
  struct blob *bv = blob_ptr(pic, blob);
  if (len) {
    *len = bv->len;
  }
  return bv->data;
}

static pic_value
pic_blob_bytevector_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_blob_p(pic, v));
}

static pic_value
pic_blob_bytevector(pic_state *pic)
{
  pic_value *argv, blob;
  int argc, i;
  unsigned char *data;

  pic_get_args(pic, "*", &argc, &argv);

  blob = pic_blob_value(pic, 0, argc);

  data = pic_blob(pic, blob, NULL);

  for (i = 0; i < argc; ++i) {
    TYPE_CHECK(pic, argv[i], int);

    if (pic_int(pic, argv[i]) < 0 || pic_int(pic, argv[i]) > 255) {
      pic_error(pic, "byte out of range", 1, argv[i]);
    }

    *data++ = (unsigned char)pic_int(pic, argv[i]);
  }

  return blob;
}

static pic_value
pic_blob_make_bytevector(pic_state *pic)
{
  pic_value blob;
  int k, b = 0;

  pic_get_args(pic, "i|i", &k, &b);

  if (b < 0 || b > 255)
    pic_error(pic, "byte out of range", 1, pic_int_value(pic, b));

  if (k < 0) {
    pic_error(pic, "make-bytevector: negative length given", 1, pic_int_value(pic, k));
  }

  blob = pic_blob_value(pic, 0, k);

  memset(pic_blob(pic, blob, NULL), (unsigned char)b, k);

  return blob;
}

static pic_value
pic_blob_bytevector_length(pic_state *pic)
{
  int len;

  pic_get_args(pic, "b", NULL, &len);

  return pic_int_value(pic, len);
}

static pic_value
pic_blob_bytevector_u8_ref(pic_state *pic)
{
  unsigned char *buf;
  int len, k;

  pic_get_args(pic, "bi", &buf, &len, &k);

  VALID_INDEX(pic, len, k);

  return pic_int_value(pic, buf[k]);
}

static pic_value
pic_blob_bytevector_u8_set(pic_state *pic)
{
  unsigned char *buf;
  int len, k, v;

  pic_get_args(pic, "bii", &buf, &len, &k, &v);

  if (v < 0 || v > 255)
    pic_error(pic, "byte out of range", 1, pic_int_value(pic, v));

  VALID_INDEX(pic, len, k);

  buf[k] = (unsigned char)v;

  return pic_undef_value(pic);
}

static pic_value
pic_blob_bytevector_copy_i(pic_state *pic)
{
  unsigned char *to, *from;
  int n, at, start, end, tolen, fromlen;

  n = pic_get_args(pic, "bib|ii", &to, &tolen, &at, &from, &fromlen, &start, &end);

  switch (n) {
  case 3:
    start = 0;
  case 4:
    end = fromlen;
  }

  VALID_ATRANGE(pic, tolen, at, fromlen, start, end);

  memmove(to + at, from + start, end - start);

  return pic_undef_value(pic);
}

static pic_value
pic_blob_bytevector_copy(pic_state *pic)
{
  unsigned char *buf;
  int n, start, end, len;

  n = pic_get_args(pic, "b|ii", &buf, &len, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  return pic_blob_value(pic, buf + start, end - start);
}

static pic_value
pic_blob_bytevector_append(pic_state *pic)
{
  int argc, i, l, len;
  unsigned char *buf, *dst;
  pic_value *argv, blob;

  pic_get_args(pic, "*", &argc, &argv);

  len = 0;
  for (i = 0; i < argc; ++i) {
    TYPE_CHECK(pic, argv[i], blob);
    pic_blob(pic, argv[i], &l);
    len += l;
  }

  blob = pic_blob_value(pic, NULL, len);

  dst = pic_blob(pic, blob, NULL);
  len = 0;
  for (i = 0; i < argc; ++i) {
    buf = pic_blob(pic, argv[i], &l);
    memcpy(dst + len, buf, l);
    len += l;
  }

  return blob;
}

static pic_value
pic_blob_list_to_bytevector(pic_state *pic)
{
  pic_value blob;
  unsigned char *data;
  pic_value list, e, it;

  pic_get_args(pic, "o", &list);

  blob = pic_blob_value(pic, 0, pic_length(pic, list));

  data = pic_blob(pic, blob, NULL);

  pic_for_each (e, list, it) {
    TYPE_CHECK(pic, e, int);

    if (pic_int(pic, e) < 0 || pic_int(pic, e) > 255)
      pic_error(pic, "byte out of range", 0);

    *data++ = (unsigned char)pic_int(pic, e);
  }
  return blob;
}

static pic_value
pic_blob_bytevector_to_list(pic_state *pic)
{
  pic_value list;
  unsigned char *buf;
  int n, len, start, end, i;

  n = pic_get_args(pic, "b|ii", &buf, &len, &start, &end);

  switch (n) {
  case 1:
    start = 0;
  case 2:
    end = len;
  }

  VALID_RANGE(pic, len, start, end);

  list = pic_nil_value(pic);
  for (i = start; i < end; ++i) {
    pic_push(pic, pic_int_value(pic, buf[i]), list);
  }
  return pic_reverse(pic, list);
}

static pic_value
pic_blob_object_to_bytevector(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_serialize(pic, obj);
}

static pic_value
pic_blob_bytevector_to_object(pic_state *pic)
{
  pic_value blob;

  pic_get_args(pic, "o", &blob);

  TYPE_CHECK(pic, blob, blob);

  return pic_deserialize(pic, blob);
}

void
pic_init_blob(pic_state *pic)
{
  pic_defun(pic, "bytevector?", pic_blob_bytevector_p);
  pic_defun(pic, "bytevector", pic_blob_bytevector);
  pic_defun(pic, "make-bytevector", pic_blob_make_bytevector);
  pic_defun(pic, "bytevector-length", pic_blob_bytevector_length);
  pic_defun(pic, "bytevector-u8-ref", pic_blob_bytevector_u8_ref);
  pic_defun(pic, "bytevector-u8-set!", pic_blob_bytevector_u8_set);
  pic_defun(pic, "bytevector-copy!", pic_blob_bytevector_copy_i);
  pic_defun(pic, "bytevector-copy", pic_blob_bytevector_copy);
  pic_defun(pic, "bytevector-append", pic_blob_bytevector_append);
  pic_defun(pic, "bytevector->list", pic_blob_bytevector_to_list);
  pic_defun(pic, "list->bytevector", pic_blob_list_to_bytevector);
  pic_defun(pic, "bytevector->object", pic_blob_bytevector_to_object);
  pic_defun(pic, "object->bytevector", pic_blob_object_to_bytevector);
}
