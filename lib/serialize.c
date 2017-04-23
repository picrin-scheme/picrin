/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"

static void
dump1(pic_state *pic, unsigned char c, pic_value port)
{
  pic_fputc(pic, c, port);
}

static void
dump4(pic_state *pic, unsigned long n, pic_value port)
{
  assert(sizeof(long) * CHAR_BIT <= 32 || n < (1ul << 32));

  dump1(pic, (n & 0xff), port);
  dump1(pic, (n & 0xff00) >> 8, port);
  dump1(pic, (n & 0xff0000) >> 16, port);
  dump1(pic, (n & 0xff000000) >> 24, port);
}

static void dump_obj(pic_state *pic, pic_value obj, pic_value port);

#define IREP_FLAGS_MASK (IREP_VARG)

static void
dump_irep(pic_state *pic, struct irep *irep, pic_value port)
{
  size_t i;
  dump1(pic, irep->argc, port);
  dump1(pic, irep->flags & IREP_FLAGS_MASK, port);
  dump1(pic, irep->frame_size, port);
  dump1(pic, irep->irepc, port);
  dump1(pic, irep->objc, port);
  dump4(pic, irep->codec, port);
  for (i = 0; i < irep->objc; ++i) {
    dump_obj(pic, irep->obj[i], port);
  }
  for (i = 0; i < irep->codec; ++i) {
    dump1(pic, irep->code[i], port);
  }
  for (i = 0; i < irep->irepc; ++i) {
    dump_irep(pic, irep->irep[i], port);
  }
}

static void
dump_obj(pic_state *pic, pic_value obj, pic_value port)
{
  if (pic_int_p(pic, obj)) {
    dump1(pic, 0x00, port);
    dump4(pic, pic_int(pic, obj), port);
  } else if (pic_str_p(pic, obj)) {
    int len, i;
    const char *str = pic_str(pic, obj, &len);
    dump1(pic, 0x01, port);
    dump4(pic, len, port);
    for (i = 0; i < len; ++i) {
      dump1(pic, str[i], port);
    }
    dump1(pic, 0, port);
  } else if (pic_sym_p(pic, obj)) {
    int len, i;
    const char *str = pic_str(pic, pic_sym_name(pic, obj), &len);
    dump1(pic, 0x02, port);
    dump4(pic, len, port);
    for (i = 0; i < len; ++i) {
      dump1(pic, str[i], port);
    }
    dump1(pic, 0, port);
  } else if (pic_proc_p(pic, obj)) {
    if (pic_proc_func_p(pic, obj)) {
      pic_error(pic, "dump: c function procedure serialization unsupported", 1, obj);
    }
    if (proc_ptr(pic, obj)->env) {
      pic_error(pic, "dump: local procedure serialization unsupported", 1, obj);
    }
    dump1(pic, 0x03, port);
    dump_irep(pic, proc_ptr(pic, obj)->u.irep, port);
  } else {
    pic_error(pic, "dump: unsupported object", 1, obj);
  }
}

pic_value
pic_serialize(pic_state *pic, pic_value obj)
{
  pic_value port = pic_funcall(pic, "open-output-bytevector", 0);
  pic_value blob;
  dump_obj(pic, obj, port);
  blob = pic_funcall(pic, "get-output-bytevector", 1, port);
  pic_fclose(pic, port);
  return blob;
}

static unsigned char
load1(pic_state *pic, pic_value port)
{
  return pic_fgetc(pic, port);
}

static unsigned long
load4(pic_state *pic, pic_value port)
{
  unsigned long x = load1(pic, port);
  x += load1(pic, port) << 8;
  x += load1(pic, port) << 16;
  x += load1(pic, port) << 24;
  return x;
}

static pic_value load_obj(pic_state *pic, pic_value port);

static struct irep *
load_irep(pic_state *pic, pic_value port)
{
  unsigned char argc, flags, frame_size, irepc, objc;
  size_t codec, i;
  pic_value *obj;
  unsigned char *code;
  struct irep **irep, *ir;
  size_t ai = pic_enter(pic);

  argc = load1(pic, port);
  flags = load1(pic, port);
  frame_size = load1(pic, port);
  irepc = load1(pic, port);
  objc = load1(pic, port);
  codec = load4(pic, port);
  obj = pic_malloc(pic, sizeof(pic_value) * objc);
  for (i = 0; i < objc; ++i) {
    obj[i] = load_obj(pic, port);
  }
  code = pic_malloc(pic, codec); /* TODO */
  pic_fread(pic, code, codec, 1, port);
  irep = pic_malloc(pic, sizeof(struct irep *) * irepc);
  for (i = 0; i < irepc; ++i) {
    irep[i] = load_irep(pic, port);
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
load_obj(pic_state *pic, pic_value port)
{
  int type, len;
  pic_value obj;
  char *buf;
  struct irep *irep;
  struct proc *proc;
  type = load1(pic, port);
  switch (type) {
  case 0x00:
    return pic_int_value(pic, load4(pic, port));
  case 0x01:
    len = load4(pic, port);
    buf = pic_malloc(pic, len + 1); /* TODO */
    pic_fread(pic, buf, len + 1, 1, port);
    obj = pic_str_value(pic, buf, len);
    pic_free(pic, buf);
    return obj;
  case 0x02:
    len = load4(pic, port);
    buf = pic_malloc(pic, len + 1); /* TODO */
    pic_fread(pic, buf, len + 1, 1, port);
    obj = pic_intern_str(pic, buf, len);
    pic_free(pic, buf);
    return obj;
  case 0x03:
    irep = load_irep(pic, port);
    proc = (struct proc *)pic_obj_alloc(pic, PIC_TYPE_PROC_IREP);
    proc->u.irep = irep;
    proc->env = NULL;
    return obj_value(pic, proc);
  default:
    pic_error(pic, "load: unsupported object", 1, pic_int_value(pic, type));
  }
}

pic_value
pic_deserialize(pic_state *pic, pic_value blob)
{
  pic_value port = pic_funcall(pic, "open-input-bytevector", 1, blob);
  pic_value obj = load_obj(pic, port);
  pic_fclose(pic, port);
  return obj;
}
