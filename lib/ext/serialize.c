/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "../object.h"
#include "../state.h"

static size_t offset = 0;

#define DUMP(c) do { printf("0x%02x, ", c); if (++offset == 12) { puts(""); offset = 0; } } while (0)

static void
dump1(pic_state *pic, unsigned char c)
{
  DUMP(c);
}

static void
dump4(pic_state *pic, unsigned long n)
{
  assert(sizeof(long) * CHAR_BIT <= 32 || n < (1ul << 32));

  dump1(pic, (n & 0xff));
  dump1(pic, (n & 0xff00) >> 8);
  dump1(pic, (n & 0xff0000) >> 16);
  dump1(pic, (n & 0xff000000) >> 24);
}

static void
dump_obj(pic_state *pic, pic_value obj)
{
  if (pic_int_p(pic, obj)) {
    dump1(pic, 0x00);
    dump4(pic, pic_int(pic, obj));
  } else if (pic_str_p(pic, obj)) {
    int len, i;
    const char *str = pic_str(pic, obj, &len);
    dump1(pic, 0x01);
    dump4(pic, len);
    for (i = 0; i < len; ++i) {
      dump1(pic, str[i]);
    }
    dump1(pic, 0);
  } else if (pic_sym_p(pic, obj)) {
    int len, i;
    const char *str = pic_str(pic, pic_sym_name(pic, obj), &len);
    dump1(pic, 0x02);
    dump4(pic, len);
    for (i = 0; i < len; ++i) {
      dump1(pic, str[i]);
    }
    dump1(pic, 0);
  } else {
    pic_error(pic, "dump: unsupported object", 1, obj);
  }
}

#define IREP_FLAGS_MASK (IREP_VARG)

static void
dump_irep(pic_state *pic, struct irep *irep)
{
  size_t i;
  dump1(pic, irep->argc);
  dump1(pic, irep->flags & IREP_FLAGS_MASK);
  dump1(pic, irep->frame_size);
  dump1(pic, irep->irepc);
  dump1(pic, irep->objc);
  dump4(pic, irep->codec);
  for (i = 0; i < irep->objc; ++i) {
    dump_obj(pic, irep->obj[i]);
  }
  for (i = 0; i < irep->codec; ++i) {
    dump1(pic, irep->code[i]);
  }
  for (i = 0; i < irep->irepc; ++i) {
    dump_irep(pic, irep->irep[i]);
  }
}

void
pic_serialize(pic_state *pic, const char *name, pic_value irep)
{
  offset = 0;
  printf("const unsigned char %s[] = {\n", name);
  dump_irep(pic, irep_ptr(pic, irep));
  if (offset != 0) puts("");
  printf("};\n");
}

const unsigned char *bin;

static unsigned char
load1(pic_state *pic)
{
  return *bin++;
}

static unsigned long
load4(pic_state *pic)
{
  unsigned long x = bin[0] + (bin[1] << 8) + (bin[2] << 16) + (bin[3] << 24);
  bin += 4;
  return x;
}

static pic_value
load_obj(pic_state *pic)
{
  int type, len;
  pic_value obj;
  type = load1(pic);
  switch (type) {
  case 0x00:
    return pic_int_value(pic, load4(pic));
  case 0x01:
    len = load4(pic);
    obj = pic_str_value(pic, bin, -len);
    bin += len + 1;
    return obj;
  case 0x02:
    len = load4(pic);
    obj = pic_str_value(pic, bin, -len);
    obj = pic_intern(pic, obj);
    bin += len + 1;
    return obj;
  default:
    pic_error(pic, "load: unsupported object", 1, pic_int_value(pic, type));
  }
}

static struct irep *
load_irep(pic_state *pic)
{
  unsigned char argc, flags, frame_size, irepc, objc;
  size_t codec, i;
  pic_value *obj;
  const unsigned char *code;
  struct irep **irep, *ir;
  argc = load1(pic);
  flags = load1(pic) | IREP_CODE_STATIC;
  frame_size = load1(pic);
  irepc = load1(pic);
  objc = load1(pic);
  codec = load4(pic);
  obj = pic_malloc(pic, sizeof(pic_value) * objc);
  for (i = 0; i < objc; ++i) {
    obj[i] = load_obj(pic);
  }
  code = bin;
  bin += codec;
  irep = pic_malloc(pic, sizeof(struct irep *) * irepc);
  for (i = 0; i < irepc; ++i) {
    irep[i] = load_irep(pic);
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
  return ir;
}

pic_value
pic_deserialize(pic_state *pic, const unsigned char *str)
{
  bin = str;
  return obj_value(pic, load_irep(pic));
}
