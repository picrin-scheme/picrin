#include <stdio.h>
#include <stdlib.h>

#include "picrin.h"
#include "picrin/proc.h"
#include "picrin/port.h"
#include "picrin/string.h"

static void write_pair(pic_state *pic, struct pic_pair *pair);
static void write_str(pic_state *pic, struct pic_string *str);

static void
write(pic_state *pic, pic_value obj)
{
  switch (pic_type(obj)) {
  case PIC_TT_NIL:
    printf("()");
    break;
  case PIC_TT_BOOL:
    if (pic_true_p(obj))
      printf("#t");
    else
      printf("#f");
    break;
  case PIC_TT_PAIR:
    printf("(");
    write_pair(pic, pic_pair_ptr(obj));
    printf(")");
    break;
  case PIC_TT_SYMBOL:
    printf("%s", pic_symbol_ptr(obj)->name);
    break;
  case PIC_TT_FLOAT:
    printf("%.10g", pic_float(obj));
    break;
  case PIC_TT_UNDEF:
    printf("#<undef>");
    break;
  case PIC_TT_PROC:
    printf("#<proc %p>", pic_proc_ptr(obj));
    break;
  case PIC_TT_PORT:
    printf("#<port %p>", pic_port_ptr(obj));
    break;
  case PIC_TT_STRING:
    printf("\"");
    write_str(pic, pic_str_ptr(obj));
    printf("\"");
    break;
  }
}

static void
write_pair(pic_state *pic, struct pic_pair *pair)
{
  write(pic, pair->car);

  if (pic_nil_p(pair->cdr)) {
    return;
  }
  if (pic_pair_p(pair->cdr)) {
    printf(" ");
    write_pair(pic, pic_pair_ptr(pair->cdr));
    return;
  }
  printf(" . ");
  write(pic, pair->cdr);
}

static void
write_str(pic_state *pic, struct pic_string *str)
{
  int i;
  const char *cstr = str->str;

  for (i = 0; i < str->len; ++i) {
    if (cstr[i] == '"' || cstr[i] == '\\') {
      putchar('\\');
    }
    putchar(cstr[i]);
  }
}

void
pic_debug(pic_state *pic, pic_value obj)
{
  write(pic, obj);
}

static pic_value
pic_port_write(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);
  write(pic, v);
  return pic_false_value();
}

static pic_value
pic_port_newline(pic_state *pic)
{
  puts("");
  return pic_false_value();
}

void
pic_init_port(pic_state *pic)
{
  pic_defun(pic, "write", pic_port_write);
  pic_defun(pic, "newline", pic_port_newline);
}

