#include <stdio.h>
#include <stdlib.h>

#include "picrin.h"
#include "picrin/proc.h"

void
pic_debug(pic_state *pic, pic_value obj)
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
    pic_debug(pic, pic_car(pic, obj));
    printf(" . ");
    pic_debug(pic, pic_cdr(pic, obj));
    printf(")");
    break;
  case PIC_TT_SYMBOL:
    printf("%s", pic_symbol_ptr(obj)->name);
    break;
  case PIC_TT_FLOAT:
    printf("%g", pic_float(obj));
    break;
  case PIC_TT_UNDEF:
    printf("#<undef>");
    break;
  case PIC_TT_PROC:
    printf("#<proc %p>", pic_proc_ptr(obj));
    break;
  }
}

static pic_value
pic_port_write(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);
  pic_debug(pic, v);
  return pic_undef_value();
}

static pic_value
pic_port_newline(pic_state *pic)
{
  puts("");
  return pic_undef_value();
}

void
pic_init_port(pic_state *pic)
{
  pic_defun(pic, "write", pic_port_write);
  pic_defun(pic, "newline", pic_port_newline);
}

