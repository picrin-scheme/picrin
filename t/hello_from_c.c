#include <stdio.h>
#include "picrin.h"

static pic_value
pic_hello_hello(pic_state *pic)
{
  pic_get_args(pic, "");
  printf("Hello from C!\n");
  return pic_none_value();
}

static pic_value
pic_hello_hello_mr(pic_state *pic)
{
  char *name;

  pic_get_args(pic, "z", &name);
  printf("Hello Mr. %s!\n", name);
  return pic_none_value();
}

int
tak(int x, int y, int z)
{
  return  x > y ?
    tak(tak(x-1, y, z), tak(y-1, z, x), tak(z-1, x, y)) :
    y;
}


static pic_value
pic_c_tak(pic_state *pic)
{
  int x, y, z;
  pic_get_args(pic, "iii", &x, &y, &z);
  return pic_int_value(tak(x, y, z));
}
void
pic_init_hello(pic_state *pic)
{
  pic_defun(pic, "hello-mr", pic_hello_hello_mr);
  pic_defun(pic, "hello", pic_hello_hello);
  pic_deflibrary ("(scheme tak)") {
    pic_defun(pic, "c-tak", pic_c_tak);  
  }
}
