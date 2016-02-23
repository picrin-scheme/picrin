/**
 * See Copyright Notice in picrin.h
 */

#include <time.h>

#include "picrin.h"
#include "picrin/extra.h"

#define UTC_TAI_DIFF 35

static pic_value
pic_current_second(pic_state *pic)
{
  time_t t;

  pic_get_args(pic, "");

  time(&t);
  return pic_float_value(pic, (double)t + UTC_TAI_DIFF);
}

static pic_value
pic_current_jiffy(pic_state *pic)
{
  clock_t c;

  pic_get_args(pic, "");

  c = clock();
  return pic_int_value(pic, (int)c); /* The year 2038 problem :-| */
}

static pic_value
pic_jiffies_per_second(pic_state *pic)
{
  pic_get_args(pic, "");

  return pic_int_value(pic, CLOCKS_PER_SEC);
}

void
pic_init_time(pic_state *pic)
{
  pic_deflibrary(pic, "scheme.time");

  pic_defun(pic, "current-second", pic_current_second);
  pic_defun(pic, "current-jiffy", pic_current_jiffy);
  pic_defun(pic, "jiffies-per-second", pic_jiffies_per_second);
}
