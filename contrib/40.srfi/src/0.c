#include "picrin.h"

void
pic_init_srfi_0(pic_state *pic)
{
    pic_add_feature(pic, "srfi-0");
    pic_add_feature(pic, "srfi-1");
    pic_add_feature(pic, "srfi-8");
    pic_add_feature(pic, "srfi-17");
    pic_add_feature(pic, "srfi-26");
    pic_add_feature(pic, "srfi-43");
    pic_add_feature(pic, "srfi-60");
    pic_add_feature(pic, "srfi-95");
    pic_add_feature(pic, "srfi-106");
    pic_add_feature(pic, "srfi-111");
}
