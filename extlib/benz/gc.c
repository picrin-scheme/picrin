#include "picrin.h"

#ifdef PIC_USE_BITMAPGC
# include "./gc/bitmap.c"
#else
# include "./gc/markandsweep.c"
#endif
