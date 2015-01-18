/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/lib.h"
#include "picrin/macro.h"
#include "picrin/error.h"

void
pic_add_feature(pic_state *pic, const char *feature)
{
  pic_push(pic, pic_sym_value(pic_intern_cstr(pic, feature)), pic->features);
}

void pic_init_bool(pic_state *);
void pic_init_pair(pic_state *);
void pic_init_port(pic_state *);
void pic_init_number(pic_state *);
void pic_init_proc(pic_state *);
void pic_init_symbol(pic_state *);
void pic_init_vector(pic_state *);
void pic_init_blob(pic_state *);
void pic_init_cont(pic_state *);
void pic_init_char(pic_state *);
void pic_init_error(pic_state *);
void pic_init_str(pic_state *);
void pic_init_macro(pic_state *);
void pic_init_var(pic_state *);
void pic_init_write(pic_state *);
void pic_init_read(pic_state *);
void pic_init_dict(pic_state *);
void pic_init_record(pic_state *);
void pic_init_eval(pic_state *);
void pic_init_lib(pic_state *);
void pic_init_attr(pic_state *);

extern const char pic_boot[];

static void
pic_init_features(pic_state *pic)
{
  pic_add_feature(pic, "picrin");
  pic_add_feature(pic, "ieee-float");

#if _POSIX_SOURCE
  pic_add_feature(pic, "posix");
#endif

#if _WIN32
  pic_add_feature(pic, "windows");
#endif

#if __unix__
  pic_add_feature(pic, "unix");
#endif
#if __gnu_linux__
  pic_add_feature(pic, "gnu-linux");
#endif
#if __FreeBSD__
  pic_add_feature(pic, "freebsd");
#endif

#if __i386__
  pic_add_feature(pic, "i386");
#elif __x86_64__
  pic_add_feature(pic, "x86-64");
#elif __ppc__
  pic_add_feature(pic, "ppc");
#elif __sparc__
  pic_add_feature(pic, "sparc");
#endif

#if __ILP32__
  pic_add_feature(pic, "ilp32");
#elif __LP64__
  pic_add_feature(pic, "lp64");
#endif

#if defined(__BYTE_ORDER__)
# if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  pic_add_feature(pic, "little-endian");
# elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  pic_add_feature(pic, "big-endian");
# endif
#else
# if __LITTLE_ENDIAN__
  pic_add_feature(pic, "little-endian");
# elif __BIG_ENDIAN__
  pic_add_feature(pic, "big-endian");
# endif
#endif
}

#define DONE pic_gc_arena_restore(pic, ai);

void
pic_init_core(pic_state *pic)
{
  size_t ai = pic_gc_arena_preserve(pic);

  pic_init_features(pic);

  pic_deflibrary (pic, "(picrin base)") {
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sDEFINE, pic->rDEFINE);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sSETBANG, pic->rSETBANG);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sQUOTE, pic->rQUOTE);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sLAMBDA, pic->rLAMBDA);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sIF, pic->rIF);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sBEGIN, pic->rBEGIN);
    pic_define_syntactic_keyword(pic, pic->lib->env, pic->sDEFINE_SYNTAX, pic->rDEFINE_SYNTAX);

    pic_init_bool(pic); DONE;
    pic_init_pair(pic); DONE;
    pic_init_port(pic); DONE;
    pic_init_number(pic); DONE;
    pic_init_proc(pic); DONE;
    pic_init_symbol(pic); DONE;
    pic_init_vector(pic); DONE;
    pic_init_blob(pic); DONE;
    pic_init_cont(pic); DONE;
    pic_init_char(pic); DONE;
    pic_init_error(pic); DONE;
    pic_init_str(pic); DONE;
    pic_init_macro(pic); DONE;
    pic_init_var(pic); DONE;
    pic_init_write(pic); DONE;
    pic_init_read(pic); DONE;
    pic_init_dict(pic); DONE;
    pic_init_record(pic); DONE;
    pic_init_eval(pic); DONE;
    pic_init_lib(pic); DONE;
    pic_init_attr(pic); DONE;

    pic_load_cstr(pic, pic_boot);
  }

  pic_import_library(pic, pic->PICRIN_BASE);
}
