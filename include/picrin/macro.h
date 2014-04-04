/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_MACRO_H__
#define PICRIN_MACRO_H__

#if defined(__cplusplus)
extern "C" {
#endif

struct pic_senv {
  PIC_OBJECT_HEADER
  xhash renames;
  struct pic_senv *up;
};

struct pic_macro {
  PIC_OBJECT_HEADER
  struct pic_proc *proc;
  struct pic_senv *senv;
};

struct pic_sc {
  PIC_OBJECT_HEADER
  pic_value expr;
  struct pic_senv *senv;
};

#define pic_sc_p(v) (pic_type(v) == PIC_TT_SC)
#define pic_sc_ptr(v) ((struct pic_sc *)pic_ptr(v))

#define pic_macro_p(v) (pic_type(v) == PIC_TT_MACRO)
#define pic_macro_ptr(v) ((struct pic_macro *)pic_ptr(v))

#define pic_senv_p(v) (pic_type(v) == PIC_TT_SENV)
#define pic_senv_ptr(v) ((struct pic_senv *)pic_ptr(v))

struct pic_senv *pic_null_syntactic_environment(pic_state *);

pic_sym pic_add_rename(pic_state *, struct pic_senv *, pic_sym);
bool pic_find_rename(pic_state *, struct pic_senv *, pic_sym, pic_sym * /* = NULL */);
void pic_put_rename(pic_state *, struct pic_senv *, pic_sym, pic_sym);

void pic_define_syntactic_keyword(pic_state *, struct pic_senv *, pic_sym);

#if defined(__cplusplus)
}
#endif

#endif
