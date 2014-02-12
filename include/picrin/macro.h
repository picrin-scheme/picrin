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
  xhash *name;
  struct pic_senv *up;
};

struct pic_syntax {
  PIC_OBJECT_HEADER
  struct pic_proc *proc;
  struct pic_senv *senv;
};

struct pic_sc {
  PIC_OBJECT_HEADER
  pic_value expr;
  struct pic_senv *senv;
};

#define pic_sc(v) ((struct pic_sc *)pic_ptr(v))
#define pic_sc_p(v) (pic_type(v) == PIC_TT_SC)

#define pic_syntax(v) ((struct pic_syntax *)pic_ptr(v))
#define pic_syntax_p(v) (pic_type(v) == PIC_TT_SYNTAX)

#define pic_senv(v) ((struct pic_senv *)pic_ptr(v))
#define pic_senv_p(v) (pic_type(v) == PIC_TT_SENV)
#define pic_senv_ptr(v) ((struct pic_senv *)pic_ptr(v))

struct pic_senv *pic_null_syntactic_env(pic_state *);
struct pic_senv *pic_minimal_syntactic_env(pic_state *);
struct pic_senv *pic_core_syntactic_env(pic_state *);

#if defined(__cplusplus)
}
#endif

#endif
