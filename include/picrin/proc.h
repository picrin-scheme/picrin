/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_PROC_H__
#define PICRIN_PROC_H__

#if defined(__cplusplus)
extern "C" {
#endif

/* native C function */
struct pic_func {
  pic_func_t f;
  pic_sym name;
};

struct pic_env {
  PIC_OBJECT_HEADER
  pic_value *regs;
  int regc;
  struct pic_env *up;
  pic_value storage[];
};

struct pic_proc {
  PIC_OBJECT_HEADER
  char kind;
  union {
    struct pic_func func;
    struct pic_irep *irep;
  } u;
  struct pic_env *env;
};

#define PIC_PROC_KIND_FUNC 1
#define PIC_PROC_KIND_IREP 2

#define pic_proc_func_p(proc) ((proc)->kind == PIC_PROC_KIND_FUNC)
#define pic_proc_irep_p(proc) ((proc)->kind == PIC_PROC_KIND_IREP)

#define pic_proc_p(o) (pic_type(o) == PIC_TT_PROC)
#define pic_proc_ptr(o) ((struct pic_proc *)pic_ptr(o))

#define pic_env_p(o) (pic_type(o) == PIC_TT_ENV)
#define pic_env_ptr(o) ((struct pic_env *)pic_ptr(o))

struct pic_proc *pic_proc_new(pic_state *, pic_func_t, const char *);
struct pic_proc *pic_proc_new_irep(pic_state *, struct pic_irep *, struct pic_env *);

pic_sym pic_proc_name(struct pic_proc *);

/* closed variables accessor */
void pic_proc_cv_init(pic_state *, struct pic_proc *, size_t);
int pic_proc_cv_size(pic_state *, struct pic_proc *);
pic_value pic_proc_cv_ref(pic_state *, struct pic_proc *, size_t);
void pic_proc_cv_set(pic_state *, struct pic_proc *, size_t, pic_value);

struct pic_proc *pic_papply(pic_state *, struct pic_proc *, pic_value);

#if defined(__cplusplus)
}
#endif

#endif
