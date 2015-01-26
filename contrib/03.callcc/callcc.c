#include "picrin.h"
#include "picrin/data.h"
#include "picrin/proc.h"
#include "picrin/pair.h"
#include "picrin/cont.h"

struct pic_cont {
  jmp_buf jmp;

  struct pic_winder *wind;

  char *stk_pos, *stk_ptr;
  ptrdiff_t stk_len;

  pic_value *st_ptr;
  size_t sp_offset, st_len;

  pic_callinfo *ci_ptr;
  size_t ci_offset, ci_len;

  struct pic_proc **xp_ptr;
  size_t xp_offset, xp_len;

  pic_code *ip;

  struct pic_object **arena;
  size_t arena_size;
  int arena_idx;

  pic_value results;
};

static void
cont_dtor(pic_state *pic, void *data)
{
  struct pic_cont *cont = data;

  pic_free(pic, cont->stk_ptr);
  pic_free(pic, cont->st_ptr);
  pic_free(pic, cont->ci_ptr);
  pic_free(pic, cont->xp_ptr);
  pic_free(pic, cont->arena);
  pic_free(pic, cont);
}

static void
cont_mark(pic_state *pic, void *data, void (*mark)(pic_state *, pic_value))
{
  struct pic_cont *cont = data;
  struct pic_winder *wind;
  pic_value *stack;
  pic_callinfo *ci;
  struct pic_proc **xp;
  size_t i;

  /* winder */
  for (wind = cont->wind; wind != NULL; wind = wind->prev) {
    if (wind->in) {
      mark(pic, pic_obj_value(wind->in));
    }
    if (wind->out) {
      mark(pic, pic_obj_value(wind->out));
    }
  }

  /* stack */
  for (stack = cont->st_ptr; stack != cont->st_ptr + cont->sp_offset; ++stack) {
    mark(pic, *stack);
  }

  /* callinfo */
  for (ci = cont->ci_ptr + cont->ci_offset; ci != cont->ci_ptr; --ci) {
    if (ci->env) {
      mark(pic, pic_obj_value(ci->env));
    }
  }

  /* exception handlers */
  for (xp = cont->xp_ptr; xp != cont->xp_ptr + cont->xp_offset; ++xp) {
    mark(pic, pic_obj_value(*xp));
  }

  /* arena */
  for (i = 0; i < (size_t)cont->arena_idx; ++i) {
    mark(pic, pic_obj_value(cont->arena[i]));
  }

  /* result values */
  mark(pic, cont->results);
}

static const pic_data_type cont_type = { "continuation", cont_dtor, cont_mark };

static void save_cont(pic_state *, struct pic_cont **);
static void restore_cont(pic_state *, struct pic_cont *);

static ptrdiff_t
native_stack_length(pic_state *pic, char **pos)
{
  char t;

  *pos = (pic->native_stack_start > &t)
    ? &t
    : pic->native_stack_start;

  return (pic->native_stack_start > &t)
    ? pic->native_stack_start - &t
    : &t - pic->native_stack_start;
}

static void
save_cont(pic_state *pic, struct pic_cont **c)
{
  void pic_vm_tear_off(pic_state *);
  struct pic_cont *cont;
  char *pos;

  pic_vm_tear_off(pic);         /* tear off */

  cont = *c = pic_alloc(pic, sizeof(struct pic_cont));

  cont->wind = pic->wind;

  cont->stk_len = native_stack_length(pic, &pos);
  cont->stk_pos = pos;
  assert(cont->stk_len > 0);
  cont->stk_ptr = pic_alloc(pic, cont->stk_len);
  memcpy(cont->stk_ptr, cont->stk_pos, cont->stk_len);

  cont->sp_offset = pic->sp - pic->stbase;
  cont->st_len = pic->stend - pic->stbase;
  cont->st_ptr = pic_alloc(pic, sizeof(pic_value) * cont->st_len);
  memcpy(cont->st_ptr, pic->stbase, sizeof(pic_value) * cont->st_len);

  cont->ci_offset = pic->ci - pic->cibase;
  cont->ci_len = pic->ciend - pic->cibase;
  cont->ci_ptr = pic_alloc(pic, sizeof(pic_callinfo) * cont->ci_len);
  memcpy(cont->ci_ptr, pic->cibase, sizeof(pic_callinfo) * cont->ci_len);

  cont->xp_offset = pic->xp - pic->xpbase;
  cont->xp_len = pic->xpend - pic->xpbase;
  cont->xp_ptr = pic_alloc(pic, sizeof(struct pic_proc *) * cont->xp_len);
  memcpy(cont->xp_ptr, pic->xpbase, sizeof(struct pic_proc *) * cont->xp_len);

  cont->ip = pic->ip;

  cont->arena_idx = pic->arena_idx;
  cont->arena_size = pic->arena_size;
  cont->arena = pic_alloc(pic, sizeof(struct pic_object *) * pic->arena_size);
  memcpy(cont->arena, pic->arena, sizeof(struct pic_object *) * pic->arena_size);

  cont->results = pic_undef_value();
}

static void
native_stack_extend(pic_state *pic, struct pic_cont *cont)
{
  volatile pic_value v[1024];

  ((void)v);
  restore_cont(pic, cont);
}

PIC_NORETURN static void
restore_cont(pic_state *pic, struct pic_cont *cont)
{
  char v;
  struct pic_cont *tmp = cont;

  if (&v < pic->native_stack_start) {
    if (&v > cont->stk_pos) native_stack_extend(pic, cont);
  }
  else {
    if (&v > cont->stk_pos + cont->stk_len) native_stack_extend(pic, cont);
  }

  pic->wind = cont->wind;

  pic->stbase = pic_realloc(pic, pic->stbase, sizeof(pic_value) * cont->st_len);
  memcpy(pic->stbase, cont->st_ptr, sizeof(pic_value) * cont->st_len);
  pic->sp = pic->stbase + cont->sp_offset;
  pic->stend = pic->stbase + cont->st_len;

  pic->cibase = pic_realloc(pic, pic->cibase, sizeof(pic_callinfo) * cont->ci_len);
  memcpy(pic->cibase, cont->ci_ptr, sizeof(pic_callinfo) * cont->ci_len);
  pic->ci = pic->cibase + cont->ci_offset;
  pic->ciend = pic->cibase + cont->ci_len;

  pic->xpbase = pic_realloc(pic, pic->xpbase, sizeof(struct pic_proc *) * cont->xp_len);
  memcpy(pic->xpbase, cont->xp_ptr, sizeof(struct pic_proc *) * cont->xp_len);
  pic->xp = pic->xpbase + cont->xp_offset;
  pic->xpend = pic->xpbase + cont->xp_len;

  pic->ip = cont->ip;

  pic->arena = pic_realloc(pic, pic->arena, sizeof(struct pic_object *) * cont->arena_size);
  memcpy(pic->arena, cont->arena, sizeof(struct pic_object *) * cont->arena_size);
  pic->arena_size = cont->arena_size;
  pic->arena_idx = cont->arena_idx;

  memcpy(cont->stk_pos, cont->stk_ptr, cont->stk_len);

  longjmp(tmp->jmp, 1);
}

PIC_NORETURN static pic_value
cont_call(pic_state *pic)
{
  struct pic_proc *proc;
  size_t argc;
  pic_value *argv;
  struct pic_cont *cont;

  proc = pic_get_proc(pic);
  pic_get_args(pic, "*", &argc, &argv);

  cont = pic_data_ptr(pic_attr_ref(pic, pic_obj_value(proc), "@@cont"))->data;
  cont->results = pic_list_by_array(pic, argc, argv);

  /* execute guard handlers */
  pic_wind(pic, pic->wind, cont->wind);

  restore_cont(pic, cont);
}

pic_value
pic_callcc(pic_state *pic, struct pic_proc *proc)
{
  struct pic_cont *cont;

  save_cont(pic, &cont);
  if (setjmp(cont->jmp)) {
    return pic_values_by_list(pic, cont->results);
  }
  else {
    struct pic_proc *c;
    struct pic_data *dat;

    c = pic_make_proc(pic, cont_call, "<continuation-procedure>");

    dat = pic_data_alloc(pic, &cont_type, cont);

    /* save the continuation object in proc */
    pic_attr_set(pic, pic_obj_value(c), "@@cont", pic_obj_value(dat));

    return pic_apply1(pic, proc, pic_obj_value(c));
  }
}

static pic_value
pic_callcc_trampoline(pic_state *pic, struct pic_proc *proc)
{
  struct pic_cont *cont;

  save_cont(pic, &cont);
  if (setjmp(cont->jmp)) {
    return pic_values_by_list(pic, cont->results);
  }
  else {
    struct pic_proc *c;
    struct pic_data *dat;

    c = pic_make_proc(pic, cont_call, "<continuation-procedure>");

    dat = pic_data_alloc(pic, &cont_type, cont);

    /* save the continuation object in proc */
    pic_attr_set(pic, pic_obj_value(c), "@@cont", pic_obj_value(dat));

    return pic_apply_trampoline(pic, proc, pic_list1(pic, pic_obj_value(c)));
  }
}

static pic_value
pic_callcc_callcc(pic_state *pic)
{
  struct pic_proc *cb;

  pic_get_args(pic, "l", &cb);

  return pic_callcc_trampoline(pic, cb);
}

#define pic_redefun(pic, lib, name, func)       \
  pic_set(pic, lib, name, pic_obj_value(pic_make_proc(pic, func, name)))

void
pic_init_callcc(pic_state *pic)
{
  pic_deflibrary (pic, "(scheme base)") {
    pic_redefun(pic, pic->PICRIN_BASE, "call-with-current-continuation", pic_callcc_callcc);
    pic_redefun(pic, pic->PICRIN_BASE, "call/cc", pic_callcc_callcc);
  }
}
