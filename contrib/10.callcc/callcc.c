#include "picrin.h"
#include "picrin/private/object.h"
#include "picrin/private/state.h"

struct fullcont {
  jmp_buf jmp;

  struct cont *prev_jmp;

  struct checkpoint *cp;

  char *stk_pos, *stk_ptr;
  ptrdiff_t stk_len;

  pic_value *st_ptr;
  size_t sp_offset;
  ptrdiff_t st_len;

  struct callinfo *ci_ptr;
  size_t ci_offset;
  ptrdiff_t ci_len;

  struct code *ip;

  struct object **arena;
  size_t arena_size, arena_idx;

  int retc;
  pic_value *retv;
};

static void
cont_dtor(pic_state *pic, void *data)
{
  struct fullcont *cont = data;

  pic_free(pic, cont->stk_ptr);
  pic_free(pic, cont->st_ptr);
  pic_free(pic, cont->ci_ptr);
  pic_free(pic, cont->arena);
  pic_free(pic, cont);
}

static void
cont_mark(pic_state *pic, void *data, void (*mark)(pic_state *, pic_value))
{
  struct fullcont *cont = data;
  struct checkpoint *cp;
  pic_value *stack;
  struct callinfo *ci;
  size_t i;

  /* checkpoint */
  for (cp = cont->cp; cp != NULL; cp = cp->prev) {
    if (cp->in) {
      mark(pic, pic_obj_value(cp->in));
    }
    if (cp->out) {
      mark(pic, pic_obj_value(cp->out));
    }
  }

  /* stack */
  for (stack = cont->st_ptr; stack != cont->st_ptr + cont->sp_offset; ++stack) {
    mark(pic, *stack);
  }

  /* callinfo */
  for (ci = cont->ci_ptr + cont->ci_offset; ci != cont->ci_ptr; --ci) {
    if (ci->cxt) {
      mark(pic, pic_obj_value(ci->cxt));
    }
  }

  /* arena */
  for (i = 0; i < cont->arena_idx; ++i) {
    mark(pic, pic_obj_value(cont->arena[i]));
  }
}

static const pic_data_type cont_type = { "continuation", cont_dtor, cont_mark };

static void save_cont(pic_state *, struct fullcont **);
static void restore_cont(pic_state *, struct fullcont *);

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
save_cont(pic_state *pic, struct fullcont **c)
{
  void pic_vm_tear_off(pic_state *);
  struct fullcont *cont;
  char *pos;

  pic_vm_tear_off(pic);         /* tear off */

  cont = *c = pic_malloc(pic, sizeof(struct fullcont));

  cont->prev_jmp = pic->cc;

  cont->cp = pic->cp;

  cont->stk_len = native_stack_length(pic, &pos);
  cont->stk_pos = pos;
  assert(cont->stk_len > 0);
  cont->stk_ptr = pic_malloc(pic, cont->stk_len);
  memcpy(cont->stk_ptr, cont->stk_pos, cont->stk_len);

  cont->sp_offset = pic->sp - pic->stbase;
  cont->st_len = pic->stend - pic->stbase;
  cont->st_ptr = pic_malloc(pic, sizeof(pic_value) * cont->st_len);
  memcpy(cont->st_ptr, pic->stbase, sizeof(pic_value) * cont->st_len);

  cont->ci_offset = pic->ci - pic->cibase;
  cont->ci_len = pic->ciend - pic->cibase;
  cont->ci_ptr = pic_malloc(pic, sizeof(struct callinfo) * cont->ci_len);
  memcpy(cont->ci_ptr, pic->cibase, sizeof(struct callinfo) * cont->ci_len);

  cont->ip = pic->ip;

  cont->arena_idx = pic->arena_idx;
  cont->arena_size = pic->arena_size;
  cont->arena = pic_malloc(pic, sizeof(struct object *) * pic->arena_size);
  memcpy(cont->arena, pic->arena, sizeof(struct object *) * pic->arena_size);

  cont->retc = 0;
  cont->retv = NULL;
}

static void
native_stack_extend(pic_state *pic, struct fullcont *cont)
{
  volatile pic_value v[1024];

  ((void)v);
  restore_cont(pic, cont);
}

PIC_NORETURN static void
restore_cont(pic_state *pic, struct fullcont *cont)
{
  char v;
  struct fullcont *tmp = cont;

  if (&v < pic->native_stack_start) {
    if (&v > cont->stk_pos) native_stack_extend(pic, cont);
  }
  else {
    if (&v < cont->stk_pos + cont->stk_len) native_stack_extend(pic, cont);
  }

  pic->cc = cont->prev_jmp;
  pic->cp = cont->cp;

  assert(pic->stend - pic->stbase >= cont->st_len);
  memcpy(pic->stbase, cont->st_ptr, sizeof(pic_value) * cont->st_len);
  pic->sp = pic->stbase + cont->sp_offset;
  pic->stend = pic->stbase + cont->st_len;

  assert(pic->ciend - pic->cibase >= cont->ci_len);
  memcpy(pic->cibase, cont->ci_ptr, sizeof(struct callinfo) * cont->ci_len);
  pic->ci = pic->cibase + cont->ci_offset;
  pic->ciend = pic->cibase + cont->ci_len;

  pic->ip = cont->ip;

  assert(pic->arena_size >= cont->arena_size);
  memcpy(pic->arena, cont->arena, sizeof(struct object *) * cont->arena_size);
  pic->arena_size = cont->arena_size;
  pic->arena_idx = cont->arena_idx;

  memcpy(cont->stk_pos, cont->stk_ptr, cont->stk_len);

  longjmp(tmp->jmp, 1);
}

PIC_NORETURN static pic_value
cont_call(pic_state *pic)
{
  int argc, i;
  pic_value *argv, *retv;
  struct fullcont *cont;

  pic_get_args(pic, "*", &argc, &argv);

  retv = pic_alloca(pic, sizeof(pic_value) * argc);
  for (i = 0; i < argc; ++i) {
    retv[i] = argv[i];
  }

  cont = pic_data(pic, pic_closure_ref(pic, 0));
  cont->retc = argc;
  cont->retv = retv;

  /* execute guard handlers */
  pic_wind(pic, pic->cp, cont->cp);

  restore_cont(pic, cont);
}

static pic_value
pic_callcc(pic_state *pic, pic_value proc)
{
  struct fullcont *cont;

  save_cont(pic, &cont);
  if (setjmp(cont->jmp)) {
    return pic_valuesk(pic, cont->retc, cont->retv);
  }
  else {
    pic_value c, args[1];

    /* save the continuation object in proc */
    c = pic_lambda(pic, cont_call, 1, pic_data_value(pic, cont, &cont_type));

    args[0] = c;
    return pic_applyk(pic, proc, 1, args);
  }
}

static pic_value
pic_callcc_callcc(pic_state *pic)
{
  pic_value proc;

  pic_get_args(pic, "l", &proc);

  return pic_callcc(pic, proc);
}

#define pic_redefun(pic, lib, name, func)               \
  pic_set(pic, lib, name, pic_lambda(pic, func, 0))

void
pic_init_callcc(pic_state *pic)
{
  pic_redefun(pic, "picrin.base", "call-with-current-continuation", pic_callcc_callcc);
  pic_redefun(pic, "picrin.base", "call/cc", pic_callcc_callcc);
}
