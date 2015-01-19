/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/vector.h"
#include "picrin/proc.h"
#include "picrin/port.h"
#include "picrin/irep.h"
#include "picrin/blob.h"
#include "picrin/lib.h"
#include "picrin/macro.h"
#include "picrin/error.h"
#include "picrin/dict.h"
#include "picrin/record.h"
#include "picrin/symbol.h"

#define GET_OPERAND(pic,n) ((pic)->ci->fp[(n)])

struct pic_proc *
pic_get_proc(pic_state *pic)
{
  pic_value v = GET_OPERAND(pic,0);

  if (! pic_proc_p(v)) {
    pic_errorf(pic, "fatal error");
  }
  return pic_proc_ptr(v);
}

/**
 * char type                    desc.
 * ---- ----                    ----
 *  o   pic_value *             object
 *  i   int *                   int
 *  I   int *, bool *           int with exactness
 *  k   size_t *                size_t implicitly converted from int
 *  f   double *                float
 *  F   double *, bool *        float with exactness
 *  s   pic_str **              string object
 *  z   char **                 c string
 *  m   pic_sym *               symbol
 *  v   pic_vec **              vector object
 *  b   pic_blob **             bytevector object
 *  c   char *                  char
 *  l   struct pic_proc **      lambda object
 *  p   struct pic_port **      port object
 *  d   struct pic_dict **      dictionary object
 *  e   struct pic_error **     error object
 *
 *  |                           optional operator
 *  *   size_t *, pic_value **  variable length operator
 */

int
pic_get_args(pic_state *pic, const char *format, ...)
{
  char c;
  int i = 1, argc = pic->ci->argc;
  va_list ap;
  bool opt = false;

  va_start(ap, format);
  while ((c = *format++)) {
    switch (c) {
    default:
      if (argc <= i && ! opt) {
	pic_errorf(pic, "wrong number of arguments");
      }
      break;
    case '|':
      break;
    case '*':
      break;
    }

    /* in order to run out of all arguments passed to this function
       (i.e. do va_arg for each argument), optional argument existence
       check is done in every case closure */

    if (c == '*')
      break;

    switch (c) {
    case '|':
      opt = true;
      break;
    case 'o': {
      pic_value *p;

      p = va_arg(ap, pic_value*);
      if (i < argc) {
        *p = GET_OPERAND(pic,i);
        i++;
      }
      break;
    }
    case 'f': {
      double *f;

      f = va_arg(ap, double *);
      if (i < argc) {
        pic_value v;

        v = GET_OPERAND(pic, i);
        switch (pic_type(v)) {
        case PIC_TT_FLOAT:
          *f = pic_float(v);
          break;
        case PIC_TT_INT:
          *f = pic_int(v);
          break;
        default:
          pic_errorf(pic, "pic_get_args: expected float or int, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'F': {
      double *f;
      bool *e;

      f = va_arg(ap, double *);
      e = va_arg(ap, bool *);
      if (i < argc) {
        pic_value v;

        v = GET_OPERAND(pic, i);
        switch (pic_type(v)) {
        case PIC_TT_FLOAT:
          *f = pic_float(v);
          *e = false;
          break;
        case PIC_TT_INT:
          *f = pic_int(v);
          *e = true;
          break;
        default:
          pic_errorf(pic, "pic_get_args: expected float or int, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'I': {
      int *k;
      bool *e;

      k = va_arg(ap, int *);
      e = va_arg(ap, bool *);
      if (i < argc) {
        pic_value v;

        v = GET_OPERAND(pic, i);
        switch (pic_type(v)) {
        case PIC_TT_FLOAT:
          *k = (int)pic_float(v);
          *e = false;
          break;
        case PIC_TT_INT:
          *k = pic_int(v);
          *e = true;
          break;
        default:
          pic_errorf(pic, "pic_get_args: expected float or int, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'i': {
      int *k;

      k = va_arg(ap, int *);
      if (i < argc) {
        pic_value v;

        v = GET_OPERAND(pic, i);
        switch (pic_type(v)) {
        case PIC_TT_FLOAT:
          *k = (int)pic_float(v);
          break;
        case PIC_TT_INT:
          *k = pic_int(v);
          break;
        default:
          pic_errorf(pic, "pic_get_args: expected int, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'k': {
      size_t *k;

      k = va_arg(ap, size_t *);
      if (i < argc) {
        pic_value v;
        int x;

        v = GET_OPERAND(pic, i);
        switch (pic_type(v)) {
        case PIC_TT_INT:
          x = pic_int(v);
          if (x < 0) {
            pic_errorf(pic, "pic_get_args: expected non-negative int, but got ~s", v);
          }
          if (sizeof(unsigned) > sizeof(size_t)) {
            if ((unsigned)x > (unsigned)SIZE_MAX) {
              pic_errorf(pic, "pic_get_args: int unrepresentable with size_t ~s", v);
            }
          }
          *k = (size_t)x;
          break;
        default:
          pic_errorf(pic, "pic_get_args: expected int, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 's': {
      pic_str **str;
      pic_value v;

      str = va_arg(ap, pic_str **);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (pic_str_p(v)) {
          *str = pic_str_ptr(v);
        }
        else {
          pic_errorf(pic, "pic_get_args: expected string, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'z': {
      const char **cstr;
      pic_value v;

      cstr = va_arg(ap, const char **);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (! pic_str_p(v)) {
          pic_errorf(pic, "pic_get_args: expected string, but got ~s", v);
        }
        *cstr = pic_str_cstr(pic_str_ptr(v));
        i++;
      }
      break;
    }
    case 'm': {
      pic_sym *m;
      pic_value v;

      m = va_arg(ap, pic_sym *);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (pic_sym_p(v)) {
          *m = pic_sym(v);
        }
        else {
          pic_errorf(pic, "pic_get_args: expected symbol, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'v': {
      struct pic_vector **vec;
      pic_value v;

      vec = va_arg(ap, struct pic_vector **);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (pic_vec_p(v)) {
          *vec = pic_vec_ptr(v);
        }
        else {
          pic_errorf(pic, "pic_get_args: expected vector, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'b': {
      struct pic_blob **b;
      pic_value v;

      b = va_arg(ap, struct pic_blob **);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (pic_blob_p(v)) {
          *b = pic_blob_ptr(v);
        }
        else {
          pic_errorf(pic, "pic_get_args: expected bytevector, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'c': {
      char *k;
      pic_value v;

      k = va_arg(ap, char *);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (pic_char_p(v)) {
          *k = pic_char(v);
        }
        else {
          pic_errorf(pic, "pic_get_args: expected char, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'l': {
      struct pic_proc **l;
      pic_value v;

      l = va_arg(ap, struct pic_proc **);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (pic_proc_p(v)) {
          *l = pic_proc_ptr(v);
        }
        else {
          pic_errorf(pic, "pic_get_args, expected procedure, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'p': {
      struct pic_port **p;
      pic_value v;

      p = va_arg(ap, struct pic_port **);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (pic_port_p(v)) {
          *p = pic_port_ptr(v);
        }
        else {
          pic_errorf(pic, "pic_get_args, expected port, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'd': {
      struct pic_dict **d;
      pic_value v;

      d = va_arg(ap, struct pic_dict **);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (pic_dict_p(v)) {
          *d = pic_dict_ptr(v);
        }
        else {
          pic_errorf(pic, "pic_get_args, expected dictionary, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'r': {
      struct pic_record **r;
      pic_value v;

      r = va_arg(ap, struct pic_record **);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (pic_record_p(v)) {
          *r = pic_record_ptr(v);
        }
        else {
          pic_errorf(pic, "pic_get_args: expected record, but got ~s", v);
        }
        i++;
      }
      break;
    }
    case 'e': {
      struct pic_error **e;
      pic_value v;

      e = va_arg(ap, struct pic_error **);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (pic_error_p(v)) {
          *e = pic_error_ptr(v);
        }
        else {
          pic_errorf(pic, "pic_get_args, expected error");
        }
        i++;
      }
      break;
    }
    default:
      pic_errorf(pic, "pic_get_args: invalid argument specifier '%c' given", c);
    }
  }
  if ('*' == c) {
    size_t *n;
    pic_value **argv;

    n = va_arg(ap, size_t *);
    argv = va_arg(ap, pic_value **);
    if (i <= argc) {
      *n = (size_t)(argc - i);
      *argv = &GET_OPERAND(pic, i);
      i = argc;
    }
  }
  else if (argc > i) {
    pic_errorf(pic, "wrong number of arguments");
  }
  va_end(ap);
  return i - 1;
}

void
pic_define_noexport(pic_state *pic, const char *name, pic_value val)
{
  pic_sym sym, rename;

  sym = pic_intern_cstr(pic, name);

  if (! pic_find_rename(pic, pic->lib->env, sym, &rename)) {
    rename = pic_add_rename(pic, pic->lib->env, sym);
  } else {
    pic_warn(pic, "redefining global");
  }

  pic_dict_set(pic, pic->globals, rename, val);
}

void
pic_define(pic_state *pic, const char *name, pic_value val)
{
  pic_define_noexport(pic, name, val);

  pic_export(pic, pic_intern_cstr(pic, name));
}

pic_value
pic_ref(pic_state *pic, struct pic_lib *lib, const char *name)
{
  pic_sym sym, rename;

  sym = pic_intern_cstr(pic, name);

  if (! pic_find_rename(pic, lib->env, sym, &rename)) {
    pic_errorf(pic, "symbol \"%s\" not defined in library ~s", name, lib->name);
  }

  return pic_dict_ref(pic, pic->globals, rename);
}

void
pic_set(pic_state *pic, struct pic_lib *lib, const char *name, pic_value val)
{
  pic_sym sym, rename;

  sym = pic_intern_cstr(pic, name);

  if (! pic_find_rename(pic, lib->env, sym, &rename)) {
    pic_errorf(pic, "symbol \"%s\" not defined in library ~s", name, lib->name);
  }

  pic_dict_set(pic, pic->globals, rename, val);
}

pic_value
pic_funcall(pic_state *pic, struct pic_lib *lib, const char *name, pic_list args)
{
  pic_value proc;

  proc = pic_ref(pic, lib, name);

  pic_assert_type(pic, proc, proc);

  return pic_apply(pic, pic_proc_ptr(proc), args);
}

void
pic_defun(pic_state *pic, const char *name, pic_func_t cfunc)
{
  struct pic_proc *proc;

  proc = pic_make_proc(pic, cfunc, name);
  pic_define(pic, name, pic_obj_value(proc));
}

void
pic_defvar(pic_state *pic, const char *name, pic_value init, struct pic_proc *conv)
{
  pic_define(pic, name, pic_obj_value(pic_make_var(pic, init, conv)));
}

static void
vm_push_env(pic_state *pic)
{
  pic_callinfo *ci = pic->ci;

  ci->env = (struct pic_env *)pic_obj_alloc(pic, offsetof(struct pic_env, storage) + sizeof(pic_value) * (size_t)(ci->regc), PIC_TT_ENV);
  ci->env->up = ci->up;
  ci->env->regc = ci->regc;
  ci->env->regs = ci->regs;
}

static void
vm_tear_off(pic_callinfo *ci)
{
  struct pic_env *env;
  int i;

  assert(ci->env != NULL);

  env = ci->env;

  if (env->regs == env->storage) {
    return;                     /* is torn off */
  }
  for (i = 0; i < env->regc; ++i) {
    env->storage[i] = env->regs[i];
  }
  env->regs = env->storage;
}

void
pic_vm_tear_off(pic_state *pic)
{
  pic_callinfo *ci;

  for (ci = pic->ci; ci > pic->cibase; ci--) {
    if (ci->env != NULL) {
      vm_tear_off(ci);
    }
  }
}

static struct pic_irep *
vm_get_irep(pic_state *pic)
{
  pic_value self;
  struct pic_irep *irep;

  self = pic->ci->fp[0];
  if (! pic_proc_p(self)) {
    pic_errorf(pic, "logic flaw");
  }
  irep = pic_proc_ptr(self)->u.irep;
  if (! pic_proc_irep_p(pic_proc_ptr(self))) {
    pic_errorf(pic, "logic flaw");
  }
  return irep;
}

pic_value
pic_apply0(pic_state *pic, struct pic_proc *proc)
{
  return pic_apply(pic, proc, pic_nil_value());
}

pic_value
pic_apply1(pic_state *pic, struct pic_proc *proc, pic_value arg1)
{
  return pic_apply(pic, proc, pic_list1(pic, arg1));
}

pic_value
pic_apply2(pic_state *pic, struct pic_proc *proc, pic_value arg1, pic_value arg2)
{
  return pic_apply(pic, proc, pic_list2(pic, arg1, arg2));
}

pic_value
pic_apply3(pic_state *pic, struct pic_proc *proc, pic_value arg1, pic_value arg2, pic_value arg3)
{
  return pic_apply(pic, proc, pic_list3(pic, arg1, arg2, arg3));
}

pic_value
pic_apply4(pic_state *pic, struct pic_proc *proc, pic_value arg1, pic_value arg2, pic_value arg3, pic_value arg4)
{
  return pic_apply(pic, proc, pic_list4(pic, arg1, arg2, arg3, arg4));
}

pic_value
pic_apply5(pic_state *pic, struct pic_proc *proc, pic_value arg1, pic_value arg2, pic_value arg3, pic_value arg4, pic_value arg5)
{
  return pic_apply(pic, proc, pic_list5(pic, arg1, arg2, arg3, arg4, arg5));
}

#if VM_DEBUG
# define OPCODE_EXEC_HOOK pic_dump_code(c)
#else
# define OPCODE_EXEC_HOOK ((void)0)
#endif

#if PIC_DIRECT_THREADED_VM
# define VM_LOOP JUMP;
# define CASE(x) L_##x: OPCODE_EXEC_HOOK;
# define NEXT pic->ip++; JUMP;
# define JUMP c = *pic->ip; goto *oplabels[c.insn];
# define VM_LOOP_END
#else
# define VM_LOOP for (;;) { c = *pic->ip; switch (c.insn) {
# define CASE(x) case x:
# define NEXT pic->ip++; break
# define JUMP break
# define VM_LOOP_END } }
#endif

#define PUSH(v) (*pic->sp++ = (v))
#define POP() (*--pic->sp)

#define PUSHCI() (++pic->ci)
#define POPCI() (pic->ci--)

#if VM_DEBUG
# define VM_BOOT_PRINT                          \
  do {                                          \
    puts("### booting VM... ###");              \
    stbase = pic->sp;                           \
    cibase = pic->ci;                           \
  } while (0)
#else
# define VM_BOOT_PRINT
#endif

#if VM_DEBUG
# define VM_END_PRINT                                                   \
  do {                                                                  \
    puts("**VM END STATE**");                                           \
    printf("stbase\t= %p\nsp\t= %p\n", (void *)stbase, (void *)pic->sp); \
    printf("cibase\t= %p\nci\t= %p\n", (void *)cibase, (void *)pic->ci); \
    if (stbase < pic->sp - 1) {                                         \
      pic_value *sp;                                                    \
      printf("* stack trace:");                                         \
      for (sp = stbase; pic->sp != sp; ++sp) {                          \
        pic_debug(pic, *sp);                                            \
        puts("");                                                       \
      }                                                                 \
    }                                                                   \
    if (stbase > pic->sp - 1) {                                         \
      puts("*** stack underflow!");                                     \
    }                                                                   \
  } while (0)
#else
# define VM_END_PRINT
#endif

#if VM_DEBUG
# define VM_CALL_PRINT                                                  \
  do {                                                                  \
    puts("\n== calling proc...");                                       \
    printf("  proc = ");                                                \
    pic_debug(pic, pic_obj_value(proc));                                \
    puts("");                                                           \
    printf("  argv = (");                                               \
    for (short i = 1; i < c.u.i; ++i) {                                 \
      if (i > 1)                                                        \
        printf(" ");                                                    \
      pic_debug(pic, pic->sp[-c.u.i + i]);                              \
    }                                                                   \
    puts(")");                                                          \
    if (! pic_proc_func_p(proc)) {                                      \
      printf("  irep = %p\n", proc->u.irep);                            \
      printf("  name = %s\n", pic_symbol_name(pic, pic_proc_name(proc))); \
      pic_dump_irep(proc->u.irep);                                      \
    }                                                                   \
    else {                                                              \
      printf("  cfunc = %p\n", (void *)proc->u.func.f);                 \
      printf("  name = %s\n", pic_symbol_name(pic, pic_proc_name(proc))); \
    }                                                                   \
    puts("== end\n");                                                   \
  } while (0)
#else
# define VM_CALL_PRINT
#endif

pic_value
pic_apply(pic_state *pic, struct pic_proc *proc, pic_value args)
{
  pic_code c;
  size_t ai = pic_gc_arena_preserve(pic);
  pic_code boot[2];

#if PIC_DIRECT_THREADED_VM
  static void *oplabels[] = {
    &&L_OP_NOP, &&L_OP_POP, &&L_OP_PUSHNIL, &&L_OP_PUSHTRUE, &&L_OP_PUSHFALSE,
    &&L_OP_PUSHINT, &&L_OP_PUSHCHAR, &&L_OP_PUSHCONST,
    &&L_OP_GREF, &&L_OP_GSET, &&L_OP_LREF, &&L_OP_LSET, &&L_OP_CREF, &&L_OP_CSET,
    &&L_OP_JMP, &&L_OP_JMPIF, &&L_OP_NOT, &&L_OP_CALL, &&L_OP_TAILCALL, &&L_OP_RET,
    &&L_OP_LAMBDA, &&L_OP_CONS, &&L_OP_CAR, &&L_OP_CDR, &&L_OP_NILP,
    &&L_OP_SYMBOLP, &&L_OP_PAIRP,
    &&L_OP_ADD, &&L_OP_SUB, &&L_OP_MUL, &&L_OP_DIV, &&L_OP_MINUS,
    &&L_OP_EQ, &&L_OP_LT, &&L_OP_LE, &&L_OP_STOP
  };
#endif

#if VM_DEBUG
  pic_value *stbase;
  pic_callinfo *cibase;
#endif

  if (! pic_list_p(args)) {
    pic_errorf(pic, "argv must be a proper list");
  }
  else {
    int argc, i;

    argc = (int)pic_length(pic, args) + 1;

    VM_BOOT_PRINT;

    PUSH(pic_obj_value(proc));
    for (i = 1; i < argc; ++i) {
      PUSH(pic_car(pic, args));
      args = pic_cdr(pic, args);
    }

    /* boot! */
    boot[0].insn = OP_CALL;
    boot[0].u.i = argc;
    boot[1].insn = OP_STOP;
    pic->ip = boot;
  }

  VM_LOOP {
    CASE(OP_NOP) {
      NEXT;
    }
    CASE(OP_POP) {
      (void)(POP());
      NEXT;
    }
    CASE(OP_PUSHNIL) {
      PUSH(pic_nil_value());
      NEXT;
    }
    CASE(OP_PUSHTRUE) {
      PUSH(pic_true_value());
      NEXT;
    }
    CASE(OP_PUSHFALSE) {
      PUSH(pic_false_value());
      NEXT;
    }
    CASE(OP_PUSHINT) {
      PUSH(pic_int_value(c.u.i));
      NEXT;
    }
    CASE(OP_PUSHCHAR) {
      PUSH(pic_char_value(c.u.c));
      NEXT;
    }
    CASE(OP_PUSHCONST) {
      struct pic_irep *irep = vm_get_irep(pic);

      PUSH(irep->pool[c.u.i]);
      NEXT;
    }
    CASE(OP_GREF) {
      struct pic_irep *irep = vm_get_irep(pic);
      pic_sym sym;

      sym = irep->syms[c.u.i];
      if (! pic_dict_has(pic, pic->globals, sym)) {
        pic_errorf(pic, "logic flaw; reference to uninitialized global variable: %s", pic_symbol_name(pic, sym));
      }
      PUSH(pic_dict_ref(pic, pic->globals, sym));
      NEXT;
    }
    CASE(OP_GSET) {
      struct pic_irep *irep = vm_get_irep(pic);
      pic_sym sym;
      pic_value val;

      sym = irep->syms[c.u.i];

      val = POP();
      pic_dict_set(pic, pic->globals, sym, val);
      NEXT;
    }
    CASE(OP_LREF) {
      pic_callinfo *ci = pic->ci;
      struct pic_irep *irep;

      if (ci->env != NULL && ci->env->regs == ci->env->storage) {
        irep = pic_get_proc(pic)->u.irep;
        if (c.u.i >= irep->argc + irep->localc) {
          PUSH(ci->env->regs[c.u.i - (ci->regs - ci->fp)]);
          NEXT;
        }
      }
      PUSH(pic->ci->fp[c.u.i]);
      NEXT;
    }
    CASE(OP_LSET) {
      pic_callinfo *ci = pic->ci;
      struct pic_irep *irep;

      if (ci->env != NULL && ci->env->regs == ci->env->storage) {
        irep = pic_get_proc(pic)->u.irep;
        if (c.u.i >= irep->argc + irep->localc) {
          ci->env->regs[c.u.i - (ci->regs - ci->fp)] = POP();
          NEXT;
        }
      }
      pic->ci->fp[c.u.i] = POP();
      NEXT;
    }
    CASE(OP_CREF) {
      int depth = c.u.r.depth;
      struct pic_env *env;

      env = pic->ci->up;
      while (--depth) {
	env = env->up;
      }
      PUSH(env->regs[c.u.r.idx]);
      NEXT;
    }
    CASE(OP_CSET) {
      int depth = c.u.r.depth;
      struct pic_env *env;

      env = pic->ci->up;
      while (--depth) {
	env = env->up;
      }
      env->regs[c.u.r.idx] = POP();
      NEXT;
    }
    CASE(OP_JMP) {
      pic->ip += c.u.i;
      JUMP;
    }
    CASE(OP_JMPIF) {
      pic_value v;

      v = POP();
      if (! pic_false_p(v)) {
	pic->ip += c.u.i;
	JUMP;
      }
      NEXT;
    }
    CASE(OP_NOT) {
      pic_value v;

      v = pic_false_p(POP()) ? pic_true_value() : pic_false_value();
      PUSH(v);
      NEXT;
    }
    CASE(OP_CALL) {
      pic_value x, v;
      pic_callinfo *ci;

      if (c.u.i == -1) {
        pic->sp += pic->ci[1].retc - 1;
        c.u.i = pic->ci[1].retc + 1;
      }

    L_CALL:
      x = pic->sp[-c.u.i];
      if (! pic_proc_p(x)) {
	pic_errorf(pic, "invalid application: ~s", x);
      }
      proc = pic_proc_ptr(x);

      VM_CALL_PRINT;

      if (pic->sp >= pic->stend) {
        pic_panic(pic, "VM stack overflow");
      }

      ci = PUSHCI();
      ci->argc = c.u.i;
      ci->retc = 1;
      ci->ip = pic->ip;
      ci->fp = pic->sp - c.u.i;
      ci->env = NULL;
      if (pic_proc_func_p(pic_proc_ptr(x))) {

        /* invoke! */
        v = proc->u.func.f(pic);
        pic->sp[0] = v;
        pic->sp += pic->ci->retc;

        pic_gc_arena_restore(pic, ai);
        goto L_RET;
      }
      else {
        struct pic_irep *irep = proc->u.irep;
	int i;
	pic_value rest;

	if (ci->argc != irep->argc) {
	  if (! (irep->varg && ci->argc >= irep->argc)) {
            pic_errorf(pic, "wrong number of arguments (%d for %d%s)", ci->argc - 1, irep->argc - 1, (irep->varg ? "+" : ""));
	  }
	}
	/* prepare rest args */
	if (irep->varg) {
	  rest = pic_nil_value();
	  for (i = 0; i < ci->argc - irep->argc; ++i) {
	    pic_gc_protect(pic, v = POP());
	    rest = pic_cons(pic, v, rest);
	  }
	  PUSH(rest);
	}
	/* prepare local variable area */
	if (irep->localc > 0) {
	  int l = irep->localc;
	  if (irep->varg) {
	    --l;
	  }
	  for (i = 0; i < l; ++i) {
	    PUSH(pic_undef_value());
	  }
	}

	/* prepare env */
        ci->up = proc->env;
        ci->regc = irep->capturec;
        ci->regs = ci->fp + irep->argc + irep->localc;

	pic->ip = irep->code;
	pic_gc_arena_restore(pic, ai);
	JUMP;
      }
    }
    CASE(OP_TAILCALL) {
      int i, argc;
      pic_value *argv;
      pic_callinfo *ci;

      if (pic->ci->env != NULL) {
        vm_tear_off(pic->ci);
      }

      if (c.u.i == -1) {
        pic->sp += pic->ci[1].retc - 1;
        c.u.i = pic->ci[1].retc + 1;
      }

      argc = c.u.i;
      argv = pic->sp - argc;
      for (i = 0; i < argc; ++i) {
	pic->ci->fp[i] = argv[i];
      }
      ci = POPCI();
      pic->sp = ci->fp + argc;
      pic->ip = ci->ip;

      /* c is not changed */
      goto L_CALL;
    }
    CASE(OP_RET) {
      int i, retc;
      pic_value *retv;
      pic_callinfo *ci;

      if (pic->ci->env != NULL) {
        vm_tear_off(pic->ci);
      }

      pic->ci->retc = c.u.i;

    L_RET:
      retc = pic->ci->retc;
      retv = pic->sp - retc;
      if (retc == 0) {
        pic->ci->fp[0] = retv[0]; /* copy at least once */
      }
      for (i = 0; i < retc; ++i) {
        pic->ci->fp[i] = retv[i];
      }
      ci = POPCI();
      pic->sp = ci->fp + 1;     /* advance only one! */
      pic->ip = ci->ip;

      NEXT;
    }
    CASE(OP_LAMBDA) {
      pic_value self;
      struct pic_irep *irep;

      self = pic->ci->fp[0];
      if (! pic_proc_p(self)) {
        pic_errorf(pic, "logic flaw");
      }
      irep = pic_proc_ptr(self)->u.irep;
      if (! pic_proc_irep_p(pic_proc_ptr(self))) {
        pic_errorf(pic, "logic flaw");
      }

      if (pic->ci->env == NULL) {
        vm_push_env(pic);
      }

      proc = pic_make_proc_irep(pic, irep->irep[c.u.i], pic->ci->env);
      PUSH(pic_obj_value(proc));
      pic_gc_arena_restore(pic, ai);
      NEXT;
    }
    CASE(OP_CONS) {
      pic_value a, b;
      pic_gc_protect(pic, b = POP());
      pic_gc_protect(pic, a = POP());
      PUSH(pic_cons(pic, a, b));
      pic_gc_arena_restore(pic, ai);
      NEXT;
    }
    CASE(OP_CAR) {
      pic_value p;
      p = POP();
      PUSH(pic_car(pic, p));
      NEXT;
    }
    CASE(OP_CDR) {
      pic_value p;
      p = POP();
      PUSH(pic_cdr(pic, p));
      NEXT;
    }
    CASE(OP_NILP) {
      pic_value p;
      p = POP();
      PUSH(pic_bool_value(pic_nil_p(p)));
      NEXT;
    }

    CASE(OP_SYMBOLP) {
      pic_value p;
      p = POP();
      PUSH(pic_bool_value(pic_sym_p(p)));
      NEXT;
    }

    CASE(OP_PAIRP) {
      pic_value p;
      p = POP();
      PUSH(pic_bool_value(pic_pair_p(p)));
      NEXT;
    }

#define DEFINE_ARITH_OP(opcode, op, guard)			\
    CASE(opcode) {						\
      pic_value a, b;						\
      b = POP();						\
      a = POP();						\
      if (pic_int_p(a) && pic_int_p(b)) {			\
	double f = (double)pic_int(a) op (double)pic_int(b);	\
	if (INT_MIN <= f && f <= INT_MAX && (guard)) {		\
	  PUSH(pic_int_value((int)f));				\
	}							\
	else {							\
	  PUSH(pic_float_value(f));				\
	}							\
      }								\
      else if (pic_float_p(a) && pic_float_p(b)) {		\
	PUSH(pic_float_value(pic_float(a) op pic_float(b)));	\
      }								\
      else if (pic_int_p(a) && pic_float_p(b)) {		\
	PUSH(pic_float_value(pic_int(a) op pic_float(b)));	\
      }								\
      else if (pic_float_p(a) && pic_int_p(b)) {		\
	PUSH(pic_float_value(pic_float(a) op pic_int(b)));	\
      }								\
      else {							\
	pic_errorf(pic, #op " got non-number operands");        \
      }								\
      NEXT;							\
    }

    DEFINE_ARITH_OP(OP_ADD, +, true);
    DEFINE_ARITH_OP(OP_SUB, -, true);
    DEFINE_ARITH_OP(OP_MUL, *, true);
    DEFINE_ARITH_OP(OP_DIV, /, f == round(f));

    CASE(OP_MINUS) {
      pic_value n;
      n = POP();
      if (pic_int_p(n)) {
	PUSH(pic_int_value(-pic_int(n)));
      }
      else if (pic_float_p(n)) {
	PUSH(pic_float_value(-pic_float(n)));
      }
      else {
	pic_errorf(pic, "unary - got a non-number operand");
      }
      NEXT;
    }

#define DEFINE_COMP_OP(opcode, op)				\
    CASE(opcode) {						\
      pic_value a, b;						\
      b = POP();						\
      a = POP();						\
      if (pic_int_p(a) && pic_int_p(b)) {			\
	PUSH(pic_bool_value(pic_int(a) op pic_int(b)));		\
      }								\
      else if (pic_float_p(a) && pic_float_p(b)) {		\
	PUSH(pic_bool_value(pic_float(a) op pic_float(b)));	\
      }								\
      else if (pic_int_p(a) && pic_float_p(b)) {		\
	PUSH(pic_bool_value(pic_int(a) op pic_float(b)));	\
      }								\
      else if (pic_float_p(a) && pic_int_p(b)) {		\
	PUSH(pic_bool_value(pic_float(a) op pic_int(b)));	\
      }								\
      else {							\
	pic_errorf(pic, #op " got non-number operands");        \
      }								\
      NEXT;							\
    }

    DEFINE_COMP_OP(OP_EQ, ==);
    DEFINE_COMP_OP(OP_LT, <);
    DEFINE_COMP_OP(OP_LE, <=);

    CASE(OP_STOP) {

      VM_END_PRINT;

      return pic_gc_protect(pic, POP());
    }
  } VM_LOOP_END;
}

pic_value
pic_apply_trampoline(pic_state *pic, struct pic_proc *proc, pic_value args)
{
  static const pic_code iseq[2] = {
    { OP_NOP, { .i = 0 } },
    { OP_TAILCALL, { .i = -1 } }
  };

  pic_value v, *sp;
  pic_callinfo *ci;

  *pic->sp++ = pic_obj_value(proc);

  sp = pic->sp;
  pic_for_each (v, args) {
    *sp++ = v;
  }

  ci = PUSHCI();
  ci->ip = (pic_code *)iseq;
  ci->fp = pic->sp;
  ci->retc = (int)pic_length(pic, args);

  if (ci->retc == 0) {
    return pic_none_value();
  } else {
    return pic_car(pic, args);
  }
}
