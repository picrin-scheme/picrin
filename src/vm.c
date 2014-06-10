/**
 * See Copyright Notice in picrin.h
 */

#include <stdlib.h>
#include <stdarg.h>
#include <limits.h>
#include <math.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/vector.h"
#include "picrin/number.h"
#include "picrin/proc.h"
#include "picrin/port.h"
#include "picrin/irep.h"
#include "picrin/blob.h"
#include "picrin/var.h"
#include "picrin/lib.h"
#include "picrin/macro.h"
#include "picrin/error.h"
#include "picrin/dict.h"

#define GET_OPERAND(pic,n) ((pic)->ci->fp[(n)])

struct pic_proc *
pic_get_proc(pic_state *pic)
{
  pic_value v = GET_OPERAND(pic,0);

  if (! pic_proc_p(v)) {
    pic_error(pic, "fatal error");
  }
  return pic_proc_ptr(v);
}

/**
 * char type
 * ---- ----
 *  o   object
 *  i   int
 *  I   int with exactness
 *  f   float
 *  F   float with exactness
 *  s   string object
 *  z   c string
 *  m   symbol
 *  v   vector object
 *  b   bytevector object
 *  c   char
 *  l   lambda object
 *  p   port object
 *  d   dictionary object
 *
 *  |  optional operator
 *  *  variable length operator
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
	pic_error(pic, "wrong number of arguments");
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
        case PIC_TT_BIGINT:
          *f = mpz_get_d(pic_bigint_ptr(v)->z);
          break;
        case PIC_TT_RATIONAL:
          *f = mpq_get_d(pic_rational_ptr(v)->q);
          break;
        default:
          pic_error(pic, "pic_get_args: expected float or int");
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
        case PIC_TT_BIGINT:
          *f = mpz_get_d(pic_bigint_ptr(v)->z);
          *e = true;
          break;
        case PIC_TT_RATIONAL:
          *f = mpq_get_d(pic_rational_ptr(v)->q);
          *e = true;
          break;
        default:
          pic_error(pic, "pic_get_args: expected float or int or rational");
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
        case PIC_TT_BIGINT:
          *k = mpz_get_d(pic_bigint_ptr(v)->z);
          *e = true;
          break;
        case PIC_TT_RATIONAL:
          *k = mpq_get_d(pic_rational_ptr(v)->q);
          *e = true;
          break;
        default:
          pic_error(pic, "pic_get_args: expected float or int or rational");
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
        case PIC_TT_BIGINT:
          *k = mpz_get_d(pic_bigint_ptr(v)->z);
          break;
        case PIC_TT_RATIONAL:
          *k = mpq_get_d(pic_rational_ptr(v)->q);
          break;
        default:
          pic_error(pic, "pic_get_args: expected int");
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
          pic_error(pic, "pic_get_args: expected string");
        }
        i++;
      }
      break;
    }
    case 'z': {
      pic_value str;
      const char **cstr;

      cstr = va_arg(ap, const char **);
      if (i < argc) {
        str = GET_OPERAND(pic,i);
        if (! pic_str_p(str)) {
          pic_error(pic, "pic_get_args: expected string");
        }
        *cstr = pic_str_cstr(pic_str_ptr(str));
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
          pic_error(pic, "pic_get_args: expected symbol");
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
          pic_error(pic, "pic_get_args: expected vector");
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
          pic_error(pic, "pic_get_args: expected bytevector");
        }
        i++;
      }
      break;
    }
    case 'c': {
      char *c;
      pic_value v;

      c = va_arg(ap, char *);
      if (i < argc) {
        v = GET_OPERAND(pic,i);
        if (pic_char_p(v)) {
          *c = pic_char(v);
        }
        else {
          pic_error(pic, "pic_get_args: expected char");
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
          pic_error(pic, "pic_get_args, expected procedure");
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
          pic_error(pic, "pic_get_args, expected port");
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
          pic_error(pic, "pic_get_args, expected dictionary");
        }
        i++;
      }
      break;
    }
    default:
      pic_error(pic, "pic_get_args: invalid argument specifier given");
    }
  }
  if ('*' == c) {
    size_t *n;
    pic_value **argv;

    n = va_arg(ap, size_t *);
    argv = va_arg(ap, pic_value **);
    if (i <= argc) {
      *n = argc - i;
      *argv = &GET_OPERAND(pic, i);
      i = argc;
    }
  }
  else if (argc > i) {
    pic_error(pic, "wrong number of arguments");
  }
  va_end(ap);
  return i - 1;
}

static size_t
global_ref(pic_state *pic, const char *name)
{
  xh_entry *e;
  pic_sym sym, rename;

  sym = pic_intern_cstr(pic, name);
  if (! pic_find_rename(pic, pic->lib->senv, sym, &rename)) {
    return SIZE_MAX;
  }
  if (! (e = xh_get_int(&pic->global_tbl, rename))) {
    return SIZE_MAX;
  }
  return xh_val(e, size_t);
}

static size_t
global_def(pic_state *pic, const char *name)
{
  pic_sym sym, rename;
  size_t gidx;

  sym = pic_intern_cstr(pic, name);
  if ((gidx = global_ref(pic, name)) != SIZE_MAX) {
    pic_warn(pic, "redefining global");
    return gidx;
  }

  /* register to the senv */
  rename = pic_add_rename(pic, pic->lib->senv, sym);

  /* register to the global table */
  gidx = pic->glen++;
  if (pic->glen >= pic->gcapa) {
    pic_error(pic, "global table overflow");
  }
  xh_put_int(&pic->global_tbl, rename, &gidx);

  return gidx;
}

void
pic_define(pic_state *pic, const char *name, pic_value val)
{
  /* push to the global arena */
  pic->globals[global_def(pic, name)] = val;

  /* export! */
  pic_export(pic, pic_intern_cstr(pic, name));
}

pic_value
pic_ref(pic_state *pic, const char *name)
{
  size_t gid;

  gid = global_ref(pic, name);
  if (gid == SIZE_MAX) {
    pic_error(pic, "symbol not defined");
  }
  return pic->globals[gid];
}

void
pic_set(pic_state *pic, const char *name, pic_value value)
{
  size_t gid;

  gid = global_ref(pic, name);
  if (gid == SIZE_MAX) {
    pic_error(pic, "symbol not defined");
  }
  pic->globals[gid] = value;
}

void
pic_defun(pic_state *pic, const char *name, pic_func_t cfunc)
{
  struct pic_proc *proc;

  proc = pic_proc_new(pic, cfunc, name);
  pic_define(pic, name, pic_obj_value(proc));
}

void
pic_defvar(pic_state *pic, const char *name, pic_value init)
{
  struct pic_var *var;

  var = pic_var_new(pic, init, NULL);
  pic_define(pic, name, pic_obj_value(pic_wrap_var(pic, var)));
}

static void
vm_push_env(pic_state *pic)
{
  pic_callinfo *ci = pic->ci;

  ci->env = (struct pic_env *)pic_obj_alloc(pic, offsetof(struct pic_env, storage) + sizeof(pic_value) * ci->regc, PIC_TT_ENV);
  ci->env->up = ci->up;
  ci->env->regc = ci->regc;
  ci->env->regs = ci->regs;
}

static void
vm_tear_off(pic_state *pic)
{
  struct pic_env *env;
  int i;

  assert(pic->ci->env != NULL);

  env = pic->ci->env;
  for (i = 0; i < env->regc; ++i) {
    env->storage[i] = env->regs[i];
  }
  env->regs = env->storage;
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

#define PUSH(v) ((pic->sp >= pic->stend) ? abort() : (*pic->sp++ = (v)))
#define POP() (*--pic->sp)

#define PUSHCI() (++pic->ci)
#define POPCI() (pic->ci--)

pic_value
pic_apply(pic_state *pic, struct pic_proc *proc, pic_value argv)
{
  pic_code c;
  size_t ai = pic_gc_arena_preserve(pic);
  size_t argc, i;
  pic_code boot[2];

#if PIC_DIRECT_THREADED_VM
  static void *oplabels[] = {
    &&L_OP_NOP, &&L_OP_POP, &&L_OP_PUSHNIL, &&L_OP_PUSHTRUE, &&L_OP_PUSHFALSE,
    &&L_OP_PUSHINT, &&L_OP_PUSHCHAR, &&L_OP_PUSHCONST,
    &&L_OP_GREF, &&L_OP_GSET, &&L_OP_LREF, &&L_OP_LSET, &&L_OP_CREF, &&L_OP_CSET,
    &&L_OP_JMP, &&L_OP_JMPIF, &&L_OP_NOT, &&L_OP_CALL, &&L_OP_TAILCALL, &&L_OP_RET,
    &&L_OP_LAMBDA, &&L_OP_CONS, &&L_OP_CAR, &&L_OP_CDR, &&L_OP_NILP,
    &&L_OP_ADD, &&L_OP_SUB, &&L_OP_MUL, &&L_OP_DIV, &&L_OP_MINUS,
    &&L_OP_EQ, &&L_OP_LT, &&L_OP_LE, &&L_OP_STOP
  };
#endif

  if (! pic_list_p(argv)) {
    pic_error(pic, "argv must be a proper list");
  }

  argc = pic_length(pic, argv) + 1;

#if VM_DEBUG
  puts("### booting VM... ###");
  pic_value *stbase = pic->sp;
  pic_callinfo *cibase = pic->ci;
#endif

  PUSH(pic_obj_value(proc));
  for (i = 1; i < argc; ++i) {
    PUSH(pic_car(pic, argv));
    argv = pic_cdr(pic, argv);
  }

  /* boot! */
  boot[0].insn = OP_CALL;
  boot[0].u.i = argc;
  boot[1].insn = OP_STOP;
  pic->ip = boot;

  VM_LOOP {
    CASE(OP_NOP) {
      NEXT;
    }
    CASE(OP_POP) {
      POP();
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
      pic_value self;
      struct pic_irep *irep;

      self = pic->ci->fp[0];
      if (! pic_proc_p(self)) {
        pic_error(pic, "logic flaw");
      }
      irep = pic_proc_ptr(self)->u.irep;
      if (! pic_proc_irep_p(pic_proc_ptr(self))) {
        pic_error(pic, "logic flaw");
      }
      PUSH(irep->pool[c.u.i]);
      NEXT;
    }
    CASE(OP_GREF) {
      PUSH(pic->globals[c.u.i]);
      NEXT;
    }
    CASE(OP_GSET) {
      pic->globals[c.u.i] = POP();
      NEXT;
    }
    CASE(OP_LREF) {
      PUSH(pic->ci->fp[c.u.i]);
      NEXT;
    }
    CASE(OP_LSET) {
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
      struct pic_proc *proc;

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

#if VM_DEBUG
      puts("\n== calling proc...");
      printf("  proc = ");
      pic_debug(pic, pic_obj_value(proc));
      puts("");
      printf("  argv = (");
      for (short i = 1; i < c.u.i; ++i) {
        if (i > 1)
          printf(" ");
        pic_debug(pic, pic->sp[-c.u.i + i]);
      }
      puts(")");
      if (! pic_proc_func_p(proc)) {
	printf("  irep = %p\n", proc->u.irep);
	printf("  name = %s\n", pic_symbol_name(pic, pic_proc_name(proc)));
	pic_dump_irep(proc->u.irep);
      }
      else {
	printf("  cfunc = %p\n", (void *)proc->u.func.f);
	printf("  name = %s\n", pic_symbol_name(pic, pic_proc_name(proc)));
      }
      puts("== end\n");
#endif

      ci = PUSHCI();
      ci->argc = c.u.i;
      ci->retc = 1;
      ci->ip = pic->ip;
      ci->fp = pic->sp - c.u.i;
      ci->env = NULL;
      if (pic_proc_func_p(pic_proc_ptr(x))) {

        /* invoke! */
	pic->sp[0] = proc->u.func.f(pic);
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
        vm_tear_off(pic);
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
        vm_tear_off(pic);
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
      struct pic_proc *proc;

      self = pic->ci->fp[0];
      if (! pic_proc_p(self)) {
        pic_error(pic, "logic flaw");
      }
      irep = pic_proc_ptr(self)->u.irep;
      if (! pic_proc_irep_p(pic_proc_ptr(self))) {
        pic_error(pic, "logic flaw");
      }

      if (pic->ci->env == NULL) {
        vm_push_env(pic);
      }

      proc = pic_proc_new_irep(pic, irep->irep[c.u.i], pic->ci->env);
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

#define DEFINE_ARITH_OP(opcode, op, name, guard)                        \
    CASE(opcode) {                                                      \
      pic_value a, b;                                                   \
      b = POP();                                                        \
      a = POP();                                                        \
      if (pic_int_p(a) && pic_int_p(b)) {                               \
        double f = (double) pic_int(a) op                               \
          (double) pic_int(b);                                          \
	if (INT_MIN <= f && f <= INT_MAX && (guard)) {                  \
	  PUSH(pic_int_value((int)f));                                  \
	}                                                               \
	else {                                                          \
          pic_rational *res = pic_rational_new(pic);                    \
          mpq_set_d(res->q, f);                                         \
          PUSH(pic_obj_value(res));                                     \
	}                                                               \
      }                                                                 \
      else if (pic_float_p(a) && pic_float_p(b)) {                      \
	PUSH(pic_float_value(pic_float(a) op pic_float(b)));            \
      }                                                                 \
      else if (pic_bigint_p(a) && pic_bigint_p(b)) {                    \
        PUSH(pic_obj_value(pic_bigint_##name(pic,                       \
                                             pic_bigint_ptr(a),         \
                                             pic_bigint_ptr(b))));      \
      }                                                                 \
      else if (pic_rational_p(a) && pic_rational_p(b)) {                \
        PUSH(pic_obj_value(pic_rational_##name(pic,                     \
                                               pic_rational_ptr(a),     \
                                               pic_rational_ptr(b))));  \
      }                                                                 \
      else if (pic_int_p(a) && pic_float_p(b)) {                        \
	PUSH(pic_float_value(pic_int(a) op pic_float(b)));              \
      }                                                                 \
      else if (pic_float_p(a) && pic_int_p(b)) {                        \
	PUSH(pic_float_value(pic_float(a) op pic_int(b)));              \
      }                                                                 \
      else if(pic_bigint_p(a) && pic_int_p(b)){                         \
        pic_bigint *c = pic_bigint_new(pic);                            \
        mpz_set_si(c->z, pic_int(b));                                   \
        PUSH(pic_obj_value(pic_bigint_##name(pic,                       \
                                             pic_bigint_ptr(a),         \
                                             c)));                      \
      }                                                                 \
      else if(pic_int_p(a) && pic_bigint_p(b)){                         \
        pic_bigint *c = pic_bigint_new(pic);                            \
        mpz_set_si(c->z, pic_int(a));                                   \
        PUSH(pic_obj_value(pic_bigint_##name(pic,                       \
                                             c,                         \
                                             pic_bigint_ptr(b))));      \
      }                                                                 \
      else if(pic_bigint_p(a) && pic_float_p(b)){                       \
        PUSH(pic_float_value(mpz_get_d(pic_bigint_ptr(a)->z)            \
                             op pic_int(b)));                           \
      }                                                                 \
      else if(pic_float_p(a) && pic_bigint_p(b)){                       \
        PUSH(pic_float_value( pic_int(a) op                             \
                              mpz_get_d(pic_bigint_ptr(b)->z)));        \
      }                                                                 \
      else if (pic_rational_p(a) && pic_int_p(b)) {                     \
        pic_rational *c = pic_rational_new(pic);                        \
        mpq_set_d(c->q, (double) pic_int(b));                           \
        PUSH(pic_obj_value(pic_rational_##name(pic,                     \
                                               pic_rational_ptr(a),     \
                                               c)));                    \
      }                                                                 \
      else if (pic_int_p(b) && pic_rational_p(a)) {                     \
        pic_rational *c = pic_rational_new(pic);                        \
        mpq_set_d(c->q, (double) pic_int(a));                           \
        PUSH(pic_obj_value(pic_rational_##name(pic,                     \
                                               c,                       \
                                               pic_rational_ptr(b))));  \
      }                                                                 \
      else if (pic_rational_p(a) && pic_float_p(b)) {                   \
        PUSH(pic_float_value(pic_float(a) op                            \
                             mpq_get_d(pic_rational_ptr(b)->q)));       \
      }                                                                 \
      else if (pic_float_p(b) && pic_rational_p(a)) {                   \
        PUSH(pic_float_value(mpq_get_d(pic_rational_ptr(b)->q)          \
                             op pic_float(a)));                         \
      }                                                                 \
      else if (pic_rational_p(a) && pic_bigint_p(b)) {                  \
        pic_rational *c = pic_rational_new(pic);                        \
        mpq_set_z(c->q, pic_bigint_ptr(b)->z);                          \
        PUSH(pic_obj_value(pic_rational_##name(pic,                     \
                                               pic_rational_ptr(a),     \
                                               c)));                    \
      }                                                                 \
      else if (pic_bigint_p(b) && pic_rational_p(a)) {                  \
        pic_rational *c = pic_rational_new(pic);                        \
        mpq_set_z(c->q, pic_bigint_ptr(a)->z);                          \
        PUSH(pic_obj_value(pic_rational_##name(pic,                     \
                                               c,                       \
                                               pic_rational_ptr(b))));  \
      }                                                                 \
      else {                                                            \
	pic_errorf(pic, #op " got non-number operands");                \
      }                                                                 \
      pic_number_normalize(pic, pic->sp - 1);                           \
      NEXT;                                                             \
    }

    DEFINE_ARITH_OP(OP_ADD, +, add, true);
    DEFINE_ARITH_OP(OP_SUB, -, sub, true);
    DEFINE_ARITH_OP(OP_MUL, *, mul,  true);
    DEFINE_ARITH_OP(OP_DIV, /, div, f == round(f));

    CASE(OP_MINUS) {
      pic_value n;
      n = POP();
      if (pic_int_p(n)) {
	PUSH(pic_int_value(-pic_int(n)));
      }
      else if (pic_float_p(n)) {
	PUSH(pic_float_value(-pic_float(n)));
      }
      else if (pic_bigint_p(n)) {
        pic_bigint *res;
        res = pic_bigint_new(pic);
        mpz_neg(res->z, pic_bigint_ptr(n)->z);
        PUSH(pic_obj_value(res));
      }
      else if (pic_rational_p(n)) {
        pic_rational *res = pic_rational_new(pic);
        mpq_neg(res->q, pic_rational_ptr(n)->q);
        PUSH(pic_obj_value(res));
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
      else if (pic_bigint_p(a) && pic_bigint_p(b)) {		\
        PUSH(pic_bool_value(mpz_cmp(pic_bigint_ptr(a)->z,       \
                                    pic_bigint_ptr(b)->z) op 0));\
      }								\
      else if (pic_rational_p(a) && pic_rational_p(b)) {	\
        PUSH(pic_bool_value(mpq_cmp(pic_rational_ptr(a)->q,     \
                                    pic_rational_ptr(b)->q) op 0));\
      }								\
      else if (pic_int_p(a) && pic_float_p(b)) {		\
	PUSH(pic_bool_value(pic_int(a) op pic_float(b)));	\
      }								\
      else if (pic_float_p(a) && pic_int_p(b)) {		\
	PUSH(pic_bool_value(pic_float(a) op pic_int(b)));	\
      }								\
      else if (pic_bigint_p(a) && pic_int_p(b)) {		\
        PUSH(pic_bool_value(mpz_cmp_si(pic_bigint_ptr(a)->z,    \
            pic_int(b)) op 0));                                 \
      }								\
      else if (pic_int_p(a) && pic_bigint_p(b)) {		\
        PUSH(pic_bool_value(0 op mpz_cmp_si(pic_bigint_ptr(b)->z,\
                                            pic_int(a))));      \
      }								\
      else if (pic_bigint_p(a) && pic_float_p(b)) {		\
        PUSH(pic_bool_value(mpz_cmp_d(pic_bigint_ptr(a)->z,     \
                                      pic_float(b)) op 0));     \
      }								\
      else if (pic_float_p(a) && pic_bigint_p(b)) {		\
        PUSH(pic_bool_value(0 op mpz_cmp_d(pic_bigint_ptr(b)->z,\
                                           pic_float(a))));     \
      }								\
      else {							\
	pic_errorf(pic, #op " got non-number operands");	\
      }								\
      NEXT;							\
    }

    DEFINE_COMP_OP(OP_EQ, ==);
    DEFINE_COMP_OP(OP_LT, <);
    DEFINE_COMP_OP(OP_LE, <=);

    CASE(OP_STOP) {

#if VM_DEBUG
      puts("**VM END STATE**");
      printf("stbase\t= %p\nsp\t= %p\n", (void *)stbase, (void *)pic->sp);
      printf("cibase\t= %p\nci\t= %p\n", (void *)cibase, (void *)pic->ci);
      if (stbase < pic->sp - 1) {
	pic_value *sp;
	printf("* stack trace:");
	for (sp = stbase; pic->sp != sp; ++sp) {
	  pic_debug(pic, *sp);
	  puts("");
	}
      }
      if (stbase > pic->sp - 1) {
	puts("*** stack underflow!");
      }
#endif

      return pic_gc_protect(pic, POP());
    }
  } VM_LOOP_END;
}

pic_value
pic_apply_trampoline(pic_state *pic, struct pic_proc *proc, pic_value args)
{
  static const pic_code iseq = { OP_TAILCALL, { .i = -1 } };

  pic_value v, *sp;
  pic_callinfo *ci;

  *pic->sp++ = pic_obj_value(proc);

  sp = pic->sp;
  pic_for_each (v, args) {
    *sp++ = v;
  }

  ci = PUSHCI();
  ci->ip = (pic_code *)&iseq - 1;
  ci->fp = pic->sp;
  ci->retc = pic_length(pic, args);
  return pic_obj_value(proc);
}

pic_value
pic_eval(pic_state *pic, pic_value program)
{
  struct pic_proc *proc;

  proc = pic_compile(pic, program);

  return pic_apply(pic, proc, pic_nil_value());
}
