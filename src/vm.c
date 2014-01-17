/**
 * See Copyright Notice in picrin.h
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <limits.h>
#include <math.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/port.h"
#include "picrin/irep.h"
#include "picrin/blob.h"
#include "picrin/var.h"

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
        default:
          pic_error(pic, "pic_get_args: expected float or int");
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
          pic_error(pic, "pic_get_args: expected float or int");
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
          pic_error(pic, "pic_get_args: expected int");
        }
        i++;
      }
      break;
    }
    case 's': {
      pic_value str;
      char **cstr;
      size_t *len;

      cstr = va_arg(ap, char **);
      len = va_arg(ap, size_t *);
      if (i < argc) {
        str = GET_OPERAND(pic,i);
        if (! pic_str_p(str)) {
          pic_error(pic, "pic_get_args: expected string");
        }
        *cstr = pic_str_ptr(str)->str;
        *len = pic_str_ptr(str)->len;
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
        if (pic_symbol_p(v)) {
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
  return i;
}

void
pic_defun(pic_state *pic, const char *name, pic_func_t cfunc)
{
  struct pic_proc *proc;

  proc = pic_proc_new(pic, cfunc);
  pic_define(pic, name, pic_obj_value(proc));
}

void
pic_defvar(pic_state *pic, const char *name, pic_value init)
{
  struct pic_var *var;

  var = pic_var_new(pic, init, NULL);
  pic_define(pic, name, pic_obj_value(pic_wrap_var(pic, var)));
}

pic_value
pic_apply_argv(pic_state *pic, struct pic_proc *proc, size_t argc, ...)
{
  va_list ap;
  pic_value v;

  va_start(ap, argc);

  v = pic_nil_value();
  while (argc--) {
    v = pic_cons(pic, va_arg(ap, pic_value), v);
  }
  v = pic_reverse(pic, v);

  va_end(ap);
  return pic_apply(pic, proc, v);
}

void print_code(pic_state *, struct pic_code);

#if VM_DEBUG
# define OPCODE_EXEC_HOOK print_code(pic, c)
#else
# define OPCODE_EXEC_HOOK ((void)0)
#endif

#if PIC_DIRECT_THREADED_VM
# define VM_LOOP JUMP;
# define CASE(x) L_##x: OPCODE_EXEC_HOOK;
# define NEXT c = *++pc; JUMP;
# define JUMP c = *pc; goto *oplabels[pc->insn];
# define VM_LOOP_END
#else
# define VM_LOOP for (;;) { c = *pc; switch (c.insn) {
# define CASE(x) case x:
# define NEXT pc++; break
# define JUMP break
# define VM_LOOP_END } }
#endif

#define PUSH(v) ((pic->sp >= pic->stend) ? abort() : (*pic->sp++ = (v)))
#define POP() (*--pic->sp)
#define POPN(i) (pic->sp -= (i))

#define PUSHCI() (++pic->ci)
#define POPCI() (pic->ci--)

pic_value
pic_apply(pic_state *pic, struct pic_proc *proc, pic_value argv)
{
  struct pic_code *pc, c;
  int ai = pic_gc_arena_preserve(pic);
  jmp_buf jmp, *prev_jmp = pic->jmp;
  size_t argc, i;
  struct pic_code boot[2];

#if PIC_DIRECT_THREADED_VM
  static void *oplabels[] = {
    &&L_OP_POP, &&L_OP_PUSHNIL, &&L_OP_PUSHTRUE, &&L_OP_PUSHFALSE, &&L_OP_PUSHFLOAT,
    &&L_OP_PUSHINT, &&L_OP_PUSHCHAR, &&L_OP_PUSHCONST,
    &&L_OP_GREF, &&L_OP_GSET, &&L_OP_LREF, &&L_OP_LSET, &&L_OP_CREF, &&L_OP_CSET,
    &&L_OP_JMP, &&L_OP_JMPIF, &&L_OP_CALL, &&L_OP_TAILCALL, &&L_OP_RET, &&L_OP_LAMBDA,
    &&L_OP_CONS, &&L_OP_CAR, &&L_OP_CDR, &&L_OP_NILP,
    &&L_OP_ADD, &&L_OP_SUB, &&L_OP_MUL, &&L_OP_DIV, &&L_OP_MINUS,
    &&L_OP_EQ, &&L_OP_LT, &&L_OP_LE, &&L_OP_STOP
  };
#endif

  if (setjmp(jmp) == 0) {
    pic->jmp = &jmp;
  }
  else {
    goto L_RAISE;
  }

  if (! pic_list_p(pic, argv)) {
    pic_error(pic, "argv must be a proper list");
  }

  argc = pic_length(pic, argv) + 1;

#if VM_DEBUG
  puts("== booting VM...");
  printf("  proc = ");
  pic_debug(pic, pic_obj_value(proc));
  puts("");
  printf("  argv = ");
  pic_debug(pic, argv);
  puts("");
  if (! proc->cfunc_p) {
    printf("  irep = ");
    print_irep(pic, proc->u.irep);
  }
  else {
    printf("  cfunc = %p\n", (void *)proc->u.cfunc);
  }
  puts("\nLet's go!");
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
  pc = boot;
  c = *pc;
  goto L_CALL;

  VM_LOOP {
    CASE(OP_POP) {
      POPN(1);
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
    CASE(OP_PUSHFLOAT) {
      PUSH(pic_float_value(c.u.f));
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
      PUSH(pic->pool[c.u.i]);
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

      env = pic->ci->env;
      while (depth--) {
	env = env->up;
      }
      PUSH(env->values[c.u.r.idx]);
      NEXT;
    }
    CASE(OP_CSET) {
      int depth = c.u.r.depth;
      struct pic_env *env;

      env = pic->ci->env;
      while (depth--) {
	env = env->up;
      }
      env->values[c.u.r.idx] = POP();
      NEXT;
    }
    CASE(OP_JMP) {
      pc += c.u.i;
      JUMP;
    }
    CASE(OP_JMPIF) {
      pic_value v;

      v = POP();
      if (! pic_false_p(v)) {
	pc += c.u.i;
	JUMP;
      }
      NEXT;
    }
    CASE(OP_CALL) {
      pic_value x, v;
      pic_callinfo *ci;
      struct pic_proc *proc;

    L_CALL:
      x = pic->sp[-c.u.i];
      if (! pic_proc_p(x)) {
#if DEBUG
	pic_debug(pic, x);
#endif
	pic->errmsg = "invalid application";
	goto L_RAISE;
      }
      proc = pic_proc_ptr(x);

#if VM_DEBUG
      puts("== calling proc...");
      printf("  proc = ");
      pic_debug(pic, pic_obj_value(proc));
      puts("");
      printf("  argv = ");
      pic_debug(pic, argv);
      puts("");
      if (! proc->cfunc_p) {
	printf("  irep = ");
	print_irep(pic, proc->u.irep);
      }
      else {
	printf("  cfunc = %p\n", (void *)proc->u.cfunc);
      }
      puts("");
#endif

      ci = PUSHCI();
      ci->argc = c.u.i;
      ci->pc = pc;
      ci->fp = pic->sp - c.u.i;
      ci->env = NULL;
      if (pic_proc_cfunc_p(x)) {
	v = proc->u.cfunc(pic);
	pic->sp = ci->fp;
	POPCI();
	PUSH(v);
	pic_gc_arena_restore(pic, ai);
	NEXT;
      }
      else {
	int i;
	pic_value rest;

	if (ci->argc != proc->u.irep->argc) {
	  if (! (proc->u.irep->varg && ci->argc >= proc->u.irep->argc)) {
	    pic->errmsg = "wrong number of arguments";
	    goto L_RAISE;
	  }
	}
	/* prepare rest args */
	if (proc->u.irep->varg) {
	  rest = pic_nil_value();
	  for (i = 0; i < ci->argc - proc->u.irep->argc; ++i) {
	    pic_gc_protect(pic, v = POP());
	    rest = pic_cons(pic, v, rest);
	  }
	  PUSH(rest);
	}
	/* prepare local variable area */
	if (proc->u.irep->localc > 0) {
	  int l = proc->u.irep->localc;
	  if (proc->u.irep->varg) {
	    --l;
	  }
	  for (i = 0; i < l; ++i) {
	    PUSH(pic_undef_value());
	  }
	}

	/* prepare env */
	if (proc->u.irep->cv_num == 0) {
	  ci->env = proc->env;
	}
	else {
	  ci->env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TT_ENV);
	  ci->env->up = proc->env;
	  ci->env->valuec = proc->u.irep->cv_num;
	  ci->env->values = (pic_value *)pic_calloc(pic, ci->env->valuec, sizeof(pic_value));
	  for (i = 0; i < ci->env->valuec; ++i) {
	    ci->env->values[i] = ci->fp[proc->u.irep->cv_tbl[i]];
	  }
	}

	pc = proc->u.irep->code;
	pic_gc_arena_restore(pic, ai);
	JUMP;
      }
    }
    CASE(OP_TAILCALL) {
      int argc;
      pic_value *argv;

      argc = c.u.i;
      argv = pic->sp - argc;
      for (i = 0; i < argc; ++i) {
	pic->ci->fp[i] = argv[i];
      }
      pic->sp = pic->ci->fp + argc;
      pc = POPCI()->pc;

      /* c is not changed */
      goto L_CALL;
    }
    CASE(OP_RET) {
      pic_value v;
      pic_callinfo *ci;

      if (pic->errmsg) {

      L_RAISE:
	goto L_STOP;
      }
      else {
	v = POP();
	ci = POPCI();
	pc = ci->pc;
	pic->sp = ci->fp;
	PUSH(v);
      }
      NEXT;
    }
    CASE(OP_LAMBDA) {
      struct pic_proc *proc;

      proc = pic_proc_new_irep(pic, pic->irep[c.u.i], pic->ci->env);
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
	pic->errmsg = #op " got non-number operands";		\
	goto L_RAISE;						\
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
	pic->errmsg = "unary - got a non-number operand";
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
	pic->errmsg = #op " got non-number operands";		\
	goto L_RAISE;						\
      }								\
      NEXT;							\
    }

    DEFINE_COMP_OP(OP_EQ, ==);
    DEFINE_COMP_OP(OP_LT, <);
    DEFINE_COMP_OP(OP_LE, <=);

    CASE(OP_STOP) {
      pic_value val;

    L_STOP:
      val = POP();

      pic->jmp = prev_jmp;
      if (pic->errmsg) {
	return pic_undef_value();
      }

#if VM_DEBUG
      puts("**VM END STATE**");
      printf("stbase\t= %p\nsp\t= %p\n", (void *)pic->stbase, (void *)pic->sp);
      printf("cibase\t= %p\nci\t= %p\n", (void *)pic->cibase, (void *)pic->ci);
      if (pic->stbase < pic->sp) {
	pic_value *sp;
	printf("* stack trace:");
	for (sp = pic->stbase; pic->sp != sp; ++sp) {
	  pic_debug(pic, *sp);
	  puts("");
	}
      }
      if (pic->stbase > pic->sp) {
	puts("*** stack underflow!");
      }
#endif

      pic_gc_protect(pic, val);

      return val;
    }
  } VM_LOOP_END;
}
