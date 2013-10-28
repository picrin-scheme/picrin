#include <stdio.h>
#include <stdarg.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/irep.h"

#define GET_OPERAND(pic,n) ((pic)->ci->fp[(n)])

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
    }

    /* in order to run out of all arguments passed to this function
       (i.e. do va_arg for each argument), optional argument existence
       check is done in every case closure */

    switch (c) {
    case '|':
      opt = true;
      break;
    case 'o':
      {
	pic_value *p;

	p = va_arg(ap, pic_value*);
	if (i < argc) {
	  *p = GET_OPERAND(pic,i);
	  i++;
	}
      }
      break;
    case 'f':
      {
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
      }
      break;
    case 'F':
      {
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
      }
      break;
    case 'I':
      {
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
      }
      break;
    case 's':
      {
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
      }
      break;
    default:
      {
	pic_error(pic, "pic_get_args: invalid argument specifier given");
      }
    }
  }
  if (argc > i) {
    pic_error(pic, "wrong number of arguments");
  }
  va_end(ap);
  return i;
}

#if PIC_DIRECT_THREADED_VM
# define VM_LOOP JUMP;
# define CASE(x) L_##x:
# define NEXT ++pc; JUMP;
# define JUMP goto *oplabels[pc->insn];
# define VM_LOOP_END
#else
# define VM_LOOP for (;;) { switch (pc->insn) {
# define CASE(x) case x:
# define NEXT pc++; break
# define JUMP break
# define VM_LOOP_END } }
#endif

#define PUSH(v) (*pic->sp++ = (v))
#define POP() (*--pic->sp)
#define POPN(i) (pic->sp -= (i))

#define PUSHCI() (++pic->ci)
#define POPCI() (pic->ci--)

pic_value
pic_apply(pic_state *pic, struct pic_proc *proc, pic_value argv)
{
  struct pic_code *pc;
  int ai = pic_gc_arena_preserve(pic);
  jmp_buf jmp;
  size_t argc, i;
  struct pic_code boot;

#if PIC_DIRECT_THREADED_VM
  static void *oplabels[] = {
    &&L_OP_POP, &&L_OP_PUSHNIL, &&L_OP_PUSHTRUE, &&L_OP_PUSHFALSE, &&L_OP_PUSHFLOAT,
    &&L_OP_PUSHINT, &&L_OP_PUSHCONST, &&L_OP_GREF, &&L_OP_GSET, &&L_OP_LREF,
    &&L_OP_LSET, &&L_OP_CREF, &&L_OP_CSET, &&L_OP_JMP, &&L_OP_JMPIF, &&L_OP_CALL,
    &&L_OP_RET, &&L_OP_LAMBDA, &&L_OP_CONS, &&L_OP_CAR, &&L_OP_CDR, &&L_OP_NILP,
    &&L_OP_ADD, &&L_OP_SUB, &&L_OP_MUL, &&L_OP_DIV,
    &&L_OP_EQ, &&L_OP_LT, &&L_OP_LE, &&L_OP_STOP
  };
#endif

  if (setjmp(jmp) == 0) {
    pic->jmp = &jmp;
  }
  else {
    goto L_RAISE;
  }

  argc = pic_length(pic, argv) + 1;

  PUSH(pic_obj_value(proc));
  for (i = 1; i < argc; ++i) {
    PUSH(pic_car(pic, argv));
    argv = pic_cdr(pic, argv);
  }

  /* boot! */
  boot.insn = OP_CALL;
  boot.u.i = argc;
  pc = &boot;
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
      PUSH(pic_float_value(pc->u.f));
      NEXT;
    }
    CASE(OP_PUSHINT) {
      PUSH(pic_int_value(pc->u.i));
      NEXT;
    }
    CASE(OP_PUSHCONST) {
      PUSH(pic->pool[pc->u.i]);
      NEXT;
    }
    CASE(OP_GREF) {
      PUSH(pic->globals[pc->u.i]);
      NEXT;
    }
    CASE(OP_GSET) {
      pic->globals[pc->u.i] = POP();
      NEXT;
    }
    CASE(OP_LREF) {
      PUSH(pic->ci->fp[pc->u.i]);
      NEXT;
    }
    CASE(OP_LSET) {
      pic->ci->fp[pc->u.i] = POP();
      NEXT;
    }
    CASE(OP_CREF) {
      int depth = pc->u.c.depth;
      struct pic_env *env;

      env = pic_proc_ptr(*pic->ci->fp)->env;
      while (depth--) {
	env = env->up;
      }
      PUSH(env->values[pc->u.c.idx]);
      NEXT;
    }
    CASE(OP_CSET) {
      int depth = pc->u.c.depth;
      struct pic_env *env;

      env = pic_proc_ptr(*pic->ci->fp)->env;
      while (depth--) {
	env = env->up;
      }
      env->values[pc->u.c.idx] = POP();
      NEXT;
    }
    CASE(OP_JMP) {
      pc += pc->u.i;
      JUMP;
    }
    CASE(OP_JMPIF) {
      pic_value v;

      v = POP();
      if (! pic_false_p(v)) {
	pc += pc->u.i;
	JUMP;
      }
      NEXT;
    }
    CASE(OP_CALL) {
      pic_value c, v;
      pic_callinfo *ci;
      struct pic_proc *proc;

    L_CALL:
      c = pic->sp[-pc->u.i];
      if (! pic_proc_p(c)) {
	pic->errmsg = "invalid application";
	goto L_RAISE;
      }
      proc = pic_proc_ptr(c);

      ci = PUSHCI();
      ci->argc = pc->u.i;
      ci->pc = pc;
      ci->fp = pic->sp - pc->u.i;
      if (pic_proc_cfunc_p(c)) {
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
	  /* prepare rest args */
	  rest = pic_nil_value();
	  for (i = 0; i < ci->argc - proc->u.irep->argc; ++i) {
	    pic_gc_protect(pic, v = POP());
	    rest = pic_cons(pic, v, rest);
	  }
	  PUSH(rest);
	}
	for (i = 0; i < proc->u.irep->argc; ++i) {
	  proc->env->values[i] = ci->fp[i];
	}
	pc = proc->u.irep->code;
	pic_gc_arena_restore(pic, ai);
	JUMP;
      }
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
      struct pic_env *env;
      int i;

      env = (struct pic_env *)pic_obj_alloc(pic, sizeof(struct pic_env), PIC_TT_ENV);
      env->num_val = pic->irep[pc->u.i]->argc;
      env->values = (pic_value *)pic_alloc(pic, sizeof(pic_value) * env->num_val);
      for (i = 0; i < env->num_val; ++i) {
	env->values[i] = pic_undef_value();
      }
      env->up = pic_proc_ptr(*pic->ci->fp)->env;

      proc = pic_proc_new(pic, pic->irep[pc->u.i], env);
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

#define DEFINE_ARITH_OP(opcode, op)				\
    CASE(opcode) {						\
      pic_value a, b;						\
      b = POP();						\
      a = POP();						\
      if (pic_int_p(a) && pic_int_p(b)) {			\
	PUSH(pic_int_value(pic_int(a) op pic_int(b)));		\
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

    DEFINE_ARITH_OP(OP_ADD, +);
    DEFINE_ARITH_OP(OP_SUB, -);
    DEFINE_ARITH_OP(OP_MUL, *);

    /* special care for (int / int) division */
    CASE(OP_DIV) {
      pic_value a, b;
      b = POP();
      a = POP();
      if (pic_int_p(a) && pic_int_p(b)) {
	PUSH(pic_float_value((double)pic_int(a) / pic_int(b)));
      }
      else if (pic_float_p(a) && pic_float_p(b)) {
	PUSH(pic_float_value(pic_float(a) / pic_float(b)));
      }
      else if (pic_int_p(a) && pic_float_p(b)) {
	PUSH(pic_float_value(pic_int(a) / pic_float(b)));
      }
      else if (pic_float_p(a) && pic_int_p(b)) {
	PUSH(pic_float_value(pic_float(a) / pic_int(b)));
      }
      else {
	pic->errmsg = "/ got non-number operands";
	goto L_RAISE;
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
      else if (pic_int_p(a) && pic_int_p(b)) {			\
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

      /* pop the first procedure */
      POPN(1);

      pic->jmp = NULL;
      if (pic->errmsg) {
	return pic_undef_value();
      }

#if VM_DEBUG
      puts("**VM END STATE**");
      printf("stbase\t= %p\nsp\t= %p\n", pic->stbase, pic->sp);
      printf("cibase\t= %p\nci\t= %p\n", pic->cibase, pic->ci);
      if (pic->stbase != pic->sp) {
	pic_value *sp;
	printf("* stack trace:");
	for (sp = pic->stbase; pic->sp != sp; ++sp) {
	  pic_debug(pic, *sp);
	  puts("");
	}
      }
#endif

      return val;
    }
  } VM_LOOP_END;
}
