#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "picrin.h"
#include "picrin/pair.h"
#include "picrin/proc.h"
#include "picrin/irep.h"

void
pic_get_args(pic_state *pic, const char *format, ...)
{
  char c;
  int i = -1;
  va_list ap;

  va_start(ap, format);
  while ((c = *format++)) {
    switch (c) {
    case 'o':
      {
	pic_value *p;

	p = va_arg(ap, pic_value*);
	*p = pic->sp[i];
	i--;
      }
      break;
    case 'f':
      {
	double *f;

	f = va_arg(ap, double *);
	*f = pic_float(pic->sp[i]);
	i--;
      }
      break;
    }
  }
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

#define PUSH(v) (*++pic->sp = (v))
#define POP() (*pic->sp--)
#define POPN(i) ((void)(pic->sp-=i))

#define PUSHCI() (++pic->ci)
#define POPCI() (pic->ci--)

pic_value
pic_run(pic_state *pic, struct pic_proc *proc, pic_value args)
{
  struct pic_code *pc;
  pic_value val;
  int ai = pic_gc_arena_preserve(pic);

#if PIC_DIRECT_THREADED_VM
  static void *oplabels[] = {
    &&L_OP_POP, &&L_OP_PUSHNIL, &&L_OP_PUSHTRUE, &&L_OP_PUSHFALSE, &&L_OP_PUSHNUM,
    &&L_OP_GREF, &&L_OP_GSET, &&L_OP_LREF, &&L_OP_JMP, &&L_OP_JMPIF,
    &&L_OP_CALL, &&L_OP_RET, &&L_OP_LAMBDA, &&L_OP_CONS, &&L_OP_CAR, &&L_OP_CDR,
    &&L_OP_NILP, &&L_OP_ADD, &&L_OP_SUB, &&L_OP_MUL, &&L_OP_DIV, &&L_OP_STOP
  };
#endif

  pc = proc->u.irep->code;

  /* adjust call frame */
  pic->sp[0] = pic_obj_value(proc);
  pic->ci->argc = 1;
  pic->ci->pc = NULL;
  pic->ci->sp = NULL;

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
    CASE(OP_PUSHNUM) {
      PUSH(pic_float_value(pc->u.f));
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
      PUSH(pic->ci->sp[pc->u.i]);
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

      c = pic->sp[0];
      proc = pic_proc_ptr(c);
      ci = PUSHCI();
      ci->argc = pc->u.i;
      ci->pc = pc;
      ci->sp = pic->sp;
      if (pic_proc_cfunc_p(c)) {
	v = proc->u.cfunc(pic);
	pic->sp -= ci->argc;
	POPCI();
	PUSH(v);
	pic_gc_arena_restore(pic, ai);
	NEXT;
      }
      else {
	pc = proc->u.irep->code;
	pic_gc_arena_restore(pic, ai);
	JUMP;
      }
    }
    CASE(OP_RET) {
      pic_value v;
      pic_callinfo *ci;

      v = POP();
      ci = POPCI();
      pc = ci->pc;
      pic->sp -= ci->argc;
      PUSH(v);
      NEXT;
    }
    CASE(OP_LAMBDA) {
      struct pic_proc *proc;

      proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc *), PIC_TT_PROC);
      proc->cfunc_p = false;
      proc->u.irep = pic->irep[pc->u.i];
      PUSH(pic_obj_value(proc));
      pic_gc_arena_restore(pic, ai);
      NEXT;
    }
    CASE(OP_CONS) {
      pic_value a, b;
      pic_gc_protect(pic, a = POP());
      pic_gc_protect(pic, b = POP());
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
    CASE(OP_ADD) {
      pic_value a, b;
      a = POP();
      b = POP();
      PUSH(pic_float_value(pic_float(a) + pic_float(b)));
      NEXT;
    }
    CASE(OP_SUB) {
      pic_value a, b;
      a = POP();
      b = POP();
      PUSH(pic_float_value(pic_float(a) - pic_float(b)));
      NEXT;
    }
    CASE(OP_MUL) {
      pic_value a, b;
      a = POP();
      b = POP();
      PUSH(pic_float_value(pic_float(a) * pic_float(b)));
      NEXT;
    }
    CASE(OP_DIV) {
      pic_value a, b;
      a = POP();
      b = POP();
      PUSH(pic_float_value(pic_float(a) / pic_float(b)));
      NEXT;
    }
    CASE(OP_STOP) {
      goto STOP;
    }
  } VM_LOOP_END;

 STOP:
  val = POP();

#if VM_DEBUG
  puts("**VM END STATE**");
  printf("stbase = %p\nsp = %p\n", pic->stbase, pic->sp);
  printf("cibase = %p\nci = %p\n", pic->cibase, pic->ci);
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

void
pic_raise(pic_state *pic, const char *str)
{
  puts(str);
  abort();
}
