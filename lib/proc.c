/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"
#include "state.h"
#include "vm.h"

pic_value
pic_lambda(pic_state *pic, pic_func_t f, int n, ...)
{
  pic_value proc;
  va_list ap;

  va_start(ap, n);
  proc = pic_vlambda(pic, f, n, ap);
  va_end(ap);
  return proc;
}

pic_value
pic_vlambda(pic_state *pic, pic_func_t f, int n, va_list ap)
{
  pic_value *env = pic_alloca(pic, sizeof(pic_value) * n);
  int i;

  for (i = 0; i < n; ++i) {
    env[i] = va_arg(ap, pic_value);
  }
  return pic_make_proc_func(pic, f, n, env);
}

pic_value
pic_make_proc_func(pic_state *pic, pic_func_t func, int n, pic_value *env)
{
  struct proc *proc;
  int i;

  proc = (struct proc *)pic_obj_alloc(pic, offsetof(struct proc, locals) + sizeof(pic_value) * n, PIC_TYPE_PROC_FUNC);
  proc->u.f.func = func;
  proc->u.f.localc = n;
  for (i = 0; i < n; ++i) {
    proc->locals[i] = env[i];
  }
  return obj_value(pic, proc);
}

pic_value
pic_make_proc_irep(pic_state *pic, struct irep *irep, struct frame *fp)
{
  struct proc *proc;

  proc = (struct proc *)pic_obj_alloc(pic, offsetof(struct proc, locals), PIC_TYPE_PROC_IREP);
  proc->u.i.irep = irep;
  proc->u.i.fp = fp;
  return obj_value(pic, proc);
}

PIC_NORETURN static void
arg_error(pic_state *pic, int actual, bool varg, int expected)
{
  const char *msg;

  msg = pic_str(pic, pic_strf_value(pic, "wrong number of arguments (%d for %s%d)", actual, (varg ? "at least " : ""), expected), NULL);

  pic_error(pic, msg, 0);
}

#define GET_PROC(pic) (pic->ci->fp[0])
#define GET_ARG(pic,n) (pic->ci->fp[(n)+1])

/**
 * char type                    desc.
 * ---- ----                    ----
 *  o   pic_value *             object
 *  i   int *                   int
 *  I   int *, bool *           int with exactness
 *  f   double *                float
 *  F   double *, bool *        float with exactness
 *  c   char *                  char
 *  z   char **                 c string
 *  b   unsigned char *, int *  bytevector
 *  u   void **, const pic_data_type *  user data type
 *  m   pic_value *             symbol
 *  v   pic_value *             vector
 *  s   pic_value *             string
 *  l   pic_value *             lambda
 *  p   pic_value *             port
 *  d   pic_value *             dictionary
 *  r   pic_value *             record
 *
 *  +                           aliasing operator
 *  |                           optional operator
 *  *   int *, pic_value **     variable length operator
 * ---- ----                    ----
 */

int
pic_get_args(pic_state *pic, const char *format, ...)
{
  char c;
  const char *p = format;
  int paramc = 0, optc = 0;
  bool proc = 0, rest = 0, opt = 0;
  int i, argc = pic->ci->argc - 1;
  va_list ap;

  /* parse format */
  if ((c = *p) != '\0') {
    if (c == '&') {
      proc = 1;
      p++;
    }
    while ((c = *p++) != '\0') {
      if (c == '+')
        continue;
      if (c == '|') {
        opt = 1; break;
      } else if (c == '*') {
        rest = 1; break;
      }
      paramc++;
    }
    if (opt) {
      while ((c = *p++) != '\0') {
        if (c == '+')
          continue;
        if (c == '*') {
          rest = 1; break;
        }
        optc++;
      }
    }
    if (rest) c = *p++;
    assert(opt <= optc); /* at least 1 char after '|'? */
    assert(c == '\0');   /* no extra chars? */
  }

  if (argc < paramc || (paramc + optc < argc && ! rest)) {
    arg_error(pic, argc, rest, paramc);
  }

  va_start(ap, format);

  /* dispatch */
  if (proc) {
    pic_value *proc;

    proc = va_arg(ap, pic_value *);
    *proc = GET_PROC(pic);
    format++;                   /* skip '&' */
  }
  for (i = 0; i < argc && i < paramc + optc; ++i) {

    c = *format++;
    if (c == '|') {
      c = *format++;
    }

    switch (c) {
    case 'o': {
      pic_value *p;

      p = va_arg(ap, pic_value*);
      *p = GET_ARG(pic, i);
      break;
    }

    case 'u': {
      void **data;
      const pic_data_type *type;
      pic_value v;

      data = va_arg(ap, void **);
      type = va_arg(ap, const pic_data_type *);
      v = GET_ARG(pic, i);
      if (pic_data_p(pic, v, type)) {
        *data = pic_data(pic, v);
      }
      else {
        const char *msg;
        msg = pic_str(pic, pic_strf_value(pic, "pic_get_args: data type \"%s\" required", type->type_name), NULL);
        pic_error(pic, msg, 1, v);
      }
      break;
    }

    case 'b': {
      unsigned char **buf;
      int *len;
      pic_value v;

      buf = va_arg(ap, unsigned char **);
      len = va_arg(ap, int *);
      v = GET_ARG(pic, i);
      if (pic_blob_p(pic, v)) {
        unsigned char *tmp = pic_blob(pic, v, len);
        if (buf) *buf = tmp;
      }
      else {
        pic_error(pic, "pic_get_args: bytevector required", 1, v);
      }
      break;
    }

#define NUM_CASE(c1, c2, ctype)                                         \
      case c1: case c2: {                                               \
        ctype *n;                                                       \
        bool *e, dummy;                                                 \
        pic_value v;                                                    \
                                                                        \
        n = va_arg(ap, ctype *);                                        \
        e = (c == c2 ? va_arg(ap, bool *) : &dummy);                    \
                                                                        \
        v = GET_ARG(pic, i);                                            \
        switch (pic_type(pic, v)) {                                     \
        case PIC_TYPE_FLOAT:                                            \
          *n = pic_float(pic, v);                                       \
          *e = false;                                                   \
          break;                                                        \
        case PIC_TYPE_INT:                                              \
          *n = pic_int(pic, v);                                         \
          *e = true;                                                    \
          break;                                                        \
        default:                                                        \
          pic_error(pic, "pic_get_args: float or int required", 1, v);  \
        }                                                               \
        break;                                                          \
      }

    NUM_CASE('i', 'I', int)
    NUM_CASE('f', 'F', double)

#define VAL_CASE(c, type, ctype, conv)                                  \
      case c: {                                                         \
        ctype *ptr;                                                     \
        pic_value v;                                                    \
                                                                        \
        ptr = va_arg(ap, ctype *);                                      \
        v = GET_ARG(pic, i);                                            \
        if (pic_## type ##_p(pic, v)) {                                 \
          *ptr = conv;                                                  \
        }                                                               \
        else {                                                          \
          pic_error(pic, "pic_get_args: " #type " required", 1, v);     \
        }                                                               \
        break;                                                          \
      }

    VAL_CASE('c', char, char, pic_char(pic, v))
    VAL_CASE('z', str, const char *, pic_str(pic, v, NULL))

#define OBJ_CASE(c, type) VAL_CASE(c, type, pic_value, v)

    OBJ_CASE('m', sym)
    OBJ_CASE('s', str)
    OBJ_CASE('l', proc)
    OBJ_CASE('v', vec)
    OBJ_CASE('d', dict)
#define pic_port_p(pic,v) pic_port_p(pic,v,NULL)
    OBJ_CASE('p', port)
#undef pic_port_p
    OBJ_CASE('r', rec)

    default:
      pic_error(pic, "pic_get_args: invalid argument specifier given", 1, pic_char_value(pic, c));
    }

    if (*format == '+') {
      pic_value *p;
      format++;
      p = va_arg(ap, pic_value *);
      *p = GET_ARG(pic, i);
    }
  }
  if (rest) {
    int *n;
    pic_value **argv;

    n = va_arg(ap, int *);
    argv = va_arg(ap, pic_value **);
    *n = argc - i;
    *argv = &GET_ARG(pic, i);
  }

  va_end(ap);

  return argc;
}

pic_value
pic_closure_ref(pic_state *pic, int n)
{
  pic_value self = GET_PROC(pic);

  if (n < 0 || proc_ptr(pic, self)->u.f.localc <= n) {
    pic_error(pic, "pic_closure_ref: index out of range", 1, pic_int_value(pic, n));
  }
  return proc_ptr(pic, self)->locals[n];
}

void
pic_closure_set(pic_state *pic, int n, pic_value v)
{
  pic_value self = GET_PROC(pic);

  if (n < 0 || proc_ptr(pic, self)->u.f.localc <= n) {
    pic_error(pic, "pic_closure_ref: index out of range", 1, pic_int_value(pic, n));
  }
  proc_ptr(pic, self)->locals[n] = v;
}

pic_value
pic_call(pic_state *pic, pic_value proc, int n, ...)
{
  pic_value r;
  va_list ap;

  va_start(ap, n);
  r = pic_vcall(pic, proc, n, ap);
  va_end(ap);
  return r;
}

pic_value
pic_vcall(pic_state *pic, pic_value proc, int n, va_list ap)
{
  pic_value *args = pic_alloca(pic, sizeof(pic_value) * n);
  int i;

  for (i = 0; i < n; ++i) {
    args[i] = va_arg(ap, pic_value);
  }
  return pic_apply(pic, proc, n, args);
}

static void
vm_push_cxt(pic_state *pic)
{
  struct callinfo *ci = pic->ci;

  ci->cxt = (struct frame *)pic_obj_alloc(pic, offsetof(struct frame, storage) + sizeof(pic_value) * ci->regc, PIC_TYPE_FRAME);
  ci->cxt->up = ci->up;
  ci->cxt->regc = ci->regc;
  ci->cxt->regs = ci->regs;
}

static void
vm_tear_off(struct callinfo *ci)
{
  struct frame *cxt;
  int i;

  assert(ci->cxt != NULL);

  cxt = ci->cxt;

  if (cxt->regs == cxt->storage) {
    return;                     /* is torn off */
  }
  for (i = 0; i < cxt->regc; ++i) {
    cxt->storage[i] = cxt->regs[i];
  }
  cxt->regs = cxt->storage;
}

void
pic_vm_tear_off(pic_state *pic)
{
  struct callinfo *ci;

  for (ci = pic->ci; ci > pic->cibase; ci--) {
    if (ci->cxt != NULL) {
      vm_tear_off(ci);
    }
  }
}

/* for arithmetic instructions */
pic_value pic_add(pic_state *, pic_value, pic_value);
pic_value pic_sub(pic_state *, pic_value, pic_value);
pic_value pic_mul(pic_state *, pic_value, pic_value);
pic_value pic_div(pic_state *, pic_value, pic_value);
bool pic_eq(pic_state *, pic_value, pic_value);
bool pic_lt(pic_state *, pic_value, pic_value);
bool pic_le(pic_state *, pic_value, pic_value);
bool pic_gt(pic_state *, pic_value, pic_value);
bool pic_ge(pic_state *, pic_value, pic_value);

pic_value
pic_apply(pic_state *pic, pic_value proc, int argc, pic_value *argv)
{
  struct code c;
  size_t ai = pic_enter(pic);
  struct code boot[2];
  int i;

#define PUSH(v) ((*pic->sp = (v)), pic->sp++)
#define POP() (*--pic->sp)

#define PUSHCI() (++pic->ci)
#define POPCI() (pic->ci--)

  PUSH(proc);

  for (i = 0; i < argc; ++i) {
    PUSH(argv[i]);
  }

  /* boot! */
  boot[0].insn = OP_CALL;
  boot[0].a = argc + 1;
  boot[1].insn = OP_STOP;
  pic->ip = boot;

#if PIC_DIRECT_THREADED_VM
# define VM_LOOP JUMP;
# define CASE(x) L_##x:
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

#if PIC_DIRECT_THREADED_VM
  static const void *oplabels[] = {
    &&L_OP_NOP, &&L_OP_POP, &&L_OP_PUSHUNDEF, &&L_OP_PUSHNIL, &&L_OP_PUSHTRUE,
    &&L_OP_PUSHFALSE, &&L_OP_PUSHINT, &&L_OP_PUSHFLOAT,
    &&L_OP_PUSHCHAR, &&L_OP_PUSHEOF, &&L_OP_PUSHCONST,
    &&L_OP_GREF, &&L_OP_GSET, &&L_OP_LREF, &&L_OP_LSET, &&L_OP_CREF, &&L_OP_CSET,
    &&L_OP_JMP, &&L_OP_JMPIF, &&L_OP_NOT, &&L_OP_CALL, &&L_OP_TAILCALL, &&L_OP_RET,
    &&L_OP_LAMBDA, &&L_OP_CONS, &&L_OP_CAR, &&L_OP_CDR, &&L_OP_NILP,
    &&L_OP_SYMBOLP, &&L_OP_PAIRP,
    &&L_OP_ADD, &&L_OP_SUB, &&L_OP_MUL, &&L_OP_DIV,
    &&L_OP_EQ, &&L_OP_LT, &&L_OP_LE, &&L_OP_GT, &&L_OP_GE, &&L_OP_STOP
  };
#endif

  VM_LOOP {
    CASE(OP_NOP) {
      NEXT;
    }
    CASE(OP_POP) {
      (void)(POP());
      NEXT;
    }
    CASE(OP_PUSHUNDEF) {
      PUSH(pic_undef_value(pic));
      NEXT;
    }
    CASE(OP_PUSHNIL) {
      PUSH(pic_nil_value(pic));
      NEXT;
    }
    CASE(OP_PUSHTRUE) {
      PUSH(pic_true_value(pic));
      NEXT;
    }
    CASE(OP_PUSHFALSE) {
      PUSH(pic_false_value(pic));
      NEXT;
    }
    CASE(OP_PUSHINT) {
      PUSH(pic_int_value(pic, pic->ci->irep->ints[c.a]));
      NEXT;
    }
    CASE(OP_PUSHFLOAT) {
      PUSH(pic_float_value(pic, pic->ci->irep->nums[c.a]));
      NEXT;
    }
    CASE(OP_PUSHCHAR) {
      PUSH(pic_char_value(pic, pic->ci->irep->ints[c.a]));
      NEXT;
    }
    CASE(OP_PUSHEOF) {
      PUSH(pic_eof_object(pic));
      NEXT;
    }
    CASE(OP_PUSHCONST) {
      PUSH(obj_value(pic, pic->ci->irep->pool[c.a]));
      NEXT;
    }
    CASE(OP_GREF) {
      PUSH(pic_global_ref(pic, obj_value(pic, pic->ci->irep->pool[c.a])));
      NEXT;
    }
    CASE(OP_GSET) {
      pic_global_set(pic, obj_value(pic, pic->ci->irep->pool[c.a]), POP());
      PUSH(pic_undef_value(pic));
      NEXT;
    }
    CASE(OP_LREF) {
      struct callinfo *ci = pic->ci;
      struct irep *irep = ci->irep;

      if (ci->cxt != NULL && ci->cxt->regs == ci->cxt->storage) {
        if (c.a >= irep->argc + irep->localc) {
          PUSH(ci->cxt->regs[c.a - (ci->regs - ci->fp)]);
          NEXT;
        }
      }
      PUSH(pic->ci->fp[c.a]);
      NEXT;
    }
    CASE(OP_LSET) {
      struct callinfo *ci = pic->ci;
      struct irep *irep = ci->irep;

      if (ci->cxt != NULL && ci->cxt->regs == ci->cxt->storage) {
        if (c.a >= irep->argc + irep->localc) {
          ci->cxt->regs[c.a - (ci->regs - ci->fp)] = POP();
          PUSH(pic_undef_value(pic));
          NEXT;
        }
      }
      pic->ci->fp[c.a] = POP();
      PUSH(pic_undef_value(pic));
      NEXT;
    }
    CASE(OP_CREF) {
      int depth = c.a;
      struct frame *cxt;

      cxt = pic->ci->up;
      while (--depth) {
	cxt = cxt->up;
      }
      PUSH(cxt->regs[c.b]);
      NEXT;
    }
    CASE(OP_CSET) {
      int depth = c.a;
      struct frame *cxt;

      cxt = pic->ci->up;
      while (--depth) {
	cxt = cxt->up;
      }
      cxt->regs[c.b] = POP();
      PUSH(pic_undef_value(pic));
      NEXT;
    }
    CASE(OP_JMP) {
      pic->ip += c.a;
      JUMP;
    }
    CASE(OP_JMPIF) {
      pic_value v;

      v = POP();
      if (! pic_false_p(pic, v)) {
	pic->ip += c.a;
	JUMP;
      }
      NEXT;
    }
    CASE(OP_CALL) {
      pic_value x, v;
      struct callinfo *ci;
      struct proc *proc;

      if (c.a == -1) {
        pic->sp += pic->ci[1].retc - 1;
        c.a = pic->ci[1].retc + 1;
      }

    L_CALL:
      x = pic->sp[-c.a];
      if (! pic_proc_p(pic, x)) {
	pic_error(pic, "invalid application", 1, x);
      }
      proc = proc_ptr(pic, x);

      if (pic->sp >= pic->stend) {
        pic_panic(pic, "VM stack overflow");
      }

      ci = PUSHCI();
      ci->argc = c.a;
      ci->retc = 1;
      ci->ip = pic->ip;
      ci->fp = pic->sp - c.a;
      ci->irep = NULL;
      ci->cxt = NULL;
      if (proc->tt == PIC_TYPE_PROC_FUNC) {

        /* invoke! */
        v = proc->u.f.func(pic);
        pic->sp[0] = v;
        pic->sp += pic->ci->retc;

        pic_leave(pic, ai);
        goto L_RET;
      }
      else {
        struct irep *irep = proc->u.i.irep;
	int i;
	pic_value rest;

        ci->irep = irep;
	if (ci->argc != irep->argc) {
	  if (! (irep->varg && ci->argc >= irep->argc)) {
            arg_error(pic, ci->argc - 1, irep->varg, irep->argc - 1);
	  }
	}
	/* prepare rest args */
	if (irep->varg) {
	  rest = pic_nil_value(pic);
	  for (i = 0; i < ci->argc - irep->argc; ++i) {
	    pic_protect(pic, v = POP());
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
	    PUSH(pic_undef_value(pic));
	  }
	}

	/* prepare cxt */
        ci->up = proc->u.i.fp;
        ci->regc = irep->capturec;
        ci->regs = ci->fp + irep->argc + irep->localc;

	pic->ip = irep->code;
	pic_leave(pic, ai);
	JUMP;
      }
    }
    CASE(OP_TAILCALL) {
      int i, argc;
      pic_value *argv;
      struct callinfo *ci;

      if (pic->ci->cxt != NULL) {
        vm_tear_off(pic->ci);
      }

      if (c.a == -1) {
        pic->sp += pic->ci[1].retc - 1;
        c.a = pic->ci[1].retc + 1;
      }

      argc = c.a;
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
      struct callinfo *ci;

      if (pic->ci->cxt != NULL) {
        vm_tear_off(pic->ci);
      }

      assert(pic->ci->retc == 1);

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
      if (pic->ci->cxt == NULL) {
        vm_push_cxt(pic);
      }

      PUSH(pic_make_proc_irep(pic, pic->ci->irep->irep[c.a], pic->ci->cxt));
      pic_leave(pic, ai);
      NEXT;
    }

    CASE(OP_CONS) {
      pic_value a, b;
      pic_protect(pic, b = POP());
      pic_protect(pic, a = POP());
      PUSH(pic_cons(pic, a, b));
      pic_leave(pic, ai);
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
      PUSH(pic_bool_value(pic, pic_nil_p(pic, p)));
      NEXT;
    }
    CASE(OP_SYMBOLP) {
      pic_value p;
      p = POP();
      PUSH(pic_bool_value(pic, pic_sym_p(pic, p)));
      NEXT;
    }
    CASE(OP_PAIRP) {
      pic_value p;
      p = POP();
      PUSH(pic_bool_value(pic, pic_pair_p(pic, p)));
      NEXT;
    }
    CASE(OP_NOT) {
      pic_value v;
      v = pic_false_p(pic, POP()) ? pic_true_value(pic) : pic_false_value(pic);
      PUSH(v);
      NEXT;
    }

    CASE(OP_ADD) {
      pic_value a, b;
      b = POP();
      a = POP();
      PUSH(pic_add(pic, a, b));
      NEXT;
    }
    CASE(OP_SUB) {
      pic_value a, b;
      b = POP();
      a = POP();
      PUSH(pic_sub(pic, a, b));
      NEXT;
    }
    CASE(OP_MUL) {
      pic_value a, b;
      b = POP();
      a = POP();
      PUSH(pic_mul(pic, a, b));
      NEXT;
    }
    CASE(OP_DIV) {
      pic_value a, b;
      b = POP();
      a = POP();
      PUSH(pic_div(pic, a, b));
      NEXT;
    }
    CASE(OP_EQ) {
      pic_value a, b;
      b = POP();
      a = POP();
      PUSH(pic_bool_value(pic, pic_eq(pic, a, b)));
      NEXT;
    }
    CASE(OP_LE) {
      pic_value a, b;
      b = POP();
      a = POP();
      PUSH(pic_bool_value(pic, pic_le(pic, a, b)));
      NEXT;
    }
    CASE(OP_LT) {
      pic_value a, b;
      b = POP();
      a = POP();
      PUSH(pic_bool_value(pic, pic_lt(pic, a, b)));
      NEXT;
    }
    CASE(OP_GE) {
      pic_value a, b;
      b = POP();
      a = POP();
      PUSH(pic_bool_value(pic, pic_ge(pic, a, b)));
      NEXT;
    }
    CASE(OP_GT) {
      pic_value a, b;
      b = POP();
      a = POP();
      PUSH(pic_bool_value(pic, pic_gt(pic, a, b)));
      NEXT;
    }

    CASE(OP_STOP) {
      return pic_protect(pic, POP());
    }
  } VM_LOOP_END;
}

pic_value
pic_applyk(pic_state *pic, pic_value proc, int argc, pic_value *args)
{
  static const struct code iseq[2] = { { OP_NOP, 0, 0 }, { OP_TAILCALL, -1, 0 } };
  pic_value *sp;
  struct callinfo *ci;
  int i;

  *pic->sp++ = proc;

  sp = pic->sp;
  for (i = 0; i < argc; ++i) {
    *sp++ = args[i];
  }

  ci = PUSHCI();
  ci->ip = iseq;
  ci->fp = pic->sp;
  ci->retc = (int)argc;

  if (ci->retc == 0) {
    return pic_undef_value(pic);
  } else {
    return args[0];
  }
}

static pic_value
pic_proc_proc_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic, pic_proc_p(pic, v));
}

static pic_value
pic_proc_apply(pic_state *pic)
{
  pic_value proc, *args, *arg_list;
  int argc, n, i;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  if (argc == 0) {
    pic_error(pic, "apply: wrong number of arguments", 0);
  }

  n = argc - 1 + pic_length(pic, args[argc - 1]);

  arg_list = pic_alloca(pic, sizeof(pic_value) * n);
  for (i = 0; i < argc - 1; ++i) {
    arg_list[i] = args[i];
  }
  while (i < n) {
    arg_list[i] = pic_list_ref(pic, args[argc - 1], i - argc + 1);
    i++;
  }
  return pic_applyk(pic, proc, n, arg_list);
}

void
pic_init_proc(pic_state *pic)
{
  pic_defun(pic, "procedure?", pic_proc_proc_p);
  pic_defun(pic, "apply", pic_proc_apply);
}
