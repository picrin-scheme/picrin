/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/opcode.h"

#define MIN(x,y) ((x) < (y) ? (x) : (y))

#define GET_OPERAND(pic,n) ((pic)->ci->fp[(n)])

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
 *  s   pic_str **              string object
 *  m   pic_sym **              symbol
 *  v   pic_vec **              vector object
 *  b   pic_blob **             bytevector object
 *  l   struct pic_proc **      lambda object
 *  p   struct pic_port **      port object
 *  d   struct pic_dict **      dictionary object
 *  e   struct pic_error **     error object
 *  r   struct pic_record **    record object
 *
 *  |                           optional operator
 *  *   int *, pic_value **  variable length operator
 */

int
pic_get_args(pic_state *pic, const char *format, ...)
{
  char c;
  int paramc = 0, optc = 0;
  int i, argc = pic->ci->argc - 1;
  va_list ap;
  bool proc = false, rest = false, opt = false;

  /* parse format */
  if ((c = *format) != '\0') {
    if (c == '&') {
      proc = true;
      format++;                 /* forget about '&' */
    }
    for (paramc = 0, c = *format; c;  c = format[++paramc]) {
      if (c == '|') {
        opt = true;
        break;
      } else if (c == '*') {
        rest = true;
        break;
      }
    }
    for (optc = 0; opt && c; c = format[paramc + opt + ++optc]) {
      if (c == '*') {
        rest = true;
        break;
      }
    }
    assert((opt ? 1 : 0) <= optc); /* at least 1 char after '|'? */
    assert(format[paramc + opt + optc + rest] == '\0'); /* no extra chars? */
  }

  if (argc < paramc || (paramc + optc < argc && ! rest)) {
    pic_errorf(pic, "pic_get_args: wrong number of arguments (%d for %s%d)", argc, rest? "at least " : "", paramc);
  }

  va_start(ap, format);

  /* dispatch */
  if (proc) {
    struct pic_proc **proc;

    proc = va_arg(ap, struct pic_proc **);
    *proc = pic_proc_ptr(GET_OPERAND(pic, 0));
  }
  for (i = 1; i <= MIN(paramc + optc, argc); ++i) {

    c = *format++;
    if (c == '|') {
      c = *format++;
    }

    switch (c) {
    case 'o': {
      pic_value *p;

      p = va_arg(ap, pic_value*);
      *p = GET_OPERAND(pic, i);
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
        v = GET_OPERAND(pic, i);                                        \
        switch (pic_type(v)) {                                          \
        case PIC_TT_FLOAT:                                              \
          *n = pic_float(v);                                            \
          *e = false;                                                   \
          break;                                                        \
        case PIC_TT_INT:                                                \
          *n = pic_int(v);                                              \
          *e = true;                                                    \
          break;                                                        \
        default:                                                        \
          pic_errorf(pic, "pic_get_args: expected float or int, but got ~s", v); \
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
        v = GET_OPERAND(pic, i);                                        \
        if (pic_## type ##_p(v)) {                                      \
          *ptr = conv;                                                  \
        }                                                               \
        else {                                                          \
          pic_errorf(pic, "pic_get_args: expected " #type ", but got ~s", v); \
        }                                                               \
        break;                                                          \
      }

    VAL_CASE('c', char, char, pic_char(v))
    VAL_CASE('z', str, const char *, pic_str_cstr(pic, pic_str_ptr(v)))

#define PTR_CASE(c, type, ctype)                        \
      VAL_CASE(c, type, ctype, pic_## type ##_ptr(v))

    PTR_CASE('s', str, pic_str *)
    PTR_CASE('m', sym, pic_sym *)
    PTR_CASE('v', vec, pic_vec *)
    PTR_CASE('b', blob, pic_blob *)
    PTR_CASE('l', proc, struct pic_proc *)
    PTR_CASE('p', port, struct pic_port *)
    PTR_CASE('d', dict, struct pic_dict *)
    PTR_CASE('e', error, struct pic_error *)
    PTR_CASE('r', rec, struct pic_record *)

    default:
      pic_errorf(pic, "pic_get_args: invalid argument specifier '%c' given", c);
    }
  }
  if (rest) {
    int *n;
    pic_value **argv;

    n = va_arg(ap, int *);
    argv = va_arg(ap, pic_value **);
    *n = argc - (i - 1);
    *argv = &GET_OPERAND(pic, i);
  }

  va_end(ap);

  return argc;
}

static pic_value
vm_gref(pic_state *pic, pic_sym *uid)
{
  if (! pic_reg_has(pic, pic->globals, uid)) {
    pic_reg_set(pic, pic->globals, uid, pic_invalid_value());

    pic_errorf(pic, "uninitialized global variable: %s", pic_symbol_name(pic, uid));

    return pic_invalid_value();
  }

  return pic_reg_ref(pic, pic->globals, uid);
}

static void
vm_gset(pic_state *pic, pic_sym *uid, pic_value value)
{
  pic_reg_set(pic, pic->globals, uid, value);
}

static void
vm_push_cxt(pic_state *pic)
{
  pic_callinfo *ci = pic->ci;

  ci->cxt = (struct pic_context *)pic_obj_alloc(pic, sizeof(struct pic_context) + sizeof(pic_value) * ci->regc, PIC_TT_CXT);
  ci->cxt->up = ci->up;
  ci->cxt->regc = ci->regc;
  ci->cxt->regs = ci->regs;
}

static void
vm_tear_off(pic_callinfo *ci)
{
  struct pic_context *cxt;
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
  pic_callinfo *ci;

  for (ci = pic->ci; ci > pic->cibase; ci--) {
    if (ci->cxt != NULL) {
      vm_tear_off(ci);
    }
  }
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
    short i;                                                            \
    puts("\n== calling proc...");                                       \
    printf("  proc = ");                                                \
    pic_debug(pic, pic_obj_value(proc));                                \
    puts("");                                                           \
    printf("  argv = (");                                               \
    for (i = 1; i < c.u.i; ++i) {                                       \
      if (i > 1)                                                        \
        printf(" ");                                                    \
      pic_debug(pic, pic->sp[-c.u.i + i]);                              \
    }                                                                   \
    puts(")");                                                          \
    if (! pic_proc_func_p(proc)) {                                      \
      printf("  irep = %p\n", proc->u.i.irep);                          \
      printf("  name = %s\n", pic_symbol_name(pic, pic_proc_name(proc))); \
      pic_dump_irep(proc->u.i.irep);                                    \
    }                                                                   \
    else {                                                              \
      printf("  cfunc = %p\n", (void *)proc->u.f.func);                 \
      printf("  name = %s\n", pic_symbol_name(pic, pic_proc_name(proc))); \
    }                                                                   \
    puts("== end\n");                                                   \
  } while (0)
#else
# define VM_CALL_PRINT
#endif

pic_value
pic_apply(pic_state *pic, struct pic_proc *proc, int argc, pic_value *argv)
{
  pic_code c;
  size_t ai = pic_gc_arena_preserve(pic);
  pic_code boot[2];
  int i;

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

#if VM_DEBUG
  pic_value *stbase;
  pic_callinfo *cibase;
#endif

  PUSH(pic_obj_value(proc));

  for (i = 0; i < argc; ++i) {
    PUSH(argv[i]);
  }

  VM_BOOT_PRINT;

  /* boot! */
  boot[0].insn = OP_CALL;
  boot[0].a = argc + 1;
  boot[1].insn = OP_STOP;
  pic->ip = boot;

  VM_LOOP {
    CASE(OP_NOP) {
      NEXT;
    }
    CASE(OP_POP) {
      (void)(POP());
      NEXT;
    }
    CASE(OP_PUSHUNDEF) {
      PUSH(pic_undef_value());
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
      PUSH(pic_int_value(pic->ci->irep->u.s.ints[c.a]));
      NEXT;
    }
    CASE(OP_PUSHFLOAT) {
      PUSH(pic_float_value(pic->ci->irep->u.s.nums[c.a]));
      NEXT;
    }
    CASE(OP_PUSHCHAR) {
      PUSH(pic_char_value(pic->ci->irep->u.s.ints[c.a]));
      NEXT;
    }
    CASE(OP_PUSHEOF) {
      PUSH(pic_eof_object());
      NEXT;
    }
    CASE(OP_PUSHCONST) {
      PUSH(pic_obj_value(pic->ci->irep->pool[c.a]));
      NEXT;
    }
    CASE(OP_GREF) {
      PUSH(vm_gref(pic, (pic_sym *)pic->ci->irep->pool[c.a]));
      NEXT;
    }
    CASE(OP_GSET) {
      vm_gset(pic, (pic_sym *)pic->ci->irep->pool[c.a], POP());
      PUSH(pic_undef_value());
      NEXT;
    }
    CASE(OP_LREF) {
      pic_callinfo *ci = pic->ci;
      struct pic_irep *irep = ci->irep;

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
      pic_callinfo *ci = pic->ci;
      struct pic_irep *irep = ci->irep;

      if (ci->cxt != NULL && ci->cxt->regs == ci->cxt->storage) {
        if (c.a >= irep->argc + irep->localc) {
          ci->cxt->regs[c.a - (ci->regs - ci->fp)] = POP();
          PUSH(pic_undef_value());
          NEXT;
        }
      }
      pic->ci->fp[c.a] = POP();
      PUSH(pic_undef_value());
      NEXT;
    }
    CASE(OP_CREF) {
      int depth = c.a;
      struct pic_context *cxt;

      cxt = pic->ci->up;
      while (--depth) {
	cxt = cxt->up;
      }
      PUSH(cxt->regs[c.b]);
      NEXT;
    }
    CASE(OP_CSET) {
      int depth = c.a;
      struct pic_context *cxt;

      cxt = pic->ci->up;
      while (--depth) {
	cxt = cxt->up;
      }
      cxt->regs[c.b] = POP();
      PUSH(pic_undef_value());
      NEXT;
    }
    CASE(OP_JMP) {
      pic->ip += c.a;
      JUMP;
    }
    CASE(OP_JMPIF) {
      pic_value v;

      v = POP();
      if (! pic_false_p(v)) {
	pic->ip += c.a;
	JUMP;
      }
      NEXT;
    }
    CASE(OP_CALL) {
      pic_value x, v;
      pic_callinfo *ci;

      if (c.a == -1) {
        pic->sp += pic->ci[1].retc - 1;
        c.a = pic->ci[1].retc + 1;
      }

    L_CALL:
      x = pic->sp[-c.a];
      if (! pic_proc_p(x)) {
	pic_errorf(pic, "invalid application: ~s", x);
      }
      proc = pic_proc_ptr(x);

      VM_CALL_PRINT;

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
      if (pic_proc_func_p(proc)) {

        /* invoke! */
        v = proc->u.f.func(pic);
        pic->sp[0] = v;
        pic->sp += pic->ci->retc;

        pic_gc_arena_restore(pic, ai);
        goto L_RET;
      }
      else {
        struct pic_irep *irep = proc->u.i.irep;
	int i;
	pic_value rest;

        ci->irep = irep;
	if (ci->argc != irep->argc) {
	  if (! (irep->varg && ci->argc >= irep->argc)) {
            pic_errorf(pic, "wrong number of arguments (%d for %s%d)", ci->argc - 1, (irep->varg ? "at least " : ""), irep->argc - 1);
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

	/* prepare cxt */
        ci->up = proc->u.i.cxt;
        ci->regc = irep->capturec;
        ci->regs = ci->fp + irep->argc + irep->localc;

	pic->ip = irep->u.s.code;
	pic_gc_arena_restore(pic, ai);
	JUMP;
      }
    }
    CASE(OP_TAILCALL) {
      int i, argc;
      pic_value *argv;
      pic_callinfo *ci;

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
      pic_callinfo *ci;

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

      proc = pic_make_proc_irep(pic, pic->ci->irep->u.s.irep[c.a].i, pic->ci->cxt);
      PUSH(pic_obj_value(proc));
      pic_gc_arena_restore(pic, ai);
      NEXT;
    }

#define check_condition(name, n) do {                                   \
      if (c.a != n + 1)                                                 \
        goto L_CALL;                                                    \
    } while (0)

    CASE(OP_CONS) {
      pic_value a, b;
      check_condition(CONS, 2);
      pic_gc_protect(pic, b = POP());
      pic_gc_protect(pic, a = POP());
      (void)POP();
      PUSH(pic_cons(pic, a, b));
      pic_gc_arena_restore(pic, ai);
      NEXT;
    }
    CASE(OP_CAR) {
      pic_value p;
      check_condition(CAR, 1);
      p = POP();
      (void)POP();
      PUSH(pic_car(pic, p));
      NEXT;
    }
    CASE(OP_CDR) {
      pic_value p;
      check_condition(CDR, 1);
      p = POP();
      (void)POP();
      PUSH(pic_cdr(pic, p));
      NEXT;
    }
    CASE(OP_NILP) {
      pic_value p;
      check_condition(NILP, 1);
      p = POP();
      (void)POP();
      PUSH(pic_bool_value(pic_nil_p(p)));
      NEXT;
    }
    CASE(OP_SYMBOLP) {
      pic_value p;
      check_condition(SYMBOLP, 1);
      p = POP();
      (void)POP();
      PUSH(pic_bool_value(pic_sym_p(p)));
      NEXT;
    }
    CASE(OP_PAIRP) {
      pic_value p;
      check_condition(PAIRP, 1);
      p = POP();
      (void)POP();
      PUSH(pic_bool_value(pic_pair_p(p)));
      NEXT;
    }
    CASE(OP_NOT) {
      pic_value v;
      check_condition(NOT, 1);
      v = pic_false_p(POP()) ? pic_true_value() : pic_false_value();
      (void)POP();
      PUSH(v);
      NEXT;
    }

    CASE(OP_ADD) {
      pic_value a, b;
      check_condition(ADD, 2);
      b = POP();
      a = POP();
      (void)POP();
      PUSH(pic_add(pic, a, b));
      NEXT;
    }
    CASE(OP_SUB) {
      pic_value a, b;
      check_condition(SUB, 2);
      b = POP();
      a = POP();
      (void)POP();
      PUSH(pic_sub(pic, a, b));
      NEXT;
    }
    CASE(OP_MUL) {
      pic_value a, b;
      check_condition(MUL, 2);
      b = POP();
      a = POP();
      (void)POP();
      PUSH(pic_mul(pic, a, b));
      NEXT;
    }
    CASE(OP_DIV) {
      pic_value a, b;
      check_condition(DIV, 2);
      b = POP();
      a = POP();
      (void)POP();
      PUSH(pic_div(pic, a, b));
      NEXT;
    }
    CASE(OP_EQ) {
      pic_value a, b;
      check_condition(EQ, 2);
      b = POP();
      a = POP();
      (void)POP();
      PUSH(pic_bool_value(pic_eq(pic, a, b)));
      NEXT;
    }
    CASE(OP_LE) {
      pic_value a, b;
      check_condition(LT, 2);
      b = POP();
      a = POP();
      (void)POP();
      PUSH(pic_bool_value(pic_le(pic, a, b)));
      NEXT;
    }
    CASE(OP_LT) {
      pic_value a, b;
      check_condition(LE, 2);
      b = POP();
      a = POP();
      (void)POP();
      PUSH(pic_bool_value(pic_lt(pic, a, b)));
      NEXT;
    }
    CASE(OP_GE) {
      pic_value a, b;
      check_condition(LT, 2);
      b = POP();
      a = POP();
      (void)POP();
      PUSH(pic_bool_value(pic_ge(pic, a, b)));
      NEXT;
    }
    CASE(OP_GT) {
      pic_value a, b;
      check_condition(LE, 2);
      b = POP();
      a = POP();
      (void)POP();
      PUSH(pic_bool_value(pic_gt(pic, a, b)));
      NEXT;
    }

    CASE(OP_STOP) {

      VM_END_PRINT;

      return pic_gc_protect(pic, POP());
    }
  } VM_LOOP_END;
}

pic_value
pic_apply_list(pic_state *pic, struct pic_proc *proc, pic_value list)
{
  int n, i = 0;
  pic_vec *args;
  pic_value x, it;

  n = pic_length(pic, list);

  args = pic_make_vec(pic, n);

  pic_for_each (x, list, it) {
    args->data[i++] = x;
  }

  return pic_apply(pic, proc, n, args->data);
}

pic_value
pic_apply_trampoline(pic_state *pic, struct pic_proc *proc, int argc, pic_value *args)
{
  pic_value *sp;
  pic_callinfo *ci;
  int i;

  PIC_INIT_CODE_I(pic->iseq[0], OP_NOP, 0);
  PIC_INIT_CODE_I(pic->iseq[1], OP_TAILCALL, -1);

  *pic->sp++ = pic_obj_value(proc);

  sp = pic->sp;
  for (i = 0; i < argc; ++i) {
    *sp++ = args[i];
  }

  ci = PUSHCI();
  ci->ip = pic->iseq;
  ci->fp = pic->sp;
  ci->retc = (int)argc;

  if (ci->retc == 0) {
    return pic_undef_value();
  } else {
    return args[0];
  }
}

pic_value
pic_apply_trampoline_list(pic_state *pic, struct pic_proc *proc, pic_value args)
{
  int i, argc = pic_length(pic, args);
  pic_value val, it;
  pic_vec *argv = pic_make_vec(pic, argc);

  i = 0;
  pic_for_each (val, args, it) {
    argv->data[i++] = val;
  }

  return pic_apply_trampoline(pic, proc, argc, argv->data);
}

static pic_value
pic_va_apply(pic_state *pic, struct pic_proc *proc, int n, ...)
{
  pic_vec *args = pic_make_vec(pic, n);
  va_list ap;
  int i = 0;

  va_start(ap, n);

  while (i < n) {
    args->data[i++] = va_arg(ap, pic_value);
  }

  va_end(ap);

  return pic_apply(pic, proc, n, args->data);
}

pic_value
pic_apply0(pic_state *pic, struct pic_proc *proc)
{
  return pic_va_apply(pic, proc, 0);
}

pic_value
pic_apply1(pic_state *pic, struct pic_proc *proc, pic_value arg1)
{
  return pic_va_apply(pic, proc, 1, arg1);
}

pic_value
pic_apply2(pic_state *pic, struct pic_proc *proc, pic_value arg1, pic_value arg2)
{
  return pic_va_apply(pic, proc, 2, arg1, arg2);
}

pic_value
pic_apply3(pic_state *pic, struct pic_proc *proc, pic_value arg1, pic_value arg2, pic_value arg3)
{
  return pic_va_apply(pic, proc, 3, arg1, arg2, arg3);
}

pic_value
pic_apply4(pic_state *pic, struct pic_proc *proc, pic_value arg1, pic_value arg2, pic_value arg3, pic_value arg4)
{
  return pic_va_apply(pic, proc, 4, arg1, arg2, arg3, arg4);
}

pic_value
pic_apply5(pic_state *pic, struct pic_proc *proc, pic_value arg1, pic_value arg2, pic_value arg3, pic_value arg4, pic_value arg5)
{
  return pic_va_apply(pic, proc, 5, arg1, arg2, arg3, arg4, arg5);
}

void
pic_define_(pic_state *pic, const char *name, pic_value val)
{
  pic_sym *sym, *uid;

  sym = pic_intern(pic, name);

  if ((uid = pic_find_identifier(pic, (pic_id *)sym, pic->lib->env)) == NULL) {
    uid = pic_add_identifier(pic, (pic_id *)sym, pic->lib->env);
  } else {
    if (pic_reg_has(pic, pic->globals, uid)) {
      pic_warnf(pic, "redefining variable: ~s", pic_obj_value(uid));
    }
  }

  pic_set(pic, pic->lib, name, val);
}

void
pic_define(pic_state *pic, const char *name, pic_value val)
{
  pic_define_(pic, name, val);
  pic_export(pic, pic_intern(pic, name));
}

void
pic_defun_(pic_state *pic, const char *name, pic_func_t cfunc)
{
  pic_define_(pic, name, pic_obj_value(pic_make_proc(pic, cfunc)));
}

void
pic_defun(pic_state *pic, const char *name, pic_func_t cfunc)
{
  pic_defun_(pic, name, cfunc);
  pic_export(pic, pic_intern(pic, name));
}

void
pic_defvar_(pic_state *pic, const char *name, pic_value init, struct pic_proc *conv)
{
  pic_define_(pic, name, pic_obj_value(pic_make_var(pic, init, conv)));
}

void
pic_defvar(pic_state *pic, const char *name, pic_value init, struct pic_proc *conv)
{
  pic_defvar_(pic, name, init, conv);
  pic_export(pic, pic_intern(pic, name));
}

pic_value
pic_ref(pic_state *pic, struct pic_lib *lib, const char *name)
{
  pic_sym *sym, *uid;

  sym = pic_intern(pic, name);

  if ((uid = pic_find_identifier(pic, (pic_id *)sym, lib->env)) == NULL) {
    pic_errorf(pic, "symbol \"%s\" not defined in library ~s", name, lib->name);
  }

  return vm_gref(pic, uid);
}

void
pic_set(pic_state *pic, struct pic_lib *lib, const char *name, pic_value val)
{
  pic_sym *sym, *uid;

  sym = pic_intern(pic, name);

  if ((uid = pic_find_identifier(pic, (pic_id *)sym, lib->env)) == NULL) {
    pic_errorf(pic, "symbol \"%s\" not defined in library ~s", name, lib->name);
  }

  vm_gset(pic, uid, val);
}

static struct pic_proc *
pic_ref_proc(pic_state *pic, struct pic_lib *lib, const char *name)
{
  pic_value proc;

  proc = pic_ref(pic, lib, name);

  pic_assert_type(pic, proc, proc);

  return pic_proc_ptr(proc);
}

pic_value
pic_funcall(pic_state *pic, struct pic_lib *lib, const char *name, pic_value args)
{
  return pic_apply_list(pic, pic_ref_proc(pic, lib, name), args);
}

pic_value
pic_funcall0(pic_state *pic, struct pic_lib *lib, const char *name)
{
  return pic_apply0(pic, pic_ref_proc(pic, lib, name));
}

pic_value
pic_funcall1(pic_state *pic, struct pic_lib *lib, const char *name, pic_value arg0)
{
  return pic_apply1(pic, pic_ref_proc(pic, lib, name), arg0);
}

pic_value
pic_funcall2(pic_state *pic, struct pic_lib *lib, const char *name, pic_value arg0, pic_value arg1)
{
  return pic_apply2(pic, pic_ref_proc(pic, lib, name), arg0, arg1);
}

pic_value
pic_funcall3(pic_state *pic, struct pic_lib *lib, const char *name, pic_value arg0, pic_value arg1, pic_value arg2)
{
  return pic_apply3(pic, pic_ref_proc(pic, lib, name), arg0, arg1, arg2);
}

void
pic_irep_incref(pic_state PIC_UNUSED(*pic), struct pic_irep *irep)
{
  irep->refc++;
}

void
pic_irep_decref(pic_state *pic, struct pic_irep *irep)
{
  size_t i;

  if (--irep->refc == 0) {
    pic_free(pic, irep->u.s.code);
    pic_free(pic, irep->u.s.ints);
    pic_free(pic, irep->u.s.nums);
    pic_free(pic, irep->pool);

    /* unchain before decref children ireps */
    irep->list.prev->next = irep->list.next;
    irep->list.next->prev = irep->list.prev;

    for (i = 0; i < irep->nirep; ++i) {
      pic_irep_decref(pic, irep->u.s.irep[i].i);
    }
    pic_free(pic, irep->u.s.irep);
    pic_free(pic, irep);
  }
}

struct pic_proc *
pic_make_proc(pic_state *pic, pic_func_t func)
{
  struct pic_proc *proc;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->tag = PIC_PROC_TAG_FUNC;
  proc->u.f.func = func;
  proc->u.f.env = NULL;
  return proc;
}

struct pic_proc *
pic_make_proc_irep(pic_state *pic, struct pic_irep *irep, struct pic_context *cxt)
{
  struct pic_proc *proc;

  proc = (struct pic_proc *)pic_obj_alloc(pic, sizeof(struct pic_proc), PIC_TT_PROC);
  proc->tag = PIC_PROC_TAG_IREP;
  proc->u.i.irep = irep;
  proc->u.i.cxt = cxt;
  pic_irep_incref(pic, irep);
  return proc;
}

struct pic_dict *
pic_proc_env(pic_state *pic, struct pic_proc *proc)
{
  assert(pic_proc_func_p(proc));

  if (! proc->u.f.env) {
    proc->u.f.env = pic_make_dict(pic);
  }
  return proc->u.f.env;
}

bool
pic_proc_env_has(pic_state *pic, struct pic_proc *proc, const char *key)
{
  return pic_dict_has(pic, pic_proc_env(pic, proc), pic_intern(pic, key));
}

pic_value
pic_proc_env_ref(pic_state *pic, struct pic_proc *proc, const char *key)
{
  return pic_dict_ref(pic, pic_proc_env(pic, proc), pic_intern(pic, key));
}

void
pic_proc_env_set(pic_state *pic, struct pic_proc *proc, const char *key, pic_value val)
{
  pic_dict_set(pic, pic_proc_env(pic, proc), pic_intern(pic, key), val);
}

static pic_value
pic_proc_proc_p(pic_state *pic)
{
  pic_value v;

  pic_get_args(pic, "o", &v);

  return pic_bool_value(pic_proc_p(v));
}

static pic_value
pic_proc_apply(pic_state *pic)
{
  struct pic_proc *proc;
  pic_value *args;
  int argc;
  pic_value arg_list;

  pic_get_args(pic, "l*", &proc, &argc, &args);

  if (argc == 0) {
    pic_errorf(pic, "apply: wrong number of arguments");
  }

  arg_list = args[--argc];
  while (argc--) {
    arg_list = pic_cons(pic, args[argc], arg_list);
  }

  return pic_apply_trampoline_list(pic, proc, arg_list);
}

void
pic_init_proc(pic_state *pic)
{
  pic_defun(pic, "procedure?", pic_proc_proc_p);
  pic_defun(pic, "apply", pic_proc_apply);
}
