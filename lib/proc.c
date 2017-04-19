/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "object.h"
#include "state.h"

struct frame *
pic_make_frame_unsafe(pic_state *pic, int n)
{
  struct frame *fp;
  int i;

  fp = (struct frame *)pic_obj_alloc_unsafe(pic, PIC_TYPE_FRAME);
  fp->regs = n ? pic_malloc(pic, sizeof(pic_value) * n) : NULL;
  fp->regc = n;
  fp->up = NULL;
  for (i = 0; i < n; ++i) {
    fp->regs[i] = pic_invalid_value(pic);
  }
  return fp;
}

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
  struct proc *proc;
  int i;

  assert(n >= 0);

  proc = (struct proc *)pic_obj_alloc(pic, PIC_TYPE_PROC_FUNC);
  proc->u.func = f;
  proc->env = NULL;
  if (n != 0) {
    proc->env = pic_make_frame_unsafe(pic, n);
  }
  for (i = 0; i < n; ++i) {
    proc->env->regs[i] = va_arg(ap, pic_value);
  }
  return obj_value(pic, proc);
}

pic_value
pic_make_proc_func(pic_state *pic, pic_func_t func)
{
  struct proc *proc;

  proc = (struct proc *)pic_obj_alloc(pic, PIC_TYPE_PROC_FUNC);
  proc->u.func = func;
  proc->env = NULL;
  return obj_value(pic, proc);
}

pic_value
pic_make_proc_irep_unsafe(pic_state *pic, struct irep *irep, struct frame *fp)
{
  struct proc *proc;

  proc = (struct proc *)pic_obj_alloc_unsafe(pic, PIC_TYPE_PROC_IREP);
  proc->u.irep = irep;
  proc->env = fp;
  return obj_value(pic, proc);
}

PIC_NORETURN static void
arg_error(pic_state *pic, int actual, bool varg, int expected)
{
  const char *msg;

  msg = pic_str(pic, pic_strf_value(pic, "wrong number of arguments (%d for %s%d)", actual, (varg ? "at least " : ""), expected), NULL);

  pic_error(pic, msg, 0);
}

#define GET_ARGC(pic) (pic->cxt->pc[1])
#define GET_PROC(pic) (pic->cxt->fp->regs[0])
#define GET_CONT(pic) (pic->cxt->fp->regs[1])
#define GET_ARG(pic,n) (pic->cxt->fp->regs[(n)+2])

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
  int i, argc = GET_ARGC(pic) - 1; /* one for continuation */
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
  struct frame *fp = pic->cxt->fp->up;
  assert(n >= 0);
  if (fp == NULL || fp->regc <= n) {
    pic_error(pic, "pic_closure_ref: index out of range", 1, pic_int_value(pic, n));
  }
  return fp->regs[n];
}

void
pic_closure_set(pic_state *pic, int n, pic_value v)
{
  struct frame *fp = pic->cxt->fp->up;
  assert(n >= 0);
  if (fp == NULL || fp->regc <= n) {
    pic_error(pic, "pic_closure_ref: index out of range", 1, pic_int_value(pic, n));
  }
  fp->regs[n] = v;
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
  size_t ai = pic_enter(pic);
  pic_value *args = pic_alloca(pic, sizeof(pic_value) * n);
  pic_value r;
  int i;

  for (i = 0; i < n; ++i) {
    args[i] = va_arg(ap, pic_value);
  }
  r = pic_apply(pic, proc, n, args);
  pic_leave(pic, ai);
  return pic_protect(pic, r);
}

pic_value
pic_apply(pic_state *pic, pic_value proc, int argc, pic_value *argv)
{
  struct context cxt;
  size_t arena_base = pic->ai;

#define MKCALL(argc) (cxt.tmpcode[0] = OP_CALL, cxt.tmpcode[1] = (argc), cxt.tmpcode)

  cxt.pc = MKCALL(argc + 1);
  cxt.sp = pic_make_frame_unsafe(pic, argc + 3);
  cxt.sp->regs[0] = proc;
  cxt.sp->regs[1] = pic->halt;
  if (argc != 0) {
    int i;
    for (i = 0; i < argc; ++i) {
      cxt.sp->regs[i + 2] = argv[i];
    }
  }
  cxt.fp = NULL;
  cxt.irep = NULL;
  cxt.prev = pic->cxt;
  pic->cxt = &cxt;

#define SAVE (pic->ai = arena_base)

  if (PIC_SETJMP(cxt.jmp) != 0) {
    SAVE;
  }

#define A (cxt.pc[1])
#define B (cxt.pc[2])
#define C (cxt.pc[3])
#define Bx ((C << 8) + B)
#define REG(i) (cxt.sp->regs[i])

#if PIC_DIRECT_THREADED_VM
# define VM_LOOP JUMP;
# define CASE(x) L_##x:
# define NEXT(n) (cxt.pc += n); JUMP;
# define JUMP goto *oplabels[*cxt.pc];
# define VM_LOOP_END
#else
# define VM_LOOP for (;;) { switch (*cxt.pc) {
# define CASE(x) case x:
# define NEXT(n) (cxt.pc += n); break
# define JUMP break
# define VM_LOOP_END } }
#endif

#if PIC_DIRECT_THREADED_VM
  static const void *oplabels[] = {
    [OP_HALT] = &&L_OP_HALT, [OP_CALL] = &&L_OP_CALL, [OP_PROC] = &&L_OP_PROC,
    [OP_LOAD] = &&L_OP_LOAD, [OP_LREF] = &&L_OP_LREF, [OP_LSET] = &&L_OP_LSET,
    [OP_GREF] = &&L_OP_GREF, [OP_GSET] = &&L_OP_GSET, [OP_COND] = &&L_OP_COND,
    [OP_LOADT] = &&L_OP_LOADT, [OP_LOADF] = &&L_OP_LOADF, [OP_LOADN] = &&L_OP_LOADN,
    [OP_LOADU] = &&L_OP_LOADU, [OP_LOADI] = &&L_OP_LOADI
  };
#endif

  VM_LOOP {
    CASE(OP_HALT) {
      pic_value ret = cxt.fp->regs[1];
      pic->cxt = pic->cxt->prev;
      pic_protect(pic, ret);
      return ret;
    }
    CASE(OP_CALL) {
      struct proc *proc;
      if (! pic_proc_p(pic, REG(0))) {
        pic_error(pic, "invalid application", 1, REG(0));
      }
      proc = proc_ptr(pic, REG(0));
      if (proc->tt == PIC_TYPE_PROC_FUNC) {
        pic_value v;
        cxt.sp->up = proc->env; /* push static link */
        cxt.fp = cxt.sp;
        cxt.sp = NULL;
        cxt.irep = NULL;
        v = proc->u.func(pic);
        if (cxt.sp != NULL) {   /* tail call */
          SAVE;
          JUMP;
        } else {
          cxt.sp = pic_make_frame_unsafe(pic, 3);
          cxt.sp->regs[0] = cxt.fp->regs[1]; /* cont. */
          cxt.sp->regs[1] = v;
          cxt.pc = MKCALL(1);
          SAVE;
          JUMP;
        }
      } else {
        struct irep *irep = proc->u.irep;

        if (A != irep->argc) {
          if (! ((irep->flags & IREP_VARG) != 0 && A >= irep->argc)) {
            arg_error(pic, A, (irep->flags & IREP_VARG), irep->argc);
          }
        }
        if (irep->flags & IREP_VARG) {
          REG(irep->argc + 1) = pic_make_list(pic, A - irep->argc, &REG(irep->argc + 1));
          SAVE;                 /* TODO: get rid of this */
        }

        cxt.sp->up = proc->env; /* push static link */
        cxt.fp = cxt.sp;
        cxt.sp = pic_make_frame_unsafe(pic, irep->frame_size);
        cxt.pc = irep->code;
        cxt.irep = irep;
        JUMP;
      }
    }
    CASE(OP_LREF) {
      struct frame *f;
      int depth = B;
      for (f = cxt.fp; depth--; f = f->up);
      REG(A) = f->regs[C];
      NEXT(4);
    }
    CASE(OP_LSET) {
      struct frame *f;
      int depth = B;
      for (f = cxt.fp; depth--; f = f->up);
      f->regs[C] = REG(A);
      NEXT(4);
    }
    CASE(OP_GREF) {
      REG(A) = pic_global_ref(pic, cxt.irep->obj[B]);
      NEXT(3);
    }
    CASE(OP_GSET) {
      pic_global_set(pic, cxt.irep->obj[B], REG(A));
      NEXT(3);
    }
    CASE(OP_COND) {
      if (pic_false_p(pic, REG(A))) {
        NEXT(Bx);
      } else {
        NEXT(4);
      }
    }
    CASE(OP_PROC) {
      REG(A) = pic_make_proc_irep_unsafe(pic, cxt.irep->irep[B], cxt.fp);
      NEXT(3);
    }
    CASE(OP_LOAD) {
      REG(A) = cxt.irep->obj[B];
      NEXT(3);
    }
    CASE(OP_LOADU) {
      REG(A) = pic_undef_value(pic);
      NEXT(2);
    }
    CASE(OP_LOADT) {
      REG(A) = pic_true_value(pic);
      NEXT(2);
    }
    CASE(OP_LOADF) {
      REG(A) = pic_false_value(pic);
      NEXT(2);
    }
    CASE(OP_LOADN) {
      REG(A) = pic_nil_value(pic);
      NEXT(2);
    }
    CASE(OP_LOADI) {
      REG(A) = pic_int_value(pic, (signed char) B);
      NEXT(3);
    }
  } VM_LOOP_END
}

pic_value
pic_applyk(pic_state *pic, pic_value proc, int argc, pic_value *args)
{
  const code_t *pc;
  struct frame *sp;

#define MKCALLK(argc)                                                   \
  (pic->cxt->tmpcode[0] = OP_CALL, pic->cxt->tmpcode[1] = (argc), pic->cxt->tmpcode)

  pc = MKCALLK(argc + 1);
  sp = pic_make_frame_unsafe(pic, argc + 3);
  sp->regs[0] = proc;
  sp->regs[1] = GET_CONT(pic);
  if (argc != 0) {
    int i;
    for (i = 0; i < argc; ++i) {
      sp->regs[i + 2] = args[i];
    }
  }
  pic->cxt->pc = pc;
  pic->cxt->sp = sp;
  return pic_invalid_value(pic);
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
