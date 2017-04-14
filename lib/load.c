/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "../object.h"
#include "../state.h"

struct irep *
assemble(pic_state *pic, pic_value as)
{
  pic_value codes, reps, objs;
  int argc, varg, frame_size, repc, objc, i;
  struct irep **irep, *ir;
  pic_value *obj, r, it;
  code_t *code;
  size_t ai = pic_enter(pic);

  codes = pic_list_ref(pic, as, 0);
  reps = pic_list_ref(pic, as, 1);
  objs = pic_list_ref(pic, as, 2);
  argc = pic_int(pic, pic_car(pic, pic_list_ref(pic, as, 3)));
  varg = pic_bool(pic, pic_cdr(pic, pic_list_ref(pic, as, 3)));
  frame_size = pic_int(pic, pic_list_ref(pic, as, 4));

  repc = pic_length(pic, reps);
  objc = pic_length(pic, objs);

  assert(0 <= argc && argc < 256);
  assert(0 <= frame_size && frame_size < 256);
  assert(0 <= repc && repc < 256);
  assert(0 <= objc && objc < 256);

  irep = pic_malloc(pic, sizeof(*irep) * repc);
  i = 0;
  pic_for_each (r, reps, it) {
    irep[i++] = assemble(pic, r);
  }
  obj = pic_malloc(pic, sizeof(*obj) * objc);
  i = 0;
  pic_for_each (r, objs, it) {
    obj[i++] = r;
  }
  i = 0;
  pic_for_each (r, codes, it) {
    if (! pic_pair_p(pic, r))
      continue;
    if (pic_eq_p(pic, pic_car(pic, r), pic_intern_lit(pic, "COND"))) {
      i += 4;
      continue;
    }
    i += pic_length(pic, r);
  }
  code = pic_malloc(pic, i);
  i = 0;
  /* TODO: validate operands */
  pic_for_each (r, codes, it) {
    if (! pic_pair_p(pic, r))
      continue;
    pic_value op = pic_car(pic, r);
    if (pic_eq_p(pic, op, pic_intern_lit(pic, "HALT"))) {
      code[i++] = OP_HALT;
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "CALL"))) {
      code[i++] = OP_CALL;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "PROC"))) {
      code[i++] = OP_PROC;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 2));
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "LOAD"))) {
      code[i++] = OP_LOAD;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 2));
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "LREF"))) {
      code[i++] = OP_LREF;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 2));
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 3));
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "LSET"))) {
      code[i++] = OP_LSET;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 2));
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 3));
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "GREF"))) {
      code[i++] = OP_GREF;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 2));
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "GSET"))) {
      code[i++] = OP_GSET;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 2));
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "COND"))) {
      pic_value label = pic_list_ref(pic, r, 2);
      pic_value x, it2;
      int offset = 0;
      pic_for_each (x, it, it2) {
        if (pic_eq_p(pic, x, label))
          break;
        if (! pic_pair_p(pic, x))
          continue;
        if (pic_eq_p(pic, pic_car(pic, x), pic_intern_lit(pic, "COND"))) {
          offset += 4;
          continue;
        }
        offset += pic_length(pic, x);
      }
      code[i++] = OP_COND;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
      code[i++] = offset % 256;
      code[i++] = offset / 256;
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "LOADT"))) {
      code[i++] = OP_LOADT;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "LOADF"))) {
      code[i++] = OP_LOADF;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "LOADN"))) {
      code[i++] = OP_LOADN;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "LOADU"))) {
      code[i++] = OP_LOADU;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
    }
    else if (pic_eq_p(pic, op, pic_intern_lit(pic, "LOADI"))) {
      code[i++] = OP_LOADI;
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 1));
      code[i++] = pic_int(pic, pic_list_ref(pic, r, 2));
    }
  }

  ir = (struct irep *)pic_obj_alloc(pic, PIC_TYPE_IREP);
  ir->argc = argc;
  ir->flags = (varg ? IREP_VARG : 0);
  ir->frame_size = frame_size;
  ir->irepc = repc;
  ir->objc = objc;
  ir->irep = irep;
  ir->obj = obj;
  ir->code = code;

  pic_leave(pic, ai);
  pic_protect(pic, obj_value(pic, ir));

  return ir;
}

static pic_value
execute(pic_state *pic, struct irep *irep)
{
  struct proc *proc;

  proc = (struct proc *)pic_obj_alloc(pic, PIC_TYPE_PROC_IREP);
  proc->u.irep = irep;
  proc->env = NULL;
  return pic_apply(pic, obj_value(pic, proc), 0, NULL);
}

pic_value
pic_load(pic_state *pic, pic_value expr)
{
  return execute(pic, assemble(pic, expr));
}

void
pic_load_native(pic_state *pic, const char *str)
{
  pic_value e, port = pic_fmemopen(pic, str, strlen(str), "r");

  pic_try {
    size_t ai = pic_enter(pic);

    while (1) {
      pic_value form = pic_read(pic, port);
      if (pic_eof_p(pic, form)) {
        break;
      }
      pic_load(pic, form);
      pic_leave(pic, ai);
    }
  }
  pic_catch(e) {
    pic_fclose(pic, port);
    pic_raise(pic, e);
  }
  pic_fclose(pic, port);
}

static pic_value
pic_load_load(pic_state *pic)
{
  pic_value program;

  pic_get_args(pic, "o", &program);

  return pic_load(pic, program);
}

void
pic_init_load(pic_state *pic)
{
  pic_defun(pic, "load", pic_load_load);
}
