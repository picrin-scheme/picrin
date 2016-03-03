/* -*- flycheck-clang-include-path: ("../extlib/benz/include") -*- */

#include "picrin.h"
#include "picrin/extra.h"
#include "picrin/private/state.h"
#include "picrin/private/object.h"

#define NUM_IREP 5000

struct irep ireps[NUM_IREP];
size_t nireps, ncode, nirepp, nints, nnums, npool;

void
sort_ireps(struct irep *root)
{
  struct irep *scan, *free;
  size_t i;

  scan = free = ireps;

  *free++ = *root;

  while (scan < free) {
    for (i = 0; i < scan->nirep; ++i) {
      *free++ = *scan->irep[i];
    }
    scan++;
  }

  nireps = scan - ireps;
}

static void
emit_irep()
{
  size_t j;

  printf("extern struct irep *pic_boot_irepp[];\n");
  printf("extern struct code pic_boot_code[];\n");
  printf("extern int pic_boot_ints[];\n");
  printf("extern double pic_boot_nums[];\n");
  printf("extern struct object *pic_boot_pool[];\n");
  printf("\n");

  printf("struct irep pic_boot_irep[] = {\n");

  ncode = nirepp = nints = nnums = npool = 0;

  for (j = 0; j < nireps; ++j) {
    printf("  {");
    printf(" { 0, 0 }, 1,");
    printf(" %d, %d, %d, %d,", ireps[j].argc, ireps[j].localc, ireps[j].capturec, ireps[j].varg);
    printf(" &pic_boot_code[%zu],", ncode);
    printf(" &pic_boot_irepp[%zu],", nirepp);
    printf(" &pic_boot_ints[%zu],", nints);
    printf(" &pic_boot_nums[%zu],", nnums);
    printf(" &pic_boot_pool[%zu],", npool);
    printf(" %zu, %zu, %zu, %zu, %zu,", ireps[j].ncode, ireps[j].nirep, ireps[j].nints, ireps[j].nnums, ireps[j].npool);
    printf(" },\n");

    ncode += ireps[j].ncode;
    nirepp += ireps[j].nirep;
    nints += ireps[j].nints;
    nnums += ireps[j].nnums;
    npool += ireps[j].npool;
  }

  printf("{ { 0, 0 }, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } };\n\n");
}

static void
emit_code()
{
  size_t i, j, k = 0;
  struct code c;

  printf("struct code pic_boot_code[] = {");

  for (j = 0; j < nireps; ++j) {
    for (i = 0; i < ireps[j].ncode; ++i) {
      c = ireps[j].code[i];
      if (k++ % 8 == 0) {
        printf("\n ");
      }
      printf(" { %d, %d, %d },", c.insn, c.a, c.b);
    }
  }

  printf("\n{ 0, 0, 0 } };\n\n");
}

static void
emit_irepp()
{
  size_t i;

  printf("struct irep *pic_boot_irepp[] = {\n");

  for (i = 1; i < nireps; ++i) {
    printf("  &pic_boot_irep[%zu],\n", i);
  }

  printf("0 };\n\n");
}

static void
emit_ints()
{
  size_t i, j;

  printf("int pic_boot_ints[] = {\n");

  printf("  ");
  for (j = 0; j < nireps; ++j) {
    for (i = 0; i < ireps[j].nints; ++i) {
      printf("%d,", ireps[j].ints[i]);
    }
  }
  printf("\n");

  printf("0 };\n\n");
}

static void
emit_nums()
{
  size_t i, j;

  printf("double pic_boot_nums[] = {\n");

  for (j = 0; j < nireps; ++j) {
    for (i = 0; i < ireps[j].nnums; ++i) {
      printf("  %f,\n", ireps[j].nums[i]);
    }
  }

  printf("0.0 };\n\n");
}

static void
emit_pool(pic_state *pic)
{
  size_t i, j, k = 0;

  printf("struct object *pic_boot_pool[] = {");

  for (j = 0; j < nireps; ++j) {
    for (i = 0; i < ireps[j].npool; ++i) {
      pic_value obj = pic_obj_value(ireps[j].pool[i]);
      if (k++ % 5 == 0) {
        printf("\n ");
      }
      if (pic_sym_p(pic, obj)) {
        printf(" (struct object *)\"M%s\",", pic_sym(pic, obj));
      } else if (pic_str_p(pic, obj)) {
        printf(" (struct object *)\"S%s\",", pic_str(pic, obj));
      } else {
        pic_error(pic, "symbol or string required", 0);
      }
    }
  }

  printf("\n0 };\n\n");
}

static void
pic_dump(pic_state *pic, pic_value proc)
{
  if (! pic_proc_p(pic, proc)) {
    pic_error(pic, "procedure required", 0);
  }

  if (! pic_irep_p(pic, proc)) {
    pic_error(pic, "c function given", 0);
  }

  sort_ireps(pic_proc_ptr(pic, proc)->u.i.irep);

  emit_irep();
  emit_code();
  emit_irepp();
  emit_ints();
  emit_nums();
  emit_pool(pic);
}

static void
print_header()
{
  puts("/* DO NOT EDIT, this file is automatically generated by tools/mkboot */");
  puts("");
  puts("#include \"picrin.h\"");
  puts("#include \"picrin/extra.h\"");
  puts("#include \"picrin/private/state.h\"");
  puts("#include \"picrin/private/object.h\"");
  puts("#include \"picrin/private/vm.h\"");
  puts("");
}

static void
print_footer()
{
  puts("static pic_value");
  puts("pic_builtins(pic_state *pic) {");
  puts("  size_t i;");
  puts("");
  puts("  for (i = 0; i < sizeof(pic_boot_pool) / sizeof(pic_boot_pool[0]) - 1; ++i) {");
  puts("    const char *str = (const char *)pic_boot_pool[i];");
  puts("    if (*str == 'M') {");
  puts("      pic_boot_pool[i] = (struct object *)pic_sym_ptr(pic, pic_intern_cstr(pic, str + 1));");
  puts("    } else {");
  puts("      pic_boot_pool[i] = (struct object *)pic_str_ptr(pic, pic_cstr_value(pic, str + 1));");
  puts("    }");
  puts("  }");
  puts("");
  puts("  for (i = 0; i < sizeof(pic_boot_irep) / sizeof(pic_boot_irep[0]) - 1; ++i) {");
  puts("    struct irep *irep = pic_boot_irep + i;");
  puts("    irep->list.next = pic->ireps.next;");
  puts("    irep->list.prev = &pic->ireps;");
  puts("    irep->list.next->prev = &irep->list;");
  puts("    irep->list.prev->next = &irep->list;");
  puts("  }");
  puts("");
  puts("  return pic_call(pic, pic_make_proc_irep(pic, pic_boot_irep, 0), 0);");
  puts("}");
  puts("");
  puts("void");
  puts("pic_boot(pic_state *pic)");
  puts("{");
  puts("  pic_value e, it, uid, env = pic_library_environment(pic, \"picrin.base\");");
  puts("");
  puts("  pic_for_each (e, pic_builtins(pic), it) {");
  puts("    uid = pic_add_identifier(pic, pic_car(pic, e), env);");
  puts("    pic_weak_set(pic, pic->macros, uid, pic_cdr(pic, e));");
  puts("    pic_export(pic, pic_car(pic, e));");
  puts("  }");
  puts("}");
}

int main()
{
  pic_state *pic = pic_open(pic_default_allocf, 0);

  pic_import(pic, "picrin.base");

  print_header();

  pic_dump(pic, pic_eval(pic, pic_read(pic, pic_stdin(pic)), "picrin.user"));

  print_footer();

  pic_close(pic);
}