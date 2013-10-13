#ifndef PICRIN_H__
#define PICRIN_H__

#include <stddef.h>
#include <stdbool.h>

#include "picrin/value.h"

struct pic_env {
  pic_value assoc;
  struct pic_env *parent;
};

#define PIC_ARENA_SIZE 1024
#define PIC_HEAP_SIZE 1024

typedef struct {
  pic_value *sp;
  pic_value *stbase, *stend;

  struct pic_env *global_env;

  struct heap_page *heap;
  struct pic_object *arena[PIC_ARENA_SIZE];
  int arena_idx;
} pic_state;

void *pic_alloc(pic_state *, size_t);
struct pic_object *pic_obj_alloc(pic_state *, size_t, enum pic_tt);
void pic_free(pic_state *, void *);

void pic_gc_protect(pic_state *, pic_value);
int pic_gc_arena_preserve(pic_state *);
void pic_gc_arena_restore(pic_state *, int);

pic_state *pic_open();
void pic_close(pic_state *);

pic_value pic_cons(pic_state *, pic_value, pic_value);
pic_value pic_car(pic_state *, pic_value);
pic_value pic_cdr(pic_state *, pic_value);

bool pic_eq_p(pic_state *, pic_value, pic_value);

pic_value pic_intern_cstr(pic_state *, const char *);

pic_value pic_parse(pic_state *, const char *);

pic_value pic_eval(pic_state *, pic_value, struct pic_env *);
pic_value pic_run(pic_state *, struct pic_proc *, pic_value);
struct pic_proc *pic_codegen(pic_state *, pic_value, struct pic_env*);

void pic_raise(pic_state *, const char *);

void pic_debug(pic_state *, pic_value);

#endif
