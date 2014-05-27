/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/pair.h"

#ifdef DLLLOAD_ENABLED
#include <dlfcn.h>
#include <string.h>
#include <libgen.h>
#endif

pic_value
pic_load_cstr(pic_state *pic, const char *src)
{
  int ai;
  pic_value v, exprs;
  struct pic_proc *proc;

  exprs = pic_parse_cstr(pic, src);
  if (pic_undef_p(exprs)) {
    pic_error(pic, "load: unexpected EOF");
  }

  pic_for_each (v, exprs) {
    ai = pic_gc_arena_preserve(pic);

    proc = pic_compile(pic, v);
    if (proc == NULL) {
      pic_error(pic, "load: compilation failure");
    }

    pic_apply(pic, proc, pic_nil_value());

    pic_gc_arena_restore(pic, ai);
  }

  return pic_none_value();
}

#ifdef DLLLOAD_ENABLED
bool
strends(const char *str, char *suffix){
  int len1 = strlen(str);
  int len2 = strlen(suffix);
  if( len1 < len2){
    return false;
  }
  else{
    if(strcmp(str + (len1-len2), suffix))
      return false;
    else
      return true;
  }  
}

pic_value
pic_load_dll(pic_state *pic, const char *fn )
{
  void *handle;
  char *error, *bname;
  char *prefix = "pic_init_";

  handle = dlopen(fn, RTLD_LAZY);
  if (!handle) {
    pic_errorf(pic, "load: could not load dll \"%s\"", fn);
  }

  bname = basename((char *)strdup(fn));
  *(strchr(bname, '.')) = '\0';

  char initfn_name[strlen(prefix) + strlen(bname) + 1];
  sprintf(initfn_name, "%s%s", prefix, bname);

  void (*init)(pic_state *) = dlsym(handle, initfn_name);
  if ((error = dlerror()) != NULL)  {
    pic_errorf(pic, "load: cannot find %s in %s: %s", initfn_name, fn, error);
  }
  (*init)(pic);
  
  return pic_none_value();
}
#endif

pic_value
pic_load(pic_state *pic, const char *fn)
{
  FILE *file;
  int ai;
  pic_value v, exprs;
  struct pic_proc *proc;
  
  file = fopen(fn, "r");
  if (file == NULL) {
    pic_errorf(pic, "load: could not read file \"%s\"", fn);
  }

  exprs = pic_parse_file(pic, file);
  if (pic_undef_p(exprs)) {
    pic_error(pic, "load: unexpected EOF");
  }

  pic_for_each (v, exprs) {
    ai = pic_gc_arena_preserve(pic);

    proc = pic_compile(pic, v);
    if (proc == NULL) {
      pic_error(pic, "load: compilation failure");
    }

    pic_apply(pic, proc, pic_nil_value());

    pic_gc_arena_restore(pic, ai);
  }

  return pic_none_value();
}

static pic_value
pic_load_load(pic_state *pic)
{
  pic_value envid;
  char *fn;

  pic_get_args(pic, "z|o", &fn, &envid);

#ifdef DLLLOAD_ENABLED
  return  strends(fn, ".so") ?
    pic_load_dll(pic, fn) :
    pic_load(pic, fn);
#else
  return pic_load(pic, fn);
#endif
}

void
pic_init_load(pic_state *pic)
{
  pic_deflibrary ("(scheme load)") {
    pic_defun(pic, "load", pic_load_load);
  }
}
