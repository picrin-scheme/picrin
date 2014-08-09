#include "picrin.h"
#include "picrin/proc.h"

#if PIC_ENABLE_LIBUV

#include <uv.h>

typedef struct {
  pic_state *pic;
  struct pic_proc *proc;
} async_data_t;

static void
pic_async_set_timeout_cb(uv_timer_t *req, int status)
{
  async_data_t *dat = (async_data_t *)req->data;
  pic_state *pic = dat->pic;
  struct pic_proc *proc = dat->proc;

  uv_timer_stop(req);

  pic_free(pic, dat);
  pic_free(pic, req);

  pic_apply0(pic, proc);
}

static pic_value
pic_async_set_timeout(pic_state *pic)
{
  struct pic_proc *proc;
  int i;
  uv_timer_t *req;
  async_data_t *dat;

  pic_get_args(pic, "li", &proc, &i);

  dat = pic_alloc(pic, sizeof(async_data_t));
  dat->pic = pic;
  dat->proc = proc;

  req = pic_alloc(pic, sizeof(uv_timer_t));
  req->data = dat;

  uv_timer_init(uv_default_loop(), req);
  uv_timer_start(req, pic_async_set_timeout_cb, i, 0);

  return pic_none_value();
}

#endif

void
pic_init_async(pic_state *pic)
{
  UNUSED(pic);

#if PIC_ENABLE_LIBUV

  pic_deflibrary (pic, "(picrin async)") {
    pic_defun(pic, "set-timeout", pic_async_set_timeout);
  }

#endif
}
