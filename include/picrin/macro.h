#ifndef MACRO_H__
#define MACRO_H__

struct pic_senv {
  PIC_OBJECT_HEADER
  struct pic_senv *up;
  struct xhash *tbl;
  struct pic_syntax **stx;
  size_t xlen, xcapa;
};

struct pic_syntax {
  PIC_OBJECT_HEADER
  enum {
    PIC_STX_DEFINE,
    PIC_STX_SET,
    PIC_STX_QUOTE,
    PIC_STX_LAMBDA,
    PIC_STX_IF,
    PIC_STX_BEGIN
  } kind;
  pic_sym sym;
};

#define pic_syntax(v) ((struct pic_syntax *)pic_ptr(v))
#define pic_syntax_p(v) (pic_type(v) == PIC_TT_SYNTAX)

struct pic_syntax *pic_syntax_new(pic_state *, int kind, pic_sym sym);

#endif
