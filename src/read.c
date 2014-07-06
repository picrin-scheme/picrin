/**
 * See Copyright Notice in picrin.h
 */

#include <ctype.h>
#include <math.h>
#include "picrin.h"
#include "picrin/error.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/vector.h"
#include "picrin/blob.h"
#include "picrin/port.h"

typedef pic_value (*read_func_t)(pic_state *, struct pic_port *, char);

static pic_value read(pic_state *pic, struct pic_port *port, char c);
static pic_value read_nullable(pic_state *pic, struct pic_port *port, char c);

static noreturn void
read_error(pic_state *pic, const char *msg)
{
  pic_throw(pic, PIC_ERROR_READ, msg, pic_nil_value());
}

static char
skip(struct pic_port *port, char c)
{
  while (isspace(c)) {
    c = xfgetc(port->file);
  }
  return c;
}

static char
next(struct pic_port *port)
{
  return xfgetc(port->file);
}

static char
peek(struct pic_port *port)
{
  char c;

  xungetc((c = xfgetc(port->file)), port->file);

  return c;
}

static bool
expect(struct pic_port *port, const char *str)
{
  char c;

  while ((c = *str++) != 0) {
    if (c != next(port))
      return false;
  }

  return true;
}

static bool
isdelim(char c)
{
  return c == EOF || strchr("();,|\" \t\n\r", c) != NULL; /* ignores "#", "'" */
}

static bool
is_digit_of_base(char c, size_t base){
  switch(base){
  case 2:
    return '0' <= c && c <= '1';
  case 8:
    return '0' <= c && c <= '7';
  case 10:
    return '0' <= c && c <= '9';
  case 16:
    return ('0' <= c && c <= '9') || ('a' <= (c | 32) && c <= 'f');
  default:
    return false;
  }
}

static pic_value
read_comment(pic_state *pic, struct pic_port *port, char c)
{
  UNUSED(pic);

  do {
    c = next(port);
  } while (! (c == EOF || c == '\n'));

  return pic_undef_value();
}

static pic_value
read_block_comment(pic_state *pic, struct pic_port *port, char c)
{
  char x, y;
  int i = 1;

  UNUSED(pic);
  UNUSED(c);

  y = next(port);

  while (y != EOF && i > 0) {
    x = y;
    y = next(port);
    if (x == '|' && y == '#') {
      i--;
    }
    if (x == '#' && y == '|') {
      i++;
    }
  }

  return pic_undef_value();
}

static pic_value
read_datum_comment(pic_state *pic, struct pic_port *port, char c)
{
  UNUSED(c);

  read(pic, port, next(port));

  return pic_undef_value();
}

static pic_value
read_quote(pic_state *pic, struct pic_port *port, char c)
{
  UNUSED(c);

  return pic_list2(pic, pic_sym_value(pic->sQUOTE), read(pic, port, next(port)));
}

static pic_value
read_quasiquote(pic_state *pic, struct pic_port *port, char c)
{
  UNUSED(c);

  return pic_list2(pic, pic_sym_value(pic->sQUASIQUOTE), read(pic, port, next(port)));
}

static pic_value
read_comma(pic_state *pic, struct pic_port *port, char c)
{
  c = next(port);

  if (c == '@') {
    return pic_list2(pic, pic_sym_value(pic->sUNQUOTE_SPLICING), read(pic, port, next(port)));
  } else {
    return pic_list2(pic, pic_sym_value(pic->sUNQUOTE), read(pic, port, c));
  }
}

static pic_value
read_symbol(pic_state *pic, struct pic_port *port, char c)
{
  size_t len;
  char *buf;
  pic_sym sym;

  len = 0;
  buf = NULL;

  do {
    if (len != 0) {
      c = next(port);
    }
    len += 1;
    buf = pic_realloc(pic, buf, len + 1);
    buf[len - 1] = c;
  } while (! isdelim(peek(port)));

  buf[len] = '\0';
  sym = pic_intern_cstr(pic, buf);
  pic_free(pic, buf);

  return pic_sym_value(sym);
}

static size_t
read_uinteger(pic_state *pic, struct pic_port *port, char c, char buf[], size_t base)
{
  size_t i = 0;

  if (! is_digit_of_base(c, base)) {
    read_error(pic, "expected one or more digits");
  }

  buf[i++] = c;
  while (is_digit_of_base(c = peek(port), base)) {
    buf[i++] = next(port);
  }

  buf[i] = '\0';

  return i;
}

static pic_value
read_number(pic_state *pic, struct pic_port *port, char c, size_t base, enum exactness exactp)
{
  char buf[256], peek_char;
  size_t i = 0;
  pic_value v;

  if(c == '.')
    goto READ_FLOAT;
  i = read_uinteger(pic, port, c, buf, base);
  peek_char = peek(port);
  switch(peek_char){
  case '.': case 'e':
    READ_FLOAT:
    {
    pic_bigfloat *f = pic_bigfloat_new(pic);
    do {
      buf[i++] = next(port);
    } while (is_digit_of_base(peek(port), base));
    buf[i] = '\0';

    mpfr_set_str(f->f, buf, base, MPFR_RNDN);
    if(exactp == EXACT){
      if(mpfr_fits_sint_p(f->f, MPFR_RNDN)){
        v = pic_int_value(mpfr_get_si(f->f, MPFR_RNDN));
      }
      else {
        pic_bigint *z = pic_bigint_new(pic);
        mpfr_get_z(z->z, f->f, MPFR_RNDN);
        v = pic_obj_value(z);
      }
    }
    else{
      v = pic_obj_value(f);
    }
    break;
  }
  case '/': {
    pic_rational *q = pic_rational_new(pic);
    do {
      buf[i++] = next(port);
    } while (is_digit_of_base(peek(port), base));
    buf[i] = '\0';
    mpq_set_str(q->q, buf, base);
    mpq_canonicalize(q->q);

    if(exactp == INEXACT){
      pic_bigfloat *f = pic_bigfloat_new(pic);
      mpfr_set_q(f->f, q->q, MPFR_RNDN);
      v = pic_obj_value(f);
    }
    else{
      v = pic_obj_value(q);
    }
    break;
  }
  default:{
    pic_bigint *z = pic_bigint_new(pic);
    mpz_set_str(z->z, buf, base);
    if(exactp == INEXACT){
      pic_bigfloat *f = pic_bigfloat_new(pic);
      mpfr_set_z(f->f, z->z, MPFR_RNDN);
      v = pic_obj_value(f);
    }
    else{
      v = pic_obj_value(z);
    }
    break;
  }
  }
  pic_number_normalize(pic, &v, INEXACT);
  return v;
}

static pic_value
negate(pic_value n)
{
  switch(pic_type(n)){
  case PIC_TT_INT:
    return pic_int_value(-pic_int(n));
  case PIC_TT_FLOAT:
    return pic_float_value(-pic_float(n));
  case PIC_TT_BIGINT:
    mpz_neg(pic_bigint_ptr(n)->z, pic_bigint_ptr(n)->z);
    return n;
  case PIC_TT_RATIONAL:
    mpq_neg(pic_rational_ptr(n)->q, pic_rational_ptr(n)->q);
    return n;
  case PIC_TT_BIGFLOAT:
    mpfr_neg(pic_bigfloat_ptr(n)->f, pic_bigfloat_ptr(n)->f, MPFR_RNDN);
    return n;
  default:
    return pic_none_value();
  }
}

static pic_value
read_minus(pic_state *pic, struct pic_port *port, char c, size_t base, enum exactness exactp)
{
  pic_value sym;

  if (is_digit_of_base(peek(port), base) || peek(port) == '.') {
    return negate(read_number(pic, port, next(port), base, exactp));
  }
  else {
    sym = read_symbol(pic, port, c);
    if (pic_eq_p(sym, pic_sym_value(pic_intern_cstr(pic, "-inf.0")))) {
      return pic_float_value(-INFINITY);
    }
    if (pic_eq_p(sym, pic_sym_value(pic_intern_cstr(pic, "-nan.0")))) {
      return pic_float_value(-NAN);
    }
    return sym;
  }
}

static pic_value
read_plus(pic_state *pic, struct pic_port *port, char c, size_t base, enum exactness exactp)
{
  pic_value sym;

  if (is_digit_of_base(peek(port), base) || peek(port) == '.') {
    return read_number(pic, port, next(port), base, exactp);
  }
  else {
    sym = read_symbol(pic, port, c);
    if (pic_eq_p(sym, pic_sym_value(pic_intern_cstr(pic, "+inf.0")))) {
      return pic_float_value(INFINITY);
    }
    if (pic_eq_p(sym, pic_sym_value(pic_intern_cstr(pic, "+nan.0")))) {
      return pic_float_value(NAN);
    }
    return read_symbol(pic, port, c);
  }
}

static pic_value
read_sigined_number(pic_state *pic, struct pic_port *port, char c, size_t base, enum exactness exactp)
{
  if(is_digit_of_base(c, base) || c == '.')
    return read_number(pic, port, c, base, exactp);
  else if( c == '-')
    return read_minus(pic, port, c, 10, exactp);
  else
    return read_plus(pic, port, c, 10, exactp);
}

static pic_value
read_number_dispatch_exactness(pic_state *pic, struct pic_port *port, char c)
{
  enum exactness exactp = c == 'e' ? EXACT : INEXACT;
  char d = next(port);
  if(is_digit_of_base(d, 10))
    return read_number(pic, port, d, 10, exactp);
  switch(d){
  case '+':
    return read_plus(pic, port, d, 10, exactp);
  case '-':
    return read_minus(pic, port, d, 10, exactp);
  case '#':
    d = next(port);
    switch(d){
    case 'b': case 'B':
      return read_sigined_number(pic, port, next(port), 2, exactp);
    case 'o': case 'O':
      return read_sigined_number(pic, port, next(port), 8, exactp);
    case 'd': case 'D':
      return read_sigined_number(pic, port, next(port), 10, exactp);
    case 'x': case 'X':
      return read_sigined_number(pic, port, next(port), 16, exactp);
    default:
      read_error(pic, "invalid base character");
    }
  default:
    read_error(pic, "invalid character after exactness");
  }
}

static pic_value
read_number_dispatch_base(pic_state *pic, struct pic_port *port, char c)
{
  size_t base =
    c == 'b' ? 2 :
    c == 'o' ? 8 :
    c == 'd' ? 10 :
    16;

  char d = next(port);
  if(is_digit_of_base(d, base))
    return read_number(pic, port, d, base, MAYBE);
  switch(d){
  case '+':
    return read_plus(pic, port, d, base, MAYBE);
  case '-':
    return read_minus(pic, port, d, base, MAYBE);
  case '#':
    d = next(port);
    switch(d){
    case 'e': case 'E':
      return read_sigined_number(pic, port, d, base, EXACT);
    case 'i': case 'I':
      return read_sigined_number(pic, port, d, base, INEXACT);
    default:
      read_error(pic, "invalid exactness character");
    }
  default:
    read_error(pic, "invalid character after base designator");
  }
}

static pic_value
read_boolean(pic_state *pic, struct pic_port *port, char c)
{
  UNUSED(pic);
  UNUSED(port);

  if (! isdelim(peek(port))) {
    if (c == 't') {
      if (! expect(port, "rue")) {
        goto fail;
      }
    } else {
      if (! expect(port, "alse")) {
        goto fail;
      }
    }
  }

  if (c == 't') {
    return pic_true_value();
  } else {
    return pic_false_value();
  }

 fail:
  read_error(pic, "illegal character during reading boolean literal");
}

static pic_value
read_char(pic_state *pic, struct pic_port *port, char c)
{
  UNUSED(pic);
  UNUSED(c);

  /* TODO: #\alart, #\space, so on and so on */

  return pic_char_value(next(port));
}

static pic_value
read_string(pic_state *pic, struct pic_port *port, char c)
{
  char *buf;
  size_t size, cnt;
  pic_str *str;

  size = 256;
  buf = pic_alloc(pic, size);
  cnt = 0;

  /* TODO: intraline whitespaces */

  while ((c = next(port)) != '"') {
    if (c == '\\') {
      switch (c = next(port)) {
      case 'a': c = '\a'; break;
      case 'b': c = '\b'; break;
      case 't': c = '\t'; break;
      case 'n': c = '\n'; break;
      case 'r': c = '\r'; break;
      }
    }
    buf[cnt++] = c;
    if (cnt >= size) {
      buf = pic_realloc(pic, buf, size *= 2);
    }
  }
  buf[cnt] = '\0';

  str = pic_str_new(pic, buf, cnt);
  pic_free(pic, buf);
  return pic_obj_value(str);
}

static pic_value
read_unsigned_blob(pic_state *pic, struct pic_port *port, char c)
{
  int nbits, n;
  size_t len;
  char *dat, buf[256];
  pic_blob *blob;

  nbits = 0;

  while (isdigit(c = next(port))) {
    nbits = 10 * nbits + c - '0';
  }

  if (nbits != 8) {
    read_error(pic, "unsupported bytevector bit width");
  }

  if (c != '(') {
    read_error(pic, "expected '(' character");
  }

  len = 0;
  dat = NULL;
  c = next(port);
  while ((c = skip(port, c)) != ')') {
    read_uinteger(pic, port, c, buf, 10);
    n = atoi(buf);
    if (n < 0 || (1 << nbits) <= n) {
      read_error(pic, "invalid element in bytevector literal");
    }
    len += 1;
    dat = pic_realloc(pic, dat, len);
    dat[len - 1] = n;
    c = next(port);
  }

  blob = pic_blob_new(pic, dat, len);
  pic_free(pic, dat);
  return pic_obj_value(blob);
}

static pic_value
read_pair(pic_state *pic, struct pic_port *port, char c)
{
  char tOPEN = c, tCLOSE = (tOPEN == '(') ? ')' : ']';
  pic_value car, cdr;

 retry:

  c = skip(port, ' ');

  if (c == tCLOSE) {
    return pic_nil_value();
  }
  if (c == '.' && isdelim(peek(port))) {
    cdr = read(pic, port, next(port));

  closing:
    if ((c = skip(port, ' ')) != tCLOSE) {
      if (pic_undef_p(read_nullable(pic, port, c))) {
        goto closing;
      }
      read_error(pic, "unmatched parenthesis");
    }
    return cdr;
  }
  else {
    car = read_nullable(pic, port, c);

    if (pic_undef_p(car)) {
      goto retry;
    }

    cdr = read_pair(pic, port, tOPEN); /* FIXME: don't use recursion */
    return pic_cons(pic, car, cdr);
  }
}

static pic_value
read_vector(pic_state *pic, struct pic_port *port, char c)
{
  pic_value list;

  list = read(pic, port, c);

  return pic_obj_value(pic_vec_new_from_list(pic, list));
}

static pic_value
read_label_set(pic_state *pic, struct pic_port *port, int i)
{
  pic_value val;
  char c;

  switch (c = skip(port, ' ')) {
  case '(': case '[':
    {
      pic_value tmp;

      val = pic_cons(pic, pic_none_value(), pic_none_value());

      xh_put_int(&pic->rlabels, i, &val);

      tmp = read(pic, port, c);
      pic_pair_ptr(val)->car = pic_car(pic, tmp);
      pic_pair_ptr(val)->cdr = pic_cdr(pic, tmp);

      return val;
    }
  case '#':
    {
      bool vect;

      if (peek(port) == '(') {
        vect = true;
      } else {
        vect = false;
      }

      if (vect) {
        pic_vec *tmp;

        val = pic_obj_value(pic_vec_new(pic, 0));

        xh_put_int(&pic->rlabels, i, &val);

        tmp = pic_vec_ptr(read(pic, port, c));
        SWAP(pic_value *, tmp->data, pic_vec_ptr(val)->data);
        SWAP(size_t, tmp->len, pic_vec_ptr(val)->len);

        return val;
      }

      FALLTHROUGH;
    }
  default:
    {
      val = read(pic, port, c);

      xh_put_int(&pic->rlabels, i, &val);

      return val;
    }
  }
}

static pic_value
read_label_ref(pic_state *pic, struct pic_port *port, int i)
{
  xh_entry *e;

  UNUSED(port);

  e = xh_get_int(&pic->rlabels, i);
  if (! e) {
    read_error(pic, "label of given index not defined");
  }
  return xh_val(e, pic_value);
}

static pic_value
read_label(pic_state *pic, struct pic_port *port, char c)
{
  int i;

  i = 0;
  do {
    i = i * 10 + c;
  } while (isdigit(c = next(port)));

  if (c == '=') {
    return read_label_set(pic, port, i);
  }
  if (c == '#') {
    return read_label_ref(pic, port, i);
  }
  read_error(pic, "broken label expression");
}

static pic_value
read_dispatch(pic_state *pic, struct pic_port *port, char c)
{
  c = next(port);

  switch (c) {
  case '!':
    return read_comment(pic, port, c);
  case '|':
    return read_block_comment(pic, port, c);
  case ';':
    return read_datum_comment(pic, port, c);
  case 't': case 'f':
    return read_boolean(pic, port, c);
  case '\\':
    return read_char(pic, port, c);
  case '(':
    return read_vector(pic, port, c);
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    return read_label(pic, port, c);
  case 'u':
    return read_unsigned_blob(pic, port, c);
  case 'i': case 'e': case 'I': case 'E':
    return read_number_dispatch_exactness(pic, port, c | 32);
  case 'b': case 'o': case 'd': case 'x':
  case 'B': case 'O': case 'D': case 'X':
    return read_number_dispatch_base(pic, port, c | 32);
  default:
    read_error(pic, "unexpected dispatch character");
  }
}

static pic_value
read_nullable(pic_state *pic, struct pic_port *port, char c)
{
  c = skip(port, c);

  if (c == EOF) {
    read_error(pic, "unexpected EOF");
  }

  switch (c) {
  case ';':
    return read_comment(pic, port, c);
  case '#':
    return read_dispatch(pic, port, c);
  case '\'':
    return read_quote(pic, port, c);
  case '`':
    return read_quasiquote(pic, port, c);
  case ',':
    return read_comma(pic, port, c);
  case '"':
    return read_string(pic, port, c);
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    return read_number(pic, port, c, 10, true);
  case '+':
    return read_plus(pic, port, c, 10, true);
  case '-':
    return read_minus(pic, port, c, 10, true);
  case '(': case '[':
    return read_pair(pic, port, c);
  default:
    return read_symbol(pic, port, c);
  }
}

static pic_value
read(pic_state *pic, struct pic_port *port, char c)
{
  pic_value val;

 retry:
  val = read_nullable(pic, port, c);

  if (pic_undef_p(val)) {
    c = next(port);
    goto retry;
  }

  return val;
}

pic_value
pic_read(pic_state *pic, struct pic_port *port)
{
  pic_value val;
  char c = next(port);

 retry:
  c = skip(port, c);

  if (c == EOF) {
    return pic_eof_object();
  }

  val = read_nullable(pic, port, c);

  if (pic_undef_p(val)) {
    c = next(port);
    goto retry;
  }

  return val;
}

pic_value
pic_read_cstr(pic_state *pic, const char *str)
{
  struct pic_port *port;

  port = pic_open_input_string(pic, str);

  return pic_read(pic, port);
}

static pic_value
pic_parse(pic_state *pic, struct pic_port *port)
{
  pic_value val, acc;

  pic_try {
    acc = pic_nil_value();
    while (! pic_eof_p(val = pic_read(pic, port))) {
      pic_push(pic, val, acc);
    }
  }
  pic_catch {
    return pic_undef_value();
  }

  return pic_reverse(pic, acc);
}

pic_list
pic_parse_file(pic_state *pic, FILE *file)
{
  struct pic_port *port;

  port = (struct pic_port *)pic_obj_alloc(pic, sizeof(struct pic_port *), PIC_TT_PORT);
  port->file = xfpopen(file);
  port->flags = PIC_PORT_OUT | PIC_PORT_TEXT;
  port->status = PIC_PORT_OPEN;

  return pic_parse(pic, port);
}

pic_list
pic_parse_cstr(pic_state *pic, const char *str)
{
  struct pic_port *port;

  port = pic_open_input_string(pic, str);

  return pic_parse(pic, port);
}

static pic_value
pic_read_read(pic_state *pic)
{
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  return pic_read(pic, port);
}

void
pic_init_read(pic_state *pic)
{
  pic_deflibrary ("(scheme read)") {
    pic_defun(pic, "read", pic_read_read);
  }
}
