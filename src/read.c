/**
 * See Copyright Notice in picrin.h
 */

#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include "picrin.h"
#include "picrin/read.h"
#include "picrin/error.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/vector.h"
#include "picrin/blob.h"
#include "picrin/port.h"

static pic_value read(pic_state *pic, struct pic_port *port, int c);
static pic_value read_nullable(pic_state *pic, struct pic_port *port, int c);

static noreturn void
read_error(pic_state *pic, const char *msg)
{
  pic_throw(pic, PIC_ERROR_READ, msg, pic_nil_value());
}

static int
skip(struct pic_port *port, int c)
{
  while (isspace(c)) {
    c = xfgetc(port->file);
  }
  return c;
}

static int
next(struct pic_port *port)
{
  return xfgetc(port->file);
}

static int
peek(struct pic_port *port)
{
  int c;

  xungetc((c = xfgetc(port->file)), port->file);

  return c;
}

static bool
expect(struct pic_port *port, const char *str)
{
  int c;

  while ((c = (int)*str++) != 0) {
    if (c != peek(port))
      return false;
    next(port);
  }

  return true;
}

static bool
isdelim(int c)
{
  return c == EOF || strchr("();,|\" \t\n\r", c) != NULL; /* ignores "#", "'" */
}

static bool
strcaseeq(const char *s1, const char *s2)
{
  char a, b;

  while ((a = *s1++) * (b = *s2++)) {
    if (tolower(a) != tolower(b))
      return false;
  }
  return a == b;
}

static pic_value
read_comment(pic_state *pic, struct pic_port *port, int c)
{
  UNUSED(pic);

  do {
    c = next(port);
  } while (! (c == EOF || c == '\n'));

  return pic_undef_value();
}

static pic_value
read_block_comment(pic_state *pic, struct pic_port *port, int c)
{
  int x, y;
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
read_datum_comment(pic_state *pic, struct pic_port *port, int c)
{
  UNUSED(c);

  read(pic, port, next(port));

  return pic_undef_value();
}

static pic_value
read_directive(pic_state *pic, struct pic_port *port, int c)
{
  switch (peek(port)) {
  case 'n':
    if (expect(port, "no-fold-case")) {
      pic->reader->typecase = PIC_CASE_DEFAULT;
      return pic_undef_value();
    }
    break;
  case 'f':
    if (expect(port, "fold-case")) {
      pic->reader->typecase = PIC_CASE_FOLD;
      return pic_undef_value();
    }
    break;
  }

  return read_comment(pic, port, c);
}

static pic_value
read_eval(pic_state *pic, struct pic_port *port, int c)
{
  pic_value form;

  UNUSED(c);

  form = read(pic, port, next(port));

  return pic_eval(pic, form, pic->lib);
}

static pic_value
read_quote(pic_state *pic, struct pic_port *port, int c)
{
  UNUSED(c);

  return pic_list2(pic, pic_sym_value(pic->sQUOTE), read(pic, port, next(port)));
}

static pic_value
read_quasiquote(pic_state *pic, struct pic_port *port, int c)
{
  UNUSED(c);

  return pic_list2(pic, pic_sym_value(pic->sQUASIQUOTE), read(pic, port, next(port)));
}

static pic_value
read_comma(pic_state *pic, struct pic_port *port, int c)
{
  c = next(port);

  if (c == '@') {
    return pic_list2(pic, pic_sym_value(pic->sUNQUOTE_SPLICING), read(pic, port, next(port)));
  } else {
    return pic_list2(pic, pic_sym_value(pic->sUNQUOTE), read(pic, port, c));
  }
}

static pic_value
read_symbol(pic_state *pic, struct pic_port *port, int c)
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
    if (pic->reader->typecase == PIC_CASE_FOLD) {
      c = tolower(c);
    }
    len += 1;
    buf = pic_realloc(pic, buf, len + 1);
    buf[len - 1] = c;
  } while (! isdelim(peek(port)));

  sym = pic_intern(pic, buf, len);
  pic_free(pic, buf);

  return pic_sym_value(sym);
}

static size_t
read_uinteger(pic_state *pic, struct pic_port *port, int c, char buf[])
{
  size_t i = 0;

  if (! isdigit(c)) {
    read_error(pic, "expected one or more digits");
  }

  buf[i++] = c;
  while (isdigit(c = peek(port))) {
    buf[i++] = next(port);
  }

  buf[i] = '\0';

  return i;
}

static size_t
read_suffix(pic_state *pic, struct pic_port *port, char buf[])
{
  size_t i = 0;
  char c;

  c = peek(port);

  if (c != 'e' && c != 'E') {
    return i;
  }

  buf[i++] = next(port);

  switch ((c = next(port))) {
  case '-':
  case '+':
    buf[i++] = c;
    c = next(port);
  default:
    return i + read_uinteger(pic, port, c, buf + i);
  }
}

static pic_value
read_number(pic_state *pic, struct pic_port *port, int c)
{
  char buf[256];
  size_t i;

  i = read_uinteger(pic, port, c, buf);

  switch (peek(port)) {
  case '.':
    buf[i++] = next(port);
    i += read_uinteger(pic, port, next(port), buf + i);
    read_suffix(pic, port, buf + i);
    return pic_float_value(atof(buf));

  default:
    read_suffix(pic, port, buf + i);
    return pic_int_value((int)atof(buf));
  }
}

static pic_value
negate(pic_value n)
{
  if (pic_int_p(n)) {
    return pic_int_value(-pic_int(n));
  } else {
    return pic_float_value(-pic_float(n));
  }
}

static pic_value
read_minus(pic_state *pic, struct pic_port *port, int c)
{
  pic_value sym;

  if (isdigit(peek(port))) {
    return negate(read_number(pic, port, next(port)));
  }
  else {
    sym = read_symbol(pic, port, c);
    if (strcaseeq(pic_symbol_name(pic, pic_sym(sym)), "-inf.0")) {
      return pic_float_value(-INFINITY);
    }
    if (strcaseeq(pic_symbol_name(pic, pic_sym(sym)), "-nan.0")) {
      return pic_float_value(-NAN);
    }
    return sym;
  }
}

static pic_value
read_plus(pic_state *pic, struct pic_port *port, int c)
{
  pic_value sym;

  if (isdigit(peek(port))) {
    return read_number(pic, port, next(port));
  }
  else {
    sym = read_symbol(pic, port, c);
    if (strcaseeq(pic_symbol_name(pic, pic_sym(sym)), "+inf.0")) {
      return pic_float_value(INFINITY);
    }
    if (strcaseeq(pic_symbol_name(pic, pic_sym(sym)), "+nan.0")) {
      return pic_float_value(NAN);
    }
    return read_symbol(pic, port, c);
  }
}

static pic_value
read_boolean(pic_state *pic, struct pic_port *port, int c)
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
read_char(pic_state *pic, struct pic_port *port, int c)
{
  c = next(port);

  if (! isdelim(peek(port))) {
    switch (c) {
    default: read_error(pic, "unexpected character after char literal");
    case 'a': c = '\a'; if (! expect(port, "lerm")) goto fail; break;
    case 'b': c = '\b'; if (! expect(port, "ackspace")) goto fail; break;
    case 'd': c = 0x7F; if (! expect(port, "elete")) goto fail; break;
    case 'e': c = 0x1B; if (! expect(port, "scape")) goto fail; break;
    case 'n':
      if ((c = peek(port)) == 'e') {
        c = '\n';
        if (! expect(port, "ewline"))
          goto fail;
      } else {
        c = '\0';
        if (! expect(port, "ull"))
          goto fail;
      }
      break;
    case 'r': c = '\r'; if (! expect(port, "eturn")) goto fail; break;
    case 's': c = ' '; if (! expect(port, "pace")) goto fail; break;
    case 't': c = '\t'; if (! expect(port, "ab")) goto fail; break;
    }
  }

  return pic_char_value(c);

 fail:
  read_error(pic, "unexpected character while reading character literal");
}

static pic_value
read_string(pic_state *pic, struct pic_port *port, int c)
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
read_pipe(pic_state *pic, struct pic_port *port, int c)
{
  char *buf;
  size_t size, cnt;
  pic_sym sym;
  /* Currently supports only ascii chars */
  char HEX_BUF[3];
  size_t i = 0;

  size = 256;
  buf = pic_alloc(pic, size);
  cnt = 0;
  while ((c = next(port)) != '|') {
    if (c == '\\') {
      switch ((c = next(port))) {
      case 'a': c = '\a'; break;
      case 'b': c = '\b'; break;
      case 't': c = '\t'; break;
      case 'n': c = '\n'; break;
      case 'r': c = '\r'; break;
      case 'x':
        i = 0;
        while ((HEX_BUF[i++] = next(port)) != ';') {
          if (i >= sizeof HEX_BUF)
            read_error(pic, "expected ';'");
        }
        c = strtol(HEX_BUF, NULL, 16);
        break;
      }
    }
    buf[cnt++] = c;
    if (cnt >= size) {
      buf = pic_realloc(pic, buf, size *= 2);
    }
  }
  buf[cnt] = '\0';

  sym = pic_intern_cstr(pic, buf);
  pic_free(pic, buf);

  return pic_sym_value(sym);
}

static pic_value
read_unsigned_blob(pic_state *pic, struct pic_port *port, int c)
{
  int nbits, n;
  size_t len, i;
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
    read_uinteger(pic, port, c, buf);
    n = atoi(buf);
    if (n < 0 || (1 << nbits) <= n) {
      read_error(pic, "invalid element in bytevector literal");
    }
    len += 1;
    dat = pic_realloc(pic, dat, len);
    dat[len - 1] = n;
    c = next(port);
  }

  blob = pic_blob_new(pic, len);
  for (i = 0; i < len; ++i) {
    blob->data[i] = dat[i];
  }

  pic_free(pic, dat);
  return pic_obj_value(blob);
}

static pic_value
read_pair(pic_state *pic, struct pic_port *port, int c)
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
read_vector(pic_state *pic, struct pic_port *port, int c)
{
  pic_value list;

  list = read(pic, port, c);

  return pic_obj_value(pic_vec_new_from_list(pic, list));
}

static pic_value
read_label_set(pic_state *pic, struct pic_port *port, int i)
{
  pic_value val;
  int c;

  switch ((c = skip(port, ' '))) {
  case '(': case '[':
    {
      pic_value tmp;

      val = pic_cons(pic, pic_none_value(), pic_none_value());

      xh_put_int(&pic->reader->labels, i, &val);

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

        xh_put_int(&pic->reader->labels, i, &val);

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

      xh_put_int(&pic->reader->labels, i, &val);

      return val;
    }
  }
}

static pic_value
read_label_ref(pic_state *pic, struct pic_port *port, int i)
{
  xh_entry *e;

  UNUSED(port);

  e = xh_get_int(&pic->reader->labels, i);
  if (! e) {
    read_error(pic, "label of given index not defined");
  }
  return xh_val(e, pic_value);
}

static pic_value
read_label(pic_state *pic, struct pic_port *port, int c)
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
read_unmatch(pic_state *pic, struct pic_port *port, int c)
{
  UNUSED(port);
  UNUSED(c);

  read_error(pic, "unmatched parenthesis");
}

static pic_value
read_dispatch(pic_state *pic, struct pic_port *port, int c)
{
  c = next(port);

  switch (c) {
  case '!':
    return read_directive(pic, port, c);
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
  case '.':
    return read_eval(pic, port, c);
  default:
    read_error(pic, "unexpected dispatch character");
  }
}

static pic_value
read_nullable(pic_state *pic, struct pic_port *port, int c)
{
  c = skip(port, c);

  if (c == EOF) {
    read_error(pic, "unexpected EOF");
  }

  switch (c) {
  case ')':
    return read_unmatch(pic, port, c);
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
  case '|':
    return read_pipe(pic, port, c);
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    return read_number(pic, port, c);
  case '+':
    return read_plus(pic, port, c);
  case '-':
    return read_minus(pic, port, c);
  case '(': case '[':
    return read_pair(pic, port, c);
  default:
    return read_symbol(pic, port, c);
  }
}

static pic_value
read(pic_state *pic, struct pic_port *port, int c)
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
  int c = next(port);

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
  pic_deflibrary (pic, "(scheme read)") {
    pic_defun(pic, "read", pic_read_read);
  }
}
