/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/read.h"
#include "picrin/error.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/vector.h"
#include "picrin/blob.h"
#include "picrin/port.h"
#include "picrin/proc.h"
#include "picrin/symbol.h"

static pic_value read(pic_state *pic, struct pic_port *port, int c);
static pic_value read_nullable(pic_state *pic, struct pic_port *port, int c);

PIC_NORETURN static void
read_error(pic_state *pic, const char *msg)
{
  pic_throw(pic, pic->sREAD, msg, pic_nil_value());
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

#if PIC_ENABLE_FLOAT
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
#endif

static int
case_fold(pic_state *pic, int c)
{
  if (pic->reader->typecase == PIC_CASE_FOLD) {
    c = tolower(c);
  }
  return c;
}

static pic_value
read_comment(pic_state *pic, struct pic_port *port, int c)
{
  PIC_UNUSED(pic);

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

  PIC_UNUSED(pic);
  PIC_UNUSED(c);

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
  PIC_UNUSED(c);

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

  PIC_UNUSED(c);

  form = read(pic, port, next(port));

  return pic_eval(pic, form, pic->lib);
}

static pic_value
read_quote(pic_state *pic, struct pic_port *port, int c)
{
  PIC_UNUSED(c);

  return pic_list2(pic, pic_obj_value(pic->sQUOTE), read(pic, port, next(port)));
}

static pic_value
read_quasiquote(pic_state *pic, struct pic_port *port, int c)
{
  PIC_UNUSED(c);

  return pic_list2(pic, pic_obj_value(pic->sQUASIQUOTE), read(pic, port, next(port)));
}

static pic_value
read_unquote(pic_state *pic, struct pic_port *port, int c)
{
  pic_sym *tag = pic->sUNQUOTE;

  PIC_UNUSED(c);

  if (peek(port) == '@') {
    tag = pic->sUNQUOTE_SPLICING;
    next(port);
  }
  return pic_list2(pic, pic_obj_value(tag), read(pic, port, next(port)));
}

static pic_value
read_symbol(pic_state *pic, struct pic_port *port, int c)
{
  size_t len;
  char *buf;
  pic_sym *sym;

  len = 1;
  buf = pic_alloc(pic, len + 1);
  buf[0] = case_fold(pic, c);
  buf[1] = 0;

  while (! isdelim(peek(port))) {
    c = next(port);
    len += 1;
    buf = pic_realloc(pic, buf, len + 1);
    buf[len - 1] = case_fold(pic, c);
    buf[len] = 0;
  }

  sym = pic_intern_cstr(pic, buf);
  pic_free(pic, buf);

  return pic_obj_value(sym);
}

static unsigned
read_uinteger(pic_state *pic, struct pic_port *port, int c)
{
  unsigned u = 0;

  if (! isdigit(c)) {
    read_error(pic, "expected one or more digits");
  }

  u = c - '0';
  while (isdigit(c = peek(port))) {
    u = u * 10 + next(port) - '0';
  }

  return u;
}

static int
read_suffix(pic_state *pic, struct pic_port *port)
{
  int c, s = 1;

  c = peek(port);

  if (c != 'e' && c != 'E') {
    return 0;
  }

  next(port);

  switch ((c = next(port))) {
  case '-':
    s = -1;
  case '+':
    c = next(port);
  default:
    return s * read_uinteger(pic, port, c);
  }
}

static pic_value
read_unsigned(pic_state *pic, struct pic_port *port, int c)
{
  unsigned u, w = 0;
  int exp, s, i, e;
#if PIC_ENABLE_FLOAT
  double f;
#endif

  u = read_uinteger(pic, port, c);

  switch (peek(port)) {
#if PIC_ENABLE_FLOAT
  case '.':
    next(port);
    w = 0, f = 1;
    while (isdigit(c = peek(port))) {
      w = w * 10 + next(port) - '0';
      f /= 10;
    }
    f = u + w * f;

    exp = read_suffix(pic, port);
    if (exp >= 0) {
      s = 0;
    } else {
      exp = -exp;
      s = 1;
    }

    e = 10;
    for (i = 0; exp; ++i) {
      if ((exp & 1) != 0) {
        f = s ? f / e : (f * e);
      }
      e *= e;
      exp >>= 1;
    }
    return pic_float_value(f);
#endif

  default:
    exp = read_suffix(pic, port);
    if (exp >= 0) {
      s = 0;
    } else {
      exp = -exp;
      s = 1;
    }

    e = 10;
    for (i = 0; exp; ++i) {
      if ((exp & 1) != 0) {
        u = s ? u / e : (u * e);
      }
      e *= e;
      exp >>= 1;
    }

    return pic_int_value(u);
  }
}

static pic_value
read_number(pic_state *pic, struct pic_port *port, int c)
{
  return read_unsigned(pic, port, c);
}

static pic_value
negate(pic_value n)
{
#if PIC_ENABLE_FLOAT
  if (pic_int_p(n)) {
    return pic_int_value(-pic_int(n));
  } else {
    return pic_float_value(-pic_float(n));
  }
#else
  return pic_int_value(-pic_int(n));
#endif
}

static pic_value
read_minus(pic_state *pic, struct pic_port *port, int c)
{
  pic_value sym;

  if (isdigit(peek(port))) {
    return negate(read_unsigned(pic, port, next(port)));
  }
  else {
    sym = read_symbol(pic, port, c);
#if PIC_ENABLE_FLOAT
    if (strcaseeq(pic_symbol_name(pic, pic_sym_ptr(sym)), "-inf.0")) {
      return pic_float_value(-INFINITY);
    }
    if (strcaseeq(pic_symbol_name(pic, pic_sym_ptr(sym)), "-nan.0")) {
      return pic_float_value(-NAN);
    }
#endif
    return sym;
  }
}

static pic_value
read_plus(pic_state *pic, struct pic_port *port, int c)
{
  pic_value sym;

  if (isdigit(peek(port))) {
    return read_unsigned(pic, port, next(port));
  }
  else {
    sym = read_symbol(pic, port, c);
#if PIC_ENABLE_FLOAT
    if (strcaseeq(pic_symbol_name(pic, pic_sym_ptr(sym)), "+inf.0")) {
      return pic_float_value(INFINITY);
    }
    if (strcaseeq(pic_symbol_name(pic, pic_sym_ptr(sym)), "+nan.0")) {
      return pic_float_value(NAN);
    }
#endif
    return sym;
  }
}

static pic_value
read_true(pic_state *pic, struct pic_port *port, int c)
{
  PIC_UNUSED(pic);

  if ((c = peek(port)) == 'r') {
    if (! expect(port, "rue")) {
      read_error(pic, "unexpected character while reading #true");
    }
  } else if (! isdelim(c)) {
    read_error(pic, "non-delimiter character given after #t");
  }

  return pic_true_value();
}

static pic_value
read_false(pic_state *pic, struct pic_port *port, int c)
{
  PIC_UNUSED(pic);

  if ((c = peek(port)) == 'a') {
    if (! expect(port, "alse")) {
      read_error(pic, "unexpected character while reading #false");
    }
  } else if (! isdelim(c)) {
    read_error(pic, "non-delimiter character given after #f");
  }

  return pic_false_value();
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

  return pic_char_value((char)c);

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
    buf[cnt++] = (char)c;
    if (cnt >= size) {
      buf = pic_realloc(pic, buf, size *= 2);
    }
  }
  buf[cnt] = '\0';

  str = pic_make_str(pic, buf, cnt);
  pic_free(pic, buf);
  return pic_obj_value(str);
}

static pic_value
read_pipe(pic_state *pic, struct pic_port *port, int c)
{
  char *buf;
  size_t size, cnt;
  pic_sym *sym;
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
        while ((HEX_BUF[i++] = (char)next(port)) != ';') {
          if (i >= sizeof HEX_BUF)
            read_error(pic, "expected ';'");
        }
        c = (char)strtol(HEX_BUF, NULL, 16);
        break;
      }
    }
    buf[cnt++] = (char)c;
    if (cnt >= size) {
      buf = pic_realloc(pic, buf, size *= 2);
    }
  }
  buf[cnt] = '\0';

  sym = pic_intern_cstr(pic, buf);
  pic_free(pic, buf);

  return pic_obj_value(sym);
}

static pic_value
read_blob(pic_state *pic, struct pic_port *port, int c)
{
  int nbits, n;
  size_t len, i;
  unsigned char *dat;
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
    n = read_uinteger(pic, port, c);
    if (n < 0 || (1 << nbits) <= n) {
      read_error(pic, "invalid element in bytevector literal");
    }
    len += 1;
    dat = pic_realloc(pic, dat, len);
    dat[len - 1] = (unsigned char)n;
    c = next(port);
  }

  blob = pic_make_blob(pic, len);
  for (i = 0; i < len; ++i) {
    blob->data[i] = dat[i];
  }

  pic_free(pic, dat);
  return pic_obj_value(blob);
}

static pic_value
read_pair(pic_state *pic, struct pic_port *port, int c)
{
  static const int tCLOSE = ')';
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

    cdr = read_pair(pic, port, '(');
    return pic_cons(pic, car, cdr);
  }
}

static pic_value
read_vector(pic_state *pic, struct pic_port *port, int c)
{
  pic_value list;

  list = read(pic, port, c);

  return pic_obj_value(pic_make_vec_from_list(pic, list));
}

static pic_value
read_label_set(pic_state *pic, struct pic_port *port, int i)
{
  pic_value val;
  int c;

  switch ((c = skip(port, ' '))) {
  case '(':
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

        val = pic_obj_value(pic_make_vec(pic, 0));

        xh_put_int(&pic->reader->labels, i, &val);

        tmp = pic_vec_ptr(read(pic, port, c));
        PIC_SWAP(pic_value *, tmp->data, pic_vec_ptr(val)->data);
        PIC_SWAP(size_t, tmp->len, pic_vec_ptr(val)->len);

        return val;
      }

      PIC_FALLTHROUGH;
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

  PIC_UNUSED(port);

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
    i = i * 10 + c - '0';
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
  PIC_UNUSED(port);
  PIC_UNUSED(c);

  read_error(pic, "unmatched parenthesis");
}

static pic_value
read_dispatch(pic_state *pic, struct pic_port *port, int c)
{
  c = next(port);

  if (c == EOF) {
    read_error(pic, "unexpected EOF");
  }

  if (pic->reader->dispatch[c] == NULL) {
    read_error(pic, "invalid character at the seeker head");
  }

  return pic->reader->dispatch[c](pic, port, c);
}

static pic_value
read_nullable(pic_state *pic, struct pic_port *port, int c)
{
  c = skip(port, c);

  if (c == EOF) {
    read_error(pic, "unexpected EOF");
  }

  if (pic->reader->table[c] == NULL) {
    read_error(pic, "invalid character at the seeker head");
  }

  return pic->reader->table[c](pic, port, c);
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

static void
reader_table_init(struct pic_reader *reader)
{
  int c;

  reader->table[0] = NULL;

  /* default reader */
  for (c = 1; c < 256; ++c) {
    reader->table[c] = read_symbol;
  }

  reader->table[')'] = read_unmatch;
  reader->table[';'] = read_comment;
  reader->table['\''] = read_quote;
  reader->table['`'] = read_quasiquote;
  reader->table[','] = read_unquote;
  reader->table['"'] = read_string;
  reader->table['|'] = read_pipe;
  reader->table['+'] = read_plus;
  reader->table['-'] = read_minus;
  reader->table['('] = read_pair;
  reader->table['#'] = read_dispatch;

  /* read number */
  for (c = '0'; c <= '9'; ++c) {
    reader->table[c] = read_number;
  }

  reader->dispatch['!'] = read_directive;
  reader->dispatch['|'] = read_block_comment;
  reader->dispatch[';'] = read_datum_comment;
  reader->dispatch['t'] = read_true;
  reader->dispatch['f'] = read_false;
  reader->dispatch['\\'] = read_char;
  reader->dispatch['('] = read_vector;
  reader->dispatch['u'] = read_blob;
  reader->dispatch['.'] = read_eval;

  /* read labels */
  for (c = '0'; c <= '9'; ++c) {
    reader->dispatch[c] = read_label;
  }
}

struct pic_reader *
pic_reader_open(pic_state *pic)
{
  struct pic_reader *reader;
  int c;

  reader = pic_alloc(pic, sizeof(struct pic_reader));
  reader->typecase = PIC_CASE_DEFAULT;
  xh_init_int(&reader->labels, sizeof(pic_value));

  for (c = 0; c < 256; ++c) {
    reader->table[c] = NULL;
  }

  for (c = 0; c < 256; ++c) {
    reader->dispatch[c] = NULL;
  }

  reader_table_init(reader);

  return reader;
}

void
pic_reader_close(pic_state *pic, struct pic_reader *reader)
{
  xh_destroy(&reader->labels);
  pic_free(pic, reader);
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
pic_read_read(pic_state *pic)
{
  struct pic_port *port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  return pic_read(pic, port);
}

void
pic_init_read(pic_state *pic)
{
  pic_defun(pic, "read", pic_read_read);
}
