/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

KHASH_DEFINE(read, int, pic_value, kh_int_hash_func, kh_int_hash_equal)

static pic_value read(pic_state *pic, struct pic_port *port, int c);
static pic_value read_nullable(pic_state *pic, struct pic_port *port, int c);

PIC_NORETURN static void
read_error(pic_state *pic, const char *msg)
{
  struct pic_error *e;

  e = pic_make_error(pic, pic_intern_cstr(pic, "read"), msg, pic_nil_value());

  pic_raise(pic, pic_obj_value(e));
}

static int
skip(pic_state *pic, struct pic_port *port, int c)
{
  while (isspace(c)) {
    c = xfgetc(pic, port->file);
  }
  return c;
}

static int
next(pic_state *pic, struct pic_port *port)
{
  return xfgetc(pic, port->file);
}

static int
peek(pic_state *pic, struct pic_port *port)
{
  int c;

  xungetc((c = xfgetc(pic, port->file)), port->file);

  return c;
}

static bool
expect(pic_state *pic, struct pic_port *port, const char *str)
{
  int c;

  while ((c = (int)*str++) != 0) {
    if (c != peek(pic, port))
      return false;
    next(pic, port);
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
  if (pic->reader.typecase == PIC_CASE_FOLD) {
    c = tolower(c);
  }
  return c;
}

static pic_value
read_comment(pic_state PIC_UNUSED(*pic), struct pic_port *port, int c)
{
  do {
    c = next(pic, port);
  } while (! (c == EOF || c == '\n'));

  return pic_invalid_value();
}

static pic_value
read_block_comment(pic_state PIC_UNUSED(*pic), struct pic_port *port, int PIC_UNUSED(c))
{
  int x, y;
  int i = 1;

  y = next(pic, port);

  while (y != EOF && i > 0) {
    x = y;
    y = next(pic, port);
    if (x == '|' && y == '#') {
      i--;
    }
    if (x == '#' && y == '|') {
      i++;
    }
  }

  return pic_invalid_value();
}

static pic_value
read_datum_comment(pic_state *pic, struct pic_port *port, int PIC_UNUSED(c))
{
  read(pic, port, next(pic, port));

  return pic_invalid_value();
}

static pic_value
read_directive(pic_state *pic, struct pic_port *port, int c)
{
  switch (peek(pic, port)) {
  case 'n':
    if (expect(pic, port, "no-fold-case")) {
      pic->reader.typecase = PIC_CASE_DEFAULT;
      return pic_invalid_value();
    }
    break;
  case 'f':
    if (expect(pic, port, "fold-case")) {
      pic->reader.typecase = PIC_CASE_FOLD;
      return pic_invalid_value();
    }
    break;
  }

  return read_comment(pic, port, c);
}

static pic_value
read_quote(pic_state *pic, struct pic_port *port, int PIC_UNUSED(c))
{
  return pic_list2(pic, pic_obj_value(pic->sQUOTE), read(pic, port, next(pic, port)));
}

static pic_value
read_quasiquote(pic_state *pic, struct pic_port *port, int PIC_UNUSED(c))
{
  return pic_list2(pic, pic_obj_value(pic->sQUASIQUOTE), read(pic, port, next(pic, port)));
}

static pic_value
read_unquote(pic_state *pic, struct pic_port *port, int PIC_UNUSED(c))
{
  pic_sym *tag = pic->sUNQUOTE;

  if (peek(pic, port) == '@') {
    tag = pic->sUNQUOTE_SPLICING;
    next(pic, port);
  }
  return pic_list2(pic, pic_obj_value(tag), read(pic, port, next(pic, port)));
}

static pic_value
read_syntax_quote(pic_state *pic, struct pic_port *port, int PIC_UNUSED(c))
{
  return pic_list2(pic, pic_obj_value(pic->sSYNTAX_QUOTE), read(pic, port, next(pic, port)));
}

static pic_value
read_syntax_quasiquote(pic_state *pic, struct pic_port *port, int PIC_UNUSED(c))
{
  return pic_list2(pic, pic_obj_value(pic->sSYNTAX_QUASIQUOTE), read(pic, port, next(pic, port)));
}

static pic_value
read_syntax_unquote(pic_state *pic, struct pic_port *port, int PIC_UNUSED(c))
{
  pic_sym *tag = pic->sSYNTAX_UNQUOTE;

  if (peek(pic, port) == '@') {
    tag = pic->sSYNTAX_UNQUOTE_SPLICING;
    next(pic, port);
  }
  return pic_list2(pic, pic_obj_value(tag), read(pic, port, next(pic, port)));
}

static pic_value
read_symbol(pic_state *pic, struct pic_port *port, int c)
{
  size_t len;
  char *buf;
  pic_sym *sym;

  len = 1;
  buf = pic_malloc(pic, len + 1);
  buf[0] = case_fold(pic, c);
  buf[1] = 0;

  while (! isdelim(peek(pic, port))) {
    c = next(pic, port);
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
  while (isdigit(c = peek(pic, port))) {
    u = u * 10 + next(pic, port) - '0';
  }

  return u;
}

static int
read_suffix(pic_state *pic, struct pic_port *port)
{
  int c, s = 1;

  c = peek(pic, port);

  if (c != 'e' && c != 'E') {
    return 0;
  }

  next(pic, port);

  switch ((c = next(pic, port))) {
  case '-':
    s = -1;
  case '+':
    c = next(pic, port);
  default:
    return s * read_uinteger(pic, port, c);
  }
}

static pic_value
read_unsigned(pic_state *pic, struct pic_port *port, int c)
{
  unsigned u;
  int exp, s, i, e;

  u = read_uinteger(pic, port, c);

  switch (peek(pic, port)) {
#if PIC_ENABLE_FLOAT
# if PIC_ENABLE_LIBC
  case '.': {
    char buf[256];
    i = sprintf(buf, "%d", u);
    buf[i++] = next(pic, port);
    while (isdigit(c = peek(pic, port))) {
      buf[i++] = next(pic, port);
    }
    sprintf(buf + i, "e%d", read_suffix(pic, port));
    return pic_float_value(atof(buf));
  }
# else
  case '.': {
    double f, g;
    next(pic, port);
    g = 0, e = 0;
    while (isdigit(c = peek(pic, port))) {
      g = g * 10 + (next(pic, port) - '0');
      e++;
    }
    f = u + g * pow(10, -e);

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
  }
# endif
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

  if (isdigit(peek(pic, port))) {
    return negate(read_unsigned(pic, port, next(pic, port)));
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

  if (isdigit(peek(pic, port))) {
    return read_unsigned(pic, port, next(pic, port));
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
  if ((c = peek(pic, port)) == 'r') {
    if (! expect(pic, port, "rue")) {
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
  if ((c = peek(pic, port)) == 'a') {
    if (! expect(pic, port, "alse")) {
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
  c = next(pic, port);

  if (! isdelim(peek(pic, port))) {
    switch (c) {
    default: read_error(pic, "unexpected character after char literal");
    case 'a': c = '\a'; if (! expect(pic, port, "lerm")) goto fail; break;
    case 'b': c = '\b'; if (! expect(pic, port, "ackspace")) goto fail; break;
    case 'd': c = 0x7F; if (! expect(pic, port, "elete")) goto fail; break;
    case 'e': c = 0x1B; if (! expect(pic, port, "scape")) goto fail; break;
    case 'n':
      if ((c = peek(pic, port)) == 'e') {
        c = '\n';
        if (! expect(pic, port, "ewline"))
          goto fail;
      } else {
        c = '\0';
        if (! expect(pic, port, "ull"))
          goto fail;
      }
      break;
    case 'r': c = '\r'; if (! expect(pic, port, "eturn")) goto fail; break;
    case 's': c = ' '; if (! expect(pic, port, "pace")) goto fail; break;
    case 't': c = '\t'; if (! expect(pic, port, "ab")) goto fail; break;
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
  buf = pic_malloc(pic, size);
  cnt = 0;

  /* TODO: intraline whitespaces */

  while ((c = next(pic, port)) != '"') {
    if (c == '\\') {
      switch (c = next(pic, port)) {
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
  buf = pic_malloc(pic, size);
  cnt = 0;
  while ((c = next(pic, port)) != '|') {
    if (c == '\\') {
      switch ((c = next(pic, port))) {
      case 'a': c = '\a'; break;
      case 'b': c = '\b'; break;
      case 't': c = '\t'; break;
      case 'n': c = '\n'; break;
      case 'r': c = '\r'; break;
      case 'x':
        i = 0;
        while ((HEX_BUF[i++] = (char)next(pic, port)) != ';') {
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

  while (isdigit(c = next(pic, port))) {
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
  c = next(pic, port);
  while ((c = skip(pic, port, c)) != ')') {
    n = read_uinteger(pic, port, c);
    if (n < 0 || (1 << nbits) <= n) {
      read_error(pic, "invalid element in bytevector literal");
    }
    len += 1;
    dat = pic_realloc(pic, dat, len);
    dat[len - 1] = (unsigned char)n;
    c = next(pic, port);
  }

  blob = pic_make_blob(pic, len);
  for (i = 0; i < len; ++i) {
    blob->data[i] = dat[i];
  }

  pic_free(pic, dat);
  return pic_obj_value(blob);
}

static pic_value
read_undef_or_blob(pic_state *pic, struct pic_port *port, int c)
{
  if ((c = peek(pic, port)) == 'n') {
    if (! expect(pic, port, "ndefined")) {
      read_error(pic, "unexpected character while reading #undefined");
    }
    return pic_undef_value();
  }
  if (! isdigit(c)) {
    read_error(pic, "expect #undefined or #u8(...), but illegal character given");
  }
  return read_blob(pic, port, 'u');
}

static pic_value
read_pair(pic_state *pic, struct pic_port *port, int c)
{
  static const int tCLOSE = ')';
  pic_value car, cdr;

 retry:

  c = skip(pic, port, ' ');

  if (c == tCLOSE) {
    return pic_nil_value();
  }
  if (c == '.' && isdelim(peek(pic, port))) {
    cdr = read(pic, port, next(pic, port));

  closing:
    if ((c = skip(pic, port, ' ')) != tCLOSE) {
      if (pic_invalid_p(read_nullable(pic, port, c))) {
        goto closing;
      }
      read_error(pic, "unmatched parenthesis");
    }
    return cdr;
  }
  else {
    car = read_nullable(pic, port, c);

    if (pic_invalid_p(car)) {
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
  khash_t(read) *h = &pic->reader.labels;
  pic_value val;
  int c, ret;
  khiter_t it;

  it = kh_put(read, h, i, &ret);

  switch ((c = skip(pic, port, ' '))) {
  case '(':
    {
      pic_value tmp;

      kh_val(h, it) = val = pic_cons(pic, pic_undef_value(), pic_undef_value());

      tmp = read(pic, port, c);
      pic_pair_ptr(val)->car = pic_car(pic, tmp);
      pic_pair_ptr(val)->cdr = pic_cdr(pic, tmp);

      return val;
    }
  case '#':
    {
      bool vect;

      if (peek(pic, port) == '(') {
        vect = true;
      } else {
        vect = false;
      }

      if (vect) {
        pic_vec *tmp;

        kh_val(h, it) = val = pic_obj_value(pic_make_vec(pic, 0));

        tmp = pic_vec_ptr(read(pic, port, c));
        PIC_SWAP(pic_value *, tmp->data, pic_vec_ptr(val)->data);
        PIC_SWAP(size_t, tmp->len, pic_vec_ptr(val)->len);

        return val;
      }

      PIC_FALLTHROUGH;
    }
  default:
    {
      kh_val(h, it) = val = read(pic, port, c);

      return val;
    }
  }
}

static pic_value
read_label_ref(pic_state *pic, struct pic_port PIC_UNUSED(*port), int i)
{
  khash_t(read) *h = &pic->reader.labels;
  khiter_t it;

  it = kh_get(read, h, i);
  if (it == kh_end(h)) {
    read_error(pic, "label of given index not defined");
  }
  return kh_val(h, it);
}

static pic_value
read_label(pic_state *pic, struct pic_port *port, int c)
{
  int i;

  i = 0;
  do {
    i = i * 10 + c - '0';
  } while (isdigit(c = next(pic, port)));

  if (c == '=') {
    return read_label_set(pic, port, i);
  }
  if (c == '#') {
    return read_label_ref(pic, port, i);
  }
  read_error(pic, "broken label expression");
}

static pic_value
read_unmatch(pic_state *pic, struct pic_port PIC_UNUSED(*port), int PIC_UNUSED(c))
{
  read_error(pic, "unmatched parenthesis");
}

static pic_value
read_dispatch(pic_state *pic, struct pic_port *port, int c)
{
  c = next(pic, port);

  if (c == EOF) {
    read_error(pic, "unexpected EOF");
  }

  if (pic->reader.dispatch[c] == NULL) {
    read_error(pic, "invalid character at the seeker head");
  }

  return pic->reader.dispatch[c](pic, port, c);
}

static pic_value
read_nullable(pic_state *pic, struct pic_port *port, int c)
{
  c = skip(pic, port, c);

  if (c == EOF) {
    read_error(pic, "unexpected EOF");
  }

  if (pic->reader.table[c] == NULL) {
    read_error(pic, "invalid character at the seeker head");
  }

  return pic->reader.table[c](pic, port, c);
}

static pic_value
read(pic_state *pic, struct pic_port *port, int c)
{
  pic_value val;

 retry:
  val = read_nullable(pic, port, c);

  if (pic_invalid_p(val)) {
    c = next(pic, port);
    goto retry;
  }

  return val;
}

static void
reader_table_init(pic_reader *reader)
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
  reader->dispatch['\''] = read_syntax_quote;
  reader->dispatch['`'] = read_syntax_quasiquote;
  reader->dispatch[','] = read_syntax_unquote;
  reader->dispatch['\\'] = read_char;
  reader->dispatch['('] = read_vector;
  reader->dispatch['u'] = read_undef_or_blob;

  /* read labels */
  for (c = '0'; c <= '9'; ++c) {
    reader->dispatch[c] = read_label;
  }
}

void
pic_reader_init(pic_state *pic)
{
  int c;

  pic->reader.typecase = PIC_CASE_DEFAULT;
  kh_init(read, &pic->reader.labels);

  for (c = 0; c < 256; ++c) {
    pic->reader.table[c] = NULL;
  }

  for (c = 0; c < 256; ++c) {
    pic->reader.dispatch[c] = NULL;
  }

  reader_table_init(&pic->reader);
}

void
pic_reader_destroy(pic_state *pic)
{
  kh_destroy(read, &pic->reader.labels);
}

pic_value
pic_read(pic_state *pic, struct pic_port *port)
{
  size_t ai = pic_gc_arena_preserve(pic);
  pic_value val;
  int c;

  while ((c = skip(pic, port, next(pic, port))) != EOF) {
    val = read_nullable(pic, port, c);

    if (! pic_invalid_p(val)) {
      break;
    }
    pic_gc_arena_restore(pic, ai);
  }
  if (c == EOF) {
    return pic_eof_object();
  }

  pic_gc_arena_restore(pic, ai);
  return pic_gc_protect(pic, val);
}

pic_value
pic_read_cstr(pic_state *pic, const char *str)
{
  struct pic_port *port = pic_open_input_string(pic, str);
  pic_value form;

  pic_try {
    form = pic_read(pic, port);
  }
  pic_catch {
    pic_close_port(pic, port);
    pic_raise(pic, pic->err);
  }

  pic_close_port(pic, port);

  return form;
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
