/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"
#include "picrin/extra.h"
#include "picrin/object.h"
#include "picrin/state.h"

#undef EOF
#define EOF (-1)

KHASH_DEFINE(read, int, pic_value, kh_int_hash_func, kh_int_hash_equal)

static pic_value read(pic_state *pic, xFILE *file, int c);
static pic_value read_nullable(pic_state *pic, xFILE *file, int c);

PIC_NORETURN static void
read_error(pic_state *pic, const char *msg, pic_value irritants)
{
  pic_error(pic, "read", msg, irritants);
}

static int
skip(pic_state *pic, xFILE *file, int c)
{
  while (isspace(c)) {
    c = xfgetc(pic, file);
  }
  return c;
}

static int
next(pic_state *pic, xFILE *file)
{
  return xfgetc(pic, file);
}

static int
peek(pic_state *pic, xFILE *file)
{
  int c;

  xungetc(pic, (c = xfgetc(pic, file)), file);

  return c;
}

static bool
expect(pic_state *pic, xFILE *file, const char *str)
{
  int c;

  while ((c = (int)*str++) != 0) {
    if (c != peek(pic, file))
      return false;
    next(pic, file);
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

static int
case_fold(pic_state *pic, int c)
{
  if (pic->reader.typecase == PIC_CASE_FOLD) {
    c = tolower(c);
  }
  return c;
}

static pic_value
read_comment(pic_state PIC_UNUSED(*pic), xFILE *file, int c)
{
  do {
    c = next(pic, file);
  } while (! (c == EOF || c == '\n'));

  return pic_invalid_value(pic);
}

static pic_value
read_block_comment(pic_state PIC_UNUSED(*pic), xFILE *file, int PIC_UNUSED(c))
{
  int x, y;
  int i = 1;

  y = next(pic, file);

  while (y != EOF && i > 0) {
    x = y;
    y = next(pic, file);
    if (x == '|' && y == '#') {
      i--;
    }
    if (x == '#' && y == '|') {
      i++;
    }
  }

  return pic_invalid_value(pic);
}

static pic_value
read_datum_comment(pic_state *pic, xFILE *file, int PIC_UNUSED(c))
{
  read(pic, file, next(pic, file));

  return pic_invalid_value(pic);
}

static pic_value
read_directive(pic_state *pic, xFILE *file, int c)
{
  switch (peek(pic, file)) {
  case 'n':
    if (expect(pic, file, "no-fold-case")) {
      pic->reader.typecase = PIC_CASE_DEFAULT;
      return pic_invalid_value(pic);
    }
    break;
  case 'f':
    if (expect(pic, file, "fold-case")) {
      pic->reader.typecase = PIC_CASE_FOLD;
      return pic_invalid_value(pic);
    }
    break;
  }

  return read_comment(pic, file, c);
}

static pic_value
read_quote(pic_state *pic, xFILE *file, int PIC_UNUSED(c))
{
  return pic_list(pic, 2, pic->sQUOTE, read(pic, file, next(pic, file)));
}

static pic_value
read_quasiquote(pic_state *pic, xFILE *file, int PIC_UNUSED(c))
{
  return pic_list(pic, 2, pic->sQUASIQUOTE, read(pic, file, next(pic, file)));
}

static pic_value
read_unquote(pic_state *pic, xFILE *file, int PIC_UNUSED(c))
{
  pic_value tag = pic->sUNQUOTE;

  if (peek(pic, file) == '@') {
    tag = pic->sUNQUOTE_SPLICING;
    next(pic, file);
  }
  return pic_list(pic, 2, tag, read(pic, file, next(pic, file)));
}

static pic_value
read_syntax_quote(pic_state *pic, xFILE *file, int PIC_UNUSED(c))
{
  return pic_list(pic, 2, pic->sSYNTAX_QUOTE, read(pic, file, next(pic, file)));
}

static pic_value
read_syntax_quasiquote(pic_state *pic, xFILE *file, int PIC_UNUSED(c))
{
  return pic_list(pic, 2, pic->sSYNTAX_QUASIQUOTE, read(pic, file, next(pic, file)));
}

static pic_value
read_syntax_unquote(pic_state *pic, xFILE *file, int PIC_UNUSED(c))
{
  pic_value tag = pic->sSYNTAX_UNQUOTE;

  if (peek(pic, file) == '@') {
    tag = pic->sSYNTAX_UNQUOTE_SPLICING;
    next(pic, file);
  }
  return pic_list(pic, 2, tag, read(pic, file, next(pic, file)));
}

static pic_value
read_symbol(pic_state *pic, xFILE *file, int c)
{
  int len;
  char *buf;
  pic_value sym;

  len = 1;
  buf = pic_malloc(pic, len + 1);
  buf[0] = case_fold(pic, c);
  buf[1] = 0;

  while (! isdelim(peek(pic, file))) {
    c = next(pic, file);
    len += 1;
    buf = pic_realloc(pic, buf, len + 1);
    buf[len - 1] = case_fold(pic, c);
    buf[len] = 0;
  }

  sym = pic_intern_cstr(pic, buf);
  pic_free(pic, buf);

  return sym;
}

static unsigned
read_uinteger(pic_state *pic, xFILE *file, int c)
{
  unsigned u = 0;

  if (! isdigit(c)) {
    read_error(pic, "expected one or more digits", pic_list(pic, 1, pic_char_value(pic, c)));
  }

  u = c - '0';
  while (isdigit(c = peek(pic, file))) {
    u = u * 10 + next(pic, file) - '0';
  }

  return u;
}

static pic_value
read_unsigned(pic_state *pic, xFILE *file, int c)
{
#define ATOF_BUF_SIZE (64)
  char buf[ATOF_BUF_SIZE];
  double flt;
  int idx = 0;  /* index into buffer */
  int dpe = 0;  /* the number of '.' or 'e' characters seen */

  if (! isdigit(c)) {
    read_error(pic, "expected one or more digits", pic_list(pic, 1, pic_char_value(pic, c)));
  }
  buf[idx++] = (char )c;
  while (isdigit(c = peek(pic, file)) && idx < ATOF_BUF_SIZE) {
    buf[idx++] = (char )next(pic, file);
  }
  if ('.' == peek(pic, file) && idx < ATOF_BUF_SIZE) {
    dpe++;
    buf[idx++] = (char )next(pic, file);
    while (isdigit(c = peek(pic, file)) && idx < ATOF_BUF_SIZE) {
      buf[idx++] = (char )next(pic, file);
    }
  }
  c = peek(pic, file);

  if ((c == 'e' || c == 'E') && idx < (ATOF_BUF_SIZE - 2)) {
    dpe++;
    buf[idx++] = (char )next(pic, file);
    switch ((c = peek(pic, file))) {
    case '-':
    case '+':
      buf[idx++] = (char )next(pic, file);
      break;
    default:
      break;
    }
    if (! isdigit(peek(pic, file))) {
      read_error(pic, "expected one or more digits", pic_list(pic, 1, pic_char_value(pic, c)));
    }
    while (isdigit(c = peek(pic, file)) && idx < ATOF_BUF_SIZE) {
      buf[idx++] = (char )next(pic, file);
    }
  }
  if (idx >= ATOF_BUF_SIZE)
    read_error(pic, "number too large", pic_list(pic, 1, pic_str_value(pic, (const char *)buf, ATOF_BUF_SIZE)));

  if (! isdelim(c))
    read_error(pic, "non-delimiter character given after number", pic_list(pic, 1, pic_char_value(pic, c)));

  buf[idx] = 0;
  flt = PIC_CSTRING_TO_DOUBLE(buf);

  if (dpe == 0 && INT_MIN <= flt && flt <= INT_MAX)
    return pic_int_value(pic, flt);
  return pic_float_value(pic, flt);
}

static pic_value
read_number(pic_state *pic, xFILE *file, int c)
{
  return read_unsigned(pic, file, c);
}

static pic_value
negate(pic_state *pic, pic_value n)
{
  if (pic_int_p(pic, n) && (INT_MIN != pic_int(pic, n))) {
    return pic_int_value(pic, -pic_int(pic, n));
  } else {
    return pic_float_value(pic, -pic_float(pic, n));
  }
}

static pic_value
read_minus(pic_state *pic, xFILE *file, int c)
{
  pic_value sym;

  if (isdigit(peek(pic, file))) {
    return negate(pic, read_unsigned(pic, file, next(pic, file)));
  }
  else {
    sym = read_symbol(pic, file, c);
    if (strcaseeq(pic_str(pic, pic_sym_name(pic, sym)), "-inf.0")) {
      return pic_float_value(pic, -(1.0 / 0.0));
    }
    if (strcaseeq(pic_str(pic, pic_sym_name(pic, sym)), "-nan.0")) {
      return pic_float_value(pic, -(0.0 / 0.0));
    }
    return sym;
  }
}

static pic_value
read_plus(pic_state *pic, xFILE *file, int c)
{
  pic_value sym;

  if (isdigit(peek(pic, file))) {
    return read_unsigned(pic, file, next(pic, file));
  }
  else {
    sym = read_symbol(pic, file, c);
    if (strcaseeq(pic_str(pic, pic_sym_name(pic, sym)), "+inf.0")) {
      return pic_float_value(pic, 1.0 / 0.0);
    }
    if (strcaseeq(pic_str(pic, pic_sym_name(pic, sym)), "+nan.0")) {
      return pic_float_value(pic, 0.0 / 0.0);
    }
    return sym;
  }
}

static pic_value
read_true(pic_state *pic, xFILE *file, int c)
{
  if ((c = peek(pic, file)) == 'r') {
    if (! expect(pic, file, "rue")) {
      read_error(pic, "unexpected character while reading #true", pic_nil_value(pic));
    }
  } else if (! isdelim(c)) {
    read_error(pic, "non-delimiter character given after #t", pic_list(pic, 1, pic_char_value(pic, c)));
  }

  return pic_true_value(pic);
}

static pic_value
read_false(pic_state *pic, xFILE *file, int c)
{
  if ((c = peek(pic, file)) == 'a') {
    if (! expect(pic, file, "alse")) {
      read_error(pic, "unexpected character while reading #false", pic_nil_value(pic));
    }
  } else if (! isdelim(c)) {
    read_error(pic, "non-delimiter character given after #f", pic_list(pic, 1, pic_char_value(pic, c)));
  }

  return pic_false_value(pic);
}

static pic_value
read_char(pic_state *pic, xFILE *file, int c)
{
  c = next(pic, file);

  if (! isdelim(peek(pic, file))) {
    switch (c) {
    default: read_error(pic, "unexpected character after char literal", pic_list(pic, 1, pic_char_value(pic, c)));
    case 'a': c = '\a'; if (! expect(pic, file, "larm")) goto fail; break;
    case 'b': c = '\b'; if (! expect(pic, file, "ackspace")) goto fail; break;
    case 'd': c = 0x7F; if (! expect(pic, file, "elete")) goto fail; break;
    case 'e': c = 0x1B; if (! expect(pic, file, "scape")) goto fail; break;
    case 'n':
      if ((c = peek(pic, file)) == 'e') {
        c = '\n';
        if (! expect(pic, file, "ewline"))
          goto fail;
      } else {
        c = '\0';
        if (! expect(pic, file, "ull"))
          goto fail;
      }
      break;
    case 'r': c = '\r'; if (! expect(pic, file, "eturn")) goto fail; break;
    case 's': c = ' '; if (! expect(pic, file, "pace")) goto fail; break;
    case 't': c = '\t'; if (! expect(pic, file, "ab")) goto fail; break;
    }
  }

  return pic_char_value(pic, (char)c);

 fail:
  read_error(pic, "unexpected character while reading character literal", pic_list(pic, 1, pic_char_value(pic, c)));
}

static pic_value
read_string(pic_state *pic, xFILE *file, int c)
{
  char *buf;
  int size, cnt;
  pic_value str;

  size = 256;
  buf = pic_malloc(pic, size);
  cnt = 0;

  /* TODO: intraline whitespaces */

  while ((c = next(pic, file)) != '"') {
    if (c == '\\') {
      switch (c = next(pic, file)) {
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

  str = pic_str_value(pic, buf, cnt);
  pic_free(pic, buf);
  return str;
}

static pic_value
read_pipe(pic_state *pic, xFILE *file, int c)
{
  char *buf;
  int size, cnt;
  pic_value sym;
  /* Currently supports only ascii chars */
  char HEX_BUF[3];
  size_t i = 0;

  size = 256;
  buf = pic_malloc(pic, size);
  cnt = 0;
  while ((c = next(pic, file)) != '|') {
    if (c == '\\') {
      switch ((c = next(pic, file))) {
      case 'a': c = '\a'; break;
      case 'b': c = '\b'; break;
      case 't': c = '\t'; break;
      case 'n': c = '\n'; break;
      case 'r': c = '\r'; break;
      case 'x':
        i = 0;
        while ((HEX_BUF[i++] = (char)next(pic, file)) != ';') {
          if (i >= sizeof HEX_BUF)
            read_error(pic, "expected ';'", pic_list(pic, 1, pic_char_value(pic, HEX_BUF[sizeof(HEX_BUF) - 1])));
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

  return sym;
}

static pic_value
read_blob(pic_state *pic, xFILE *file, int c)
{
  int nbits, n;
  int len;
  unsigned char *dat;
  pic_value blob;

  nbits = 0;

  while (isdigit(c = next(pic, file))) {
    nbits = 10 * nbits + c - '0';
  }

  if (nbits != 8) {
    read_error(pic, "unsupported bytevector bit width", pic_list(pic, 1, pic_int_value(pic, nbits)));
  }

  if (c != '(') {
    read_error(pic, "expected '(' character", pic_list(pic, 1, pic_char_value(pic, c)));
  }

  len = 0;
  dat = NULL;
  c = next(pic, file);
  while ((c = skip(pic, file, c)) != ')') {
    n = read_uinteger(pic, file, c);
    if (n < 0 || (1 << nbits) <= n) {
      read_error(pic, "invalid element in bytevector literal", pic_list(pic, 1, pic_int_value(pic, n)));
    }
    len += 1;
    dat = pic_realloc(pic, dat, len);
    dat[len - 1] = (unsigned char)n;
    c = next(pic, file);
  }

  blob = pic_blob_value(pic, dat, len);

  pic_free(pic, dat);
  return blob;
}

static pic_value
read_undef_or_blob(pic_state *pic, xFILE *file, int c)
{
  if ((c = peek(pic, file)) == 'n') {
    if (! expect(pic, file, "ndefined")) {
      read_error(pic, "unexpected character while reading #undefined", pic_nil_value(pic));
    }
    return pic_undef_value(pic);
  }
  if (! isdigit(c)) {
    read_error(pic, "expect #undefined or #u8(...), but illegal character given", pic_list(pic, 1, pic_char_value(pic, c)));
  }
  return read_blob(pic, file, 'u');
}

static pic_value
read_pair(pic_state *pic, xFILE *file, int c)
{
  static const int tCLOSE = ')';
  pic_value car, cdr;

 retry:

  c = skip(pic, file, ' ');

  if (c == tCLOSE) {
    return pic_nil_value(pic);
  }
  if (c == '.' && isdelim(peek(pic, file))) {
    cdr = read(pic, file, next(pic, file));

  closing:
    if ((c = skip(pic, file, ' ')) != tCLOSE) {
      if (pic_invalid_p(pic, read_nullable(pic, file, c))) {
        goto closing;
      }
      read_error(pic, "unmatched parenthesis", pic_nil_value(pic));
    }
    return cdr;
  }
  else {
    car = read_nullable(pic, file, c);

    if (pic_invalid_p(pic, car)) {
      goto retry;
    }

    cdr = read_pair(pic, file, '(');
    return pic_cons(pic, car, cdr);
  }
}

static pic_value
read_vector(pic_state *pic, xFILE *file, int c)
{
  pic_value list, it, elem, vec;
  int i = 0;

  list = read(pic, file, c);

  vec = pic_make_vec(pic, pic_length(pic, list), NULL);

  pic_for_each (elem, list, it) {
    pic_vec_set(pic, vec, i++, elem);
  }

  return vec;
}

static pic_value
read_label_set(pic_state *pic, xFILE *file, int i)
{
  khash_t(read) *h = &pic->reader.labels;
  pic_value val;
  int c, ret;
  khiter_t it;

  it = kh_put(read, h, i, &ret);

  switch ((c = skip(pic, file, ' '))) {
  case '(':
    {
      pic_value tmp;

      kh_val(h, it) = val = pic_cons(pic, pic_undef_value(pic), pic_undef_value(pic));

      tmp = read(pic, file, c);
      pic_pair_ptr(pic, val)->car = pic_car(pic, tmp);
      pic_pair_ptr(pic, val)->cdr = pic_cdr(pic, tmp);

      return val;
    }
  case '#':
    {
      bool vect;

      if (peek(pic, file) == '(') {
        vect = true;
      } else {
        vect = false;
      }

      if (vect) {
        pic_value tmp;

        kh_val(h, it) = val = pic_make_vec(pic, 0, NULL);

        tmp = read(pic, file, c);
        PIC_SWAP(pic_value *, pic_vec_ptr(pic, tmp)->data, pic_vec_ptr(pic, val)->data);
        PIC_SWAP(int, pic_vec_ptr(pic, tmp)->len, pic_vec_ptr(pic, val)->len);

        return val;
      }

      PIC_FALLTHROUGH;
    }
  default:
    {
      kh_val(h, it) = val = read(pic, file, c);

      return val;
    }
  }
}

static pic_value
read_label_ref(pic_state *pic, xFILE PIC_UNUSED(*file), int i)
{
  khash_t(read) *h = &pic->reader.labels;
  khiter_t it;

  it = kh_get(read, h, i);
  if (it == kh_end(h)) {
    read_error(pic, "label of given index not defined", pic_list(pic, 1, pic_int_value(pic, i)));
  }
  return kh_val(h, it);
}

static pic_value
read_label(pic_state *pic, xFILE *file, int c)
{
  int i;

  i = 0;
  do {
    i = i * 10 + c - '0';
  } while (isdigit(c = next(pic, file)));

  if (c == '=') {
    return read_label_set(pic, file, i);
  }
  if (c == '#') {
    return read_label_ref(pic, file, i);
  }
  read_error(pic, "broken label expression", pic_nil_value(pic));
}

static pic_value
read_unmatch(pic_state *pic, xFILE PIC_UNUSED(*file), int PIC_UNUSED(c))
{
  read_error(pic, "unmatched parenthesis", pic_nil_value(pic));
}

static pic_value
read_dispatch(pic_state *pic, xFILE *file, int c)
{
  c = next(pic, file);

  if (c == EOF) {
    read_error(pic, "unexpected EOF", pic_nil_value(pic));
  }

  if (pic->reader.dispatch[c] == NULL) {
    read_error(pic, "invalid character at the seeker head", pic_list(pic, 1, pic_char_value(pic, c)));
  }

  return pic->reader.dispatch[c](pic, file, c);
}

static pic_value
read_nullable(pic_state *pic, xFILE *file, int c)
{
  c = skip(pic, file, c);

  if (c == EOF) {
    read_error(pic, "unexpected EOF", pic_nil_value(pic));
  }

  if (pic->reader.table[c] == NULL) {
    read_error(pic, "invalid character at the seeker head", pic_list(pic, 1, pic_char_value(pic, c)));
  }

  return pic->reader.table[c](pic, file, c);
}

static pic_value
read(pic_state *pic, xFILE *file, int c)
{
  pic_value val;

 retry:
  val = read_nullable(pic, file, c);

  if (pic_invalid_p(pic, val)) {
    c = next(pic, file);
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
pic_read(pic_state *pic, pic_value port)
{
  size_t ai = pic_enter(pic);
  pic_value val;
  xFILE *file = pic_fileno(pic, port);
  int c;

  while ((c = skip(pic, file, next(pic, file))) != EOF) {
    val = read_nullable(pic, file, c);

    if (! pic_invalid_p(pic, val)) {
      break;
    }
    pic_leave(pic, ai);
  }
  if (c == EOF) {
    return pic_eof_object(pic);
  }

  pic_leave(pic, ai);
  return pic_protect(pic, val);
}

pic_value
pic_read_cstr(pic_state *pic, const char *str)
{
  pic_value port = pic_open_port(pic, xfopen_buf(pic, str, strlen(str), "r"));
  pic_value form;

  pic_try {
    form = pic_read(pic, port);
  }
  pic_catch {
    pic_close_port(pic, port);
    pic_raise(pic, pic_err(pic));
  }

  pic_close_port(pic, port);

  return form;
}

static pic_value
pic_read_read(pic_state *pic)
{
  pic_value port = pic_stdin(pic);

  pic_get_args(pic, "|p", &port);

  return pic_read(pic, port);
}

void
pic_init_read(pic_state *pic)
{
  pic_defun(pic, "read", pic_read_read);
}
