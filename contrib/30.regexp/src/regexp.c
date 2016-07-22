#include "picrin.h"
#include "picrin/extra.h"

#include <regex.h>

struct pic_regexp_t {
  regex_t reg;
  const char *flags;
};

static void
regexp_dtor(pic_state *pic, void *data)
{
  struct pic_regexp_t *preg;

  preg = data;
  regfree(&preg->reg);
  pic_free(pic, data);
}

static const pic_data_type regexp_type = { "regexp", regexp_dtor, NULL };

static pic_value
pic_regexp_regexp(pic_state *pic)
{
  const char *ptrn, *flags = "";
  int cflags, err;
  struct pic_regexp_t *reg;

  pic_get_args(pic, "z|z", &ptrn, &flags);

  cflags = REG_EXTENDED;

  while (*flags) {
    switch (*flags++) {
    case 'g':
    case 'G':
      /* pass */
      break;
    case 'i':
    case 'I':
      cflags |= REG_ICASE;
      break;
    case 'm':
    case 'M':
      cflags |= REG_NEWLINE;
      break;
    }
  }

  reg = pic_malloc(pic, sizeof(struct pic_regexp_t));
  reg->flags = flags;

  if ((err = regcomp(&reg->reg, ptrn, cflags)) != 0) {
    char errbuf[256];

    regerror(err, &reg->reg, errbuf, sizeof errbuf);
    regexp_dtor(pic, &reg->reg);

    pic_error(pic, "regexp compilation error", 1, pic_cstr_value(pic, errbuf));
  }

  return pic_data_value(pic, reg, &regexp_type);
}

static pic_value
pic_regexp_regexp_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic, pic_data_p(pic, obj, &regexp_type));
}

static pic_value
pic_regexp_regexp_match(pic_state *pic)
{
  struct pic_regexp_t *reg;
  const char *input;
  regmatch_t match[100];
  pic_value str, matches, positions;
  int i, offset;

  pic_get_args(pic, "uz", &reg, &regexp_type, &input);

  matches = pic_nil_value(pic);
  positions = pic_nil_value(pic);

  if (strchr(reg->flags, 'g') != NULL) {
    /* global search */

    offset = 0;
    while (regexec(&reg->reg, input, 1, match, 0) != REG_NOMATCH) {
      pic_push(pic, pic_str_value(pic, input, match[0].rm_eo - match[0].rm_so), matches);
      pic_push(pic, pic_int_value(pic, offset), positions);

      offset += match[0].rm_eo;
      input += match[0].rm_eo;
    }
  } else {
    /* local search */

    if (regexec(&reg->reg, input, 100, match, 0) == 0) {
      for (i = 0; i < 100; ++i) {
        if (match[i].rm_so == -1) {
          break;
        }
        str = pic_str_value(pic, input + match[i].rm_so, match[i].rm_eo - match[i].rm_so);
        pic_push(pic, str, matches);
        pic_push(pic, pic_int_value(pic, match[i].rm_so), positions);
      }
    }
  }

  if (pic_nil_p(pic, matches)) {
    matches = pic_false_value(pic);
    positions = pic_false_value(pic);
  } else {
    matches = pic_reverse(pic, matches);
    positions = pic_reverse(pic, positions);
  }
  return pic_return(pic, 2, matches, positions);
}

static pic_value
pic_regexp_regexp_split(pic_state *pic)
{
  struct pic_regexp_t *reg;
  const char *input;
  regmatch_t match;
  pic_value output = pic_nil_value(pic);

  pic_get_args(pic, "uz", &reg, &regexp_type, &input);

  while (regexec(&reg->reg, input, 1, &match, 0) != REG_NOMATCH) {
    pic_push(pic, pic_str_value(pic, input, match.rm_so), output);

    input += match.rm_eo;
  }

  pic_push(pic, pic_cstr_value(pic, input), output);

  return pic_reverse(pic, output);
}

static pic_value
pic_regexp_regexp_replace(pic_state *pic)
{
  struct pic_regexp_t *reg;
  const char *input;
  regmatch_t match;
  pic_value txt, output = pic_lit_value(pic, "");

  pic_get_args(pic, "uzs", &reg, &regexp_type, &input, &txt);

  while (regexec(&reg->reg, input, 1, &match, 0) != REG_NOMATCH) {
    output = pic_str_cat(pic, output, pic_str_value(pic, input, match.rm_so));
    output = pic_str_cat(pic, output, txt);

    input += match.rm_eo;
  }

  return pic_str_cat(pic, output, pic_str_value(pic, input, strlen(input)));
}

void
pic_init_regexp(pic_state *pic)
{
  pic_deflibrary(pic, "picrin.regexp");

  pic_defun(pic, "regexp", pic_regexp_regexp);
  pic_defun(pic, "regexp?", pic_regexp_regexp_p);
  pic_defun(pic, "regexp-match", pic_regexp_regexp_match);
  /* pic_defun(pic, "regexp-search", pic_regexp_regexp_search); */
  pic_defun(pic, "regexp-split", pic_regexp_regexp_split);
  pic_defun(pic, "regexp-replace", pic_regexp_regexp_replace);
}
