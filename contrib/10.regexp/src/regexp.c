#include "picrin.h"
#include "picrin/data.h"
#include "picrin/pair.h"
#include "picrin/string.h"
#include "picrin/cont.h"

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

static const pic_data_type regexp_type = { "regexp", regexp_dtor };

#define pic_regexp_p(o) (pic_data_type_p((o), &regexp_type))
#define pic_regexp_data_ptr(o) ((struct pic_regexp_t *)pic_data_ptr(o)->data)

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

  reg = pic_alloc(pic, sizeof(struct pic_regexp_t));
  reg->flags = flags;

  if ((err = regcomp(&reg->reg, ptrn, cflags)) != 0) {
    char errbuf[regerror(err, &reg->reg, NULL, 0)];

    regerror(err, &reg->reg, errbuf, sizeof errbuf);
    regexp_dtor(pic, &reg->reg);

    pic_errorf(pic, "regexp compilation error: %s", errbuf);
  }

  return pic_obj_value(pic_data_alloc(pic, &regexp_type, reg));
}

static pic_value
pic_regexp_regexp_p(pic_state *pic)
{
  pic_value obj;

  pic_get_args(pic, "o", &obj);

  return pic_bool_value(pic_regexp_p(obj));
}

static pic_value
pic_regexp_regexp_match(pic_state *pic)
{
  pic_value reg;
  const char *input;
  regmatch_t match[100];
  pic_value matches, positions;
  pic_str *str;
  int i, offset;

  pic_get_args(pic, "oz", &reg, &input);

  pic_assert_type(pic, reg, regexp);

  matches = pic_nil_value();
  positions = pic_nil_value();

  if (strchr(pic_regexp_data_ptr(reg)->flags, 'g') != NULL) {
    /* global search */

    offset = 0;
    while (regexec(&pic_regexp_data_ptr(reg)->reg, input, 1, match, 0) != REG_NOMATCH) {
      pic_push(pic, pic_obj_value(pic_str_new(pic, input, match[0].rm_eo - match[0].rm_so)), matches);
      pic_push(pic, pic_int_value(offset), positions);

      offset += match[0].rm_eo;
      input += match[0].rm_eo;
    }
  } else {
    /* local search */

    if (regexec(&pic_regexp_data_ptr(reg)->reg, input, 100, match, 0) == 0) {
      for (i = 0; i < 100; ++i) {
        if (match[i].rm_so == -1) {
          break;
        }
        str = pic_str_new(pic, input + match[i].rm_so, match[i].rm_eo - match[i].rm_so);
        pic_push(pic, pic_obj_value(str), matches);
        pic_push(pic, pic_int_value(match[i].rm_so), positions);
      }
    }
  }

  if (pic_nil_p(matches)) {
    matches = pic_false_value();
    positions = pic_false_value();
  } else {
    matches = pic_reverse(pic, matches);
    positions = pic_reverse(pic, positions);
  }
  return pic_values2(pic, matches, positions);
}

static pic_value
pic_regexp_regexp_split(pic_state *pic)
{
  pic_value reg;
  const char *input;
  regmatch_t match;
  pic_value output = pic_nil_value();

  pic_get_args(pic, "oz", &reg, &input);

  pic_assert_type(pic, reg, regexp);

  while (regexec(&pic_regexp_data_ptr(reg)->reg, input, 1, &match, 0) != REG_NOMATCH) {
    pic_push(pic, pic_obj_value(pic_str_new(pic, input, match.rm_so)), output);

    input += match.rm_eo;
  }

  pic_push(pic, pic_obj_value(pic_str_new_cstr(pic, input)), output);

  return pic_reverse(pic, output);
}

static pic_value
pic_regexp_regexp_replace(pic_state *pic)
{
  pic_value reg;
  const char *input;
  regmatch_t match;
  pic_str *txt, *output = pic_str_new(pic, NULL, 0);

  pic_get_args(pic, "ozs", &reg, &input, &txt);

  pic_assert_type(pic, reg, regexp);

  while (regexec(&pic_regexp_data_ptr(reg)->reg, input, 1, &match, 0) != REG_NOMATCH) {
    output = pic_strcat(pic, output, pic_str_new(pic, input, match.rm_so));
    output = pic_strcat(pic, output, txt);

    input += match.rm_eo;
  }

  output = pic_strcat(pic, output, pic_str_new(pic, input, strlen(input)));

  return pic_obj_value(output);
}

void
pic_init_regexp(pic_state *pic)
{
  pic_deflibrary (pic, "(picrin regexp)") {
    pic_defun(pic, "regexp", pic_regexp_regexp);
    pic_defun(pic, "regexp?", pic_regexp_regexp_p);
    pic_defun(pic, "regexp-match", pic_regexp_regexp_match);
    /* pic_defun(pic, "regexp-search", pic_regexp_regexp_search); */
    pic_defun(pic, "regexp-split", pic_regexp_regexp_split);
    pic_defun(pic, "regexp-replace", pic_regexp_regexp_replace);
  }
}
