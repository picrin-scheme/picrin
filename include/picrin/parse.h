/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_PARSE_H__
#define PICRIN_PARSE_H__

#if defined(__cplusplus)
extern "C" {
#endif

struct parser_control {
  pic_state *pic;
  void *yyscanner;
  pic_value value;
  bool incomp;
  int yynerrs;
  struct pic_vector *yy_arena;
  size_t yy_arena_idx;
};

#if defined(__cplusplus)
}
#endif

#endif
