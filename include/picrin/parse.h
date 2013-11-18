#ifndef PARSE_H__
#define PARSE_H__

struct parser_control {
  pic_state *pic;
  void *yyscanner;
  pic_value value;
  bool incomp;
  int yynerrs;
  struct pic_vector *yy_arena;
  int yy_arena_idx;
};

#endif
