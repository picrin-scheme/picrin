/**
 * See Copyright Notice in picrin.h
 */

#ifndef PICRIN_PARSE_H__
#define PICRIN_PARSE_H__

#if defined(__cplusplus)
extern "C" {
#endif

enum {
  tEOF = 0,
  tLABEL_SET, tLABEL_REF, tDATUM_COMMENT,
  tLPAREN, tRPAREN, tLBRACKET, tRBRACKET, tDOT, tVPAREN,
  tQUOTE, tQUASIQUOTE, tUNQUOTE, tUNQUOTE_SPLICING,
  tINT, tBOOLEAN,
  tFLOAT,
  tSYMBOL, tSTRING,
  tCHAR,
  tBYTEVECTOR,
};

typedef union YYSTYPE {
  int i;
  double f;
  struct {
    char *dat;
    size_t len;
  } buf;
  char c;
} YYSTYPE;

struct parser_control {
  pic_state *pic;
  YYSTYPE yylval;
  xhash labels;
  jmp_buf jmp;
  const char *msg;
};

#if defined(__cplusplus)
}
#endif

#endif
