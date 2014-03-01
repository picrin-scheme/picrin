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
  tDATUM_COMMENT,
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
    char *buf;
    size_t len;
  } str;
  char c;
  struct {
    char *dat;
    size_t len, capa;
  } blob;
} YYSTYPE;

struct parser_control {
  pic_state *pic;
  YYSTYPE yylval;
  jmp_buf jmp;
  const char *msg;
};

#if defined(__cplusplus)
}
#endif

#endif
