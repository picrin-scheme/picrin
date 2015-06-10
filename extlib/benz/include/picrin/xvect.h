#ifndef XVECT_H__
#define XVECT_H__

/* The MIT License

   Copyright (c) 2008, by Attractive Chaos <attractor@live.co.uk>
   Copyright (c) 2014, by Yuichi Nishiwaki <yuichi@idylls.jp>

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*/

#define xv_realloc(P,Z) pic_realloc(pic,P,Z)
#define xv_free(P) pic_free(pic,P)

#define xv_roundup32(x)                                                 \
  (--(x), (x)|=(x)>>1, (x)|=(x)>>2, (x)|=(x)>>4, (x)|=(x)>>8, (x)|=(x)>>16, ++(x))

#define xvect_t(type) struct { size_t n, m; type *a; }
#define xv_init(v) ((v).n = (v).m = 0, (v).a = 0)
#define xv_destroy(v) xv_free((v).a)
#define xv_A(v, i) ((v).a[(i)])
#define xv_pop(v) ((v).a[--(v).n])
#define xv_size(v) ((v).n)
#define xv_max(v) ((v).m)

#define xv_resize(type, v, s)                                           \
  ((v).m = (s), (v).a = (type*)xv_realloc((v).a, sizeof(type) * (v).m))

#define xv_copy(type, v1, v0)                           \
  do {                                                  \
    if ((v1).m < (v0).n) xv_resize(type, v1, (v0).n);	\
    (v1).n = (v0).n;                                    \
    memcpy((v1).a, (v0).a, sizeof(type) * (v0).n);      \
  } while (0)                                           \

#define xv_push(type, v, x)                                     \
  do {                                                          \
    if ((v).n == (v).m) {                                       \
      (v).m = (v).m? (v).m<<1 : (size_t)2;                      \
      (v).a = (type*)xv_realloc((v).a, sizeof(type) * (v).m);	\
    }                                                           \
    (v).a[(v).n++] = (x);                                       \
  } while (0)

#define xv_pushp(type, v)                                          \
  (((v).n == (v).m)?                                               \
   ((v).m = ((v).m? (v).m<<1 : (size_t)2),                         \
    (v).a = (type*)xv_realloc((v).a, sizeof(type) * (v).m), 0)     \
   : 0), ((v).a + ((v).n++))

#define xv_a(type, v, i)                                        \
  (((v).m <= (size_t)(i)?                                       \
    ((v).m = (v).n = (i) + 1, xv_roundup32((v).m),              \
     (v).a = (type*)xv_realloc((v).a, sizeof(type) * (v).m), 0) \
    : (v).n <= (size_t)(i)? (v).n = (i) + 1                     \
    : (size_t)0), (v).a[(i)])

#endif
