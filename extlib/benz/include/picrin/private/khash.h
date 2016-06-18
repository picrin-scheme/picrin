/* The MIT License

   Copyright (c) 2015 by Yuichi Nishiwaki <yuichi.nishiwaki@gmail.com>
   Copyright (c) 2008, 2009, 2011 by Attractive Chaos <attractor@live.co.uk>

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

#ifndef PICRIN_KHASH_H
#define PICRIN_KHASH_H

#define ac_isempty(flag, i) ((flag[i>>4]>>((i&0xfU)<<1))&2)
#define ac_isdel(flag, i) ((flag[i>>4]>>((i&0xfU)<<1))&1)
#define ac_iseither(flag, i) ((flag[i>>4]>>((i&0xfU)<<1))&3)
#define ac_set_isdel_false(flag, i) (flag[i>>4]&=~(1ul<<((i&0xfU)<<1)))
#define ac_set_isempty_false(flag, i) (flag[i>>4]&=~(2ul<<((i&0xfU)<<1)))
#define ac_set_isboth_false(flag, i) (flag[i>>4]&=~(3ul<<((i&0xfU)<<1)))
#define ac_set_isdel_true(flag, i) (flag[i>>4]|=1ul<<((i&0xfU)<<1))

#define ac_roundup32(x)                                                 \
  (--(x), (x)|=(x)>>1, (x)|=(x)>>2, (x)|=(x)>>4, (x)|=(x)>>8, (x)|=(x)>>16, ++(x))

#define ac_fsize(m) ((m) < 16? 1 : (m)>>4)
#define ac_hash_upper(x) ((((x) * 2) * 77 / 100 + 1) / 2)

#define KHASH_DECLARE(name, khkey_t, khval_t)                           \
  typedef struct {                                                      \
    int n_buckets, size, n_occupied, upper_bound;                       \
    int *flags;                                                         \
    khkey_t *keys;                                                      \
    khval_t *vals;                                                      \
  } kh_##name##_t;                                                      \
  void kh_init_##name(kh_##name##_t *h);                                \
  void kh_destroy_##name(pic_state *, kh_##name##_t *h);                \
  void kh_clear_##name(kh_##name##_t *h);                               \
  int kh_get_##name(pic_state *, const kh_##name##_t *h, khkey_t key);  \
  void kh_resize_##name(pic_state *, kh_##name##_t *h, int new_n_buckets); \
  int kh_put_##name(pic_state *, kh_##name##_t *h, khkey_t key, int *ret); \
  void kh_del_##name(kh_##name##_t *h, int x);

#define KHASH_DEFINE(name, khkey_t, khval_t, hash_func, hash_equal)     \
  KHASH_DEFINE2(name, khkey_t, khval_t, 1, hash_func, hash_equal)
#define KHASH_DEFINE2(name, khkey_t, khval_t, kh_is_map, hash_func, hash_equal) \
  void kh_init_##name(kh_##name##_t *h) {                               \
    memset(h, 0, sizeof(kh_##name##_t));                                \
  }                                                                     \
  void kh_destroy_##name(pic_state *pic, kh_##name##_t *h)              \
  {                                                                     \
    pic_free(pic, h->flags);                                            \
    pic_free(pic, (void *)h->keys);                                     \
    pic_free(pic, (void *)h->vals);                                     \
  }                                                                     \
  void kh_clear_##name(kh_##name##_t *h)                                \
  {                                                                     \
    if (h->flags) {                                                     \
      memset(h->flags, 0xaa, ac_fsize(h->n_buckets) * sizeof(int));     \
      h->size = h->n_occupied = 0;                                      \
    }                                                                   \
  }                                                                     \
  int kh_get_##name(pic_state *pic, const kh_##name##_t *h, khkey_t key) \
  {                                                                     \
    (void)pic;                                                          \
    if (h->n_buckets) {                                                 \
      int k, i, last, mask, step = 0;                                   \
      mask = h->n_buckets - 1;                                          \
      k = hash_func(key); i = k & mask;                                 \
      last = i;                                                         \
      while (!ac_isempty(h->flags, i) && (ac_isdel(h->flags, i) || !hash_equal(h->keys[i], key))) { \
        i = (i + (++step)) & mask;                                      \
        if (i == last) return h->n_buckets;                             \
      }                                                                 \
      return ac_iseither(h->flags, i)? h->n_buckets : i;		\
    } else return 0;                                                    \
  }                                                                     \
  void kh_resize_##name(pic_state *pic, kh_##name##_t *h, int new_n_buckets) \
  { /* This function uses 0.25*n_buckets bytes of working space instead of [sizeof(key_t+val_t)+.25]*n_buckets. */ \
    int *new_flags = 0;                                                 \
    int j = 1;                                                          \
    {                                                                   \
      ac_roundup32(new_n_buckets);                                      \
      if (new_n_buckets < 4) new_n_buckets = 4;                         \
      if (h->size >= ac_hash_upper(new_n_buckets)) j = 0; /* requested size is too small */ \
      else { /* hash table size to be changed (shrink or expand); rehash */ \
        new_flags = pic_malloc(pic, ac_fsize(new_n_buckets) * sizeof(int)); \
        memset(new_flags, 0xaa, ac_fsize(new_n_buckets) * sizeof(int)); \
        if (h->n_buckets < new_n_buckets) {	/* expand */		\
          h->keys = pic_realloc(pic, (void *)h->keys, new_n_buckets * sizeof(khkey_t)); \
          if (kh_is_map) {                                              \
            h->vals = pic_realloc(pic, (void *)h->vals, new_n_buckets * sizeof(khval_t)); \
          }                                                             \
        } /* otherwise shrink */                                        \
      }                                                                 \
    }                                                                   \
    if (j) { /* rehashing is needed */                                  \
      for (j = 0; j != h->n_buckets; ++j) {                             \
        if (ac_iseither(h->flags, j) == 0) {                            \
          khkey_t key = h->keys[j];                                     \
          khval_t val;                                                  \
          int new_mask;                                                 \
          new_mask = new_n_buckets - 1;                                 \
          if (kh_is_map) val = h->vals[j];                              \
          ac_set_isdel_true(h->flags, j);                               \
          while (1) { /* kick-out process; sort of like in Cuckoo hashing */ \
            int k, i, step = 0;                                         \
            k = hash_func(key);                                         \
            i = k & new_mask;                                           \
            while (!ac_isempty(new_flags, i)) i = (i + (++step)) & new_mask; \
            ac_set_isempty_false(new_flags, i);                         \
            if (i < h->n_buckets && ac_iseither(h->flags, i) == 0) { /* kick out the existing element */ \
              { khkey_t tmp = h->keys[i]; h->keys[i] = key; key = tmp; } \
              if (kh_is_map) { khval_t tmp = h->vals[i]; h->vals[i] = val; val = tmp; } \
              ac_set_isdel_true(h->flags, i); /* mark it as deleted in the old hash table */ \
            } else { /* write the element and jump out of the loop */   \
              h->keys[i] = key;                                         \
              if (kh_is_map) h->vals[i] = val;                          \
              break;                                                    \
            }                                                           \
          }                                                             \
        }                                                               \
      }                                                                 \
      if (h->n_buckets > new_n_buckets) { /* shrink the hash table */   \
        h->keys = pic_realloc(pic, (void *)h->keys, new_n_buckets * sizeof(khkey_t)); \
        if (kh_is_map) h->vals = pic_realloc(pic, (void *)h->vals, new_n_buckets * sizeof(khval_t)); \
      }                                                                 \
      pic_free(pic, h->flags); /* free the working space */             \
      h->flags = new_flags;                                             \
      h->n_buckets = new_n_buckets;                                     \
      h->n_occupied = h->size;                                          \
      h->upper_bound = ac_hash_upper(h->n_buckets);                     \
    }                                                                   \
  }                                                                     \
  int kh_put_##name(pic_state *pic, kh_##name##_t *h, khkey_t key, int *ret) \
  {                                                                     \
    int x;                                                              \
    if (h->n_occupied >= h->upper_bound) { /* update the hash table */  \
      if (h->n_buckets > (h->size<<1)) {                                \
        kh_resize_##name(pic, h, h->n_buckets - 1); /* clear "deleted" elements */ \
      } else {                                                          \
        kh_resize_##name(pic, h, h->n_buckets + 1); /* expand the hash table */ \
      }                                                                 \
    } /* TODO: to implement automatically shrinking; resize() already support shrinking */ \
    {                                                                   \
      int k, i, site, last, mask = h->n_buckets - 1, step = 0;          \
      x = site = h->n_buckets; k = hash_func(key); i = k & mask;        \
      if (ac_isempty(h->flags, i)) x = i; /* for speed up */            \
      else {                                                            \
        last = i;                                                       \
        while (!ac_isempty(h->flags, i) && (ac_isdel(h->flags, i) || !hash_equal(h->keys[i], key))) { \
          if (ac_isdel(h->flags, i)) site = i;                          \
          i = (i + (++step)) & mask;                                    \
          if (i == last) { x = site; break; }                           \
        }                                                               \
        if (x == h->n_buckets) {                                        \
          if (ac_isempty(h->flags, i) && site != h->n_buckets) x = site; \
          else x = i;                                                   \
        }                                                               \
      }                                                                 \
    }                                                                   \
    if (ac_isempty(h->flags, x)) { /* not present at all */		\
      h->keys[x] = key;                                                 \
      ac_set_isboth_false(h->flags, x);                                 \
      ++h->size; ++h->n_occupied;                                       \
      *ret = 1;                                                         \
    } else if (ac_isdel(h->flags, x)) { /* deleted */                   \
      h->keys[x] = key;                                                 \
      ac_set_isboth_false(h->flags, x);                                 \
      ++h->size;                                                        \
      *ret = 2;                                                         \
    } else *ret = 0; /* Don't touch h->keys[x] if present and not deleted */ \
    return x;                                                           \
  }                                                                     \
  void kh_del_##name(kh_##name##_t *h, int x)                           \
  {                                                                     \
    if (x != h->n_buckets && !ac_iseither(h->flags, x)) {               \
      ac_set_isdel_true(h->flags, x);                                   \
      --h->size;                                                        \
    }                                                                   \
  }

/* --- BEGIN OF HASH FUNCTIONS --- */

#define kh_ptr_hash_func(key) (int)(long)(key)
#define kh_ptr_hash_equal(a, b) ((a) == (b))
#define kh_int_hash_func(key) (int)(key)
#define kh_int_hash_equal(a, b) ((a) == (b))
PIC_INLINE int kh_str_hash_func(const char *s) {
  int h = 0;
  while (*s) {
    h = (h << 5) - h + *s++;
  }
  return h;
}
#define kh_str_cmp_func(a, b) (strcmp((a), (b)) == 0)


/* --- END OF HASH FUNCTIONS --- */

#define khash_t(name) kh_##name##_t
#define kh_init(name, h) kh_init_##name(h)
#define kh_destroy(name, h) kh_destroy_##name(pic, h)
#define kh_clear(name, h) kh_clear_##name(h)
#define kh_resize(name, h, s) kh_resize_##name(pic, h, s)
#define kh_put(name, h, k, r) kh_put_##name(pic, h, k, r)
#define kh_get(name, h, k) kh_get_##name(pic, h, k)
#define kh_del(name, h, k) kh_del_##name(h, k)

#define kh_exist(h, x) (!ac_iseither((h)->flags, (x)))
#define kh_key(h, x) ((h)->keys[x])
#define kh_val(h, x) ((h)->vals[x])
#define kh_value(h, x) ((h)->vals[x])
#define kh_begin(h) (0)
#define kh_end(h) ((h)->n_buckets)
#define kh_size(h) ((h)->size)
#define kh_n_buckets(h) ((h)->n_buckets)

#endif /* AC_KHASH_H */
