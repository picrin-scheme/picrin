/* emyg_atod.c 
**
** Copyright (C) 2015 Doug Currie, Londonderry, NH, USA
** 
** Permission is hereby granted, free of charge, to any person obtaining a copy
** of this software and associated documentation files (the "Software"), to deal
** in the Software without restriction, including without limitation the rights
** to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
** copies of the Software, and to permit persons to whom the Software is
** furnished to do so, subject to the following conditions:
** 
** The above copyright notice and this permission notice shall be included in
** all copies or substantial portions of the Software.
** 
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
** IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
** FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
** AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
** LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
** OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
** THE SOFTWARE.
*/

/* This code is an implementation of strtod() to accompany emyg_dtoa.c
** It is based on an algorithm described by Aubrey Jaffer, January 2015, 
** "Easy Accurate Reading and Writing of Floating-Point Numbers"
** arXiv:1310.8121v6 -- http://arxiv.org/abs/1310.8121v6
*/
/* This implementation, besides the obvious translation from Java to C, has
** some differences/improvements:
** 1. All memory allocations are on the stack, no heap allocations
** 2. We don't shift numerator right for the case: negative dpoint, positive bex
**    This can happen for an input like -139745422447689500.0 that has... 
**    mant: 1397454224476895000 dp: -1 bex: 5. Shifting the mantissa right would 
**    lose information; instead we shift the scale factor left.
** 3. We use a roundQuotient function (quornd) optimized for atod; it adjusts 
**    the result to be 53 bits if it is 54 before rounding by appropriate use
**    of the quotient LSB and remainder; this avoids doing the divide twice. 
*/

#include <stdlib.h>
#include <limits.h>
#include <stdint.h>
#include <ctype.h>  // used by emyg_strtod
#include <math.h>   // scalbn()

#include "emyg_pow5.h"  // in separate file to support alternate implementations, and it's big

#ifdef TESTING_QUOREM
#include <stdio.h>
#include <string.h>
#define QUODBG(x) x
#else
#define QUODBG(x)
#endif

#define OPTIMIZE_FOR_ATOD

#define BIGNUM_JUMBO_SIZE_UINT32   (30) /* 960 bits > (64 + 54 + (log (expt 5 345) 2) = 919) */
#define BIGNUM_NORMAL_SIZE_UINT32  (28) /* when we don't need the extra space for multiply */
#define BIGNUM_QUOTIENT_SIZE_UINT32 (4) /* difference twixt dividend - divisor; see m - n + 1 */

static const int doubleMantissaBits = 53;

static inline int nlz32 (uint32_t x)
{
#if defined(_MSC_VER) && defined(_M_AMD64)
   unsigned long bitno;
   uint8_t s = _BitScanReverse(&bitno, (unsigned long )x);
   return s ? (31 - (int )bitno) : 32;
#elif defined(__GNUC__) && (UINT_MAX == 0xFFFFFFFFUL)
   return (x == 0u) ? 32 : __builtin_clz((unsigned int )x);
#elif defined(__GNUC__) && (ULONG_MAX == 0xFFFFFFFFUL)
   return (x == 0u) ? 32 : __builtin_clzl((unsigned long )x);
#else
   int n;
   if (x == 0) return(32);
   n = 0;
   if (x <= 0x0000FFFF) { n = n +16; x = x <<16; }
   if (x <= 0x00FFFFFF) { n = n + 8; x = x << 8; }
   if (x <= 0x0FFFFFFF) { n = n + 4; x = x << 4; }
   if (x <= 0x3FFFFFFF) { n = n + 2; x = x << 2; }
   if (x <= 0x7FFFFFFF) { n = n + 1; }
   return n;
#endif
}

static inline int nlz64 (uint64_t x)
{
#if defined(_MSC_VER) && defined(_M_AMD64)
    unsigned long bitno;
    uint8_t s = _BitScanReverse64(&bitno, x);
    return s ? (63 - (int )bitno) : 64;
#elif defined(__GNUC__) && (ULONG_MAX > 0xFFFFFFFFFFFFFFF5ULL)
   return (x == 0u) ? 64 : __builtin_clzl((unsigned long )x);
#elif defined(__GNUC__) && (ULONGLONG_MAX > 0xFFFFFFFFFFFFFFF5ULL)
    return (x == 0u) ? 64 : __builtin_clzll((unsigned long long )x);
#else
    int s = nlz32(x >> 32);
    return (s == 32) ? (32 + nlz32(x & UINT32_MAX)) : s;
#endif
}

/* Divide code divmnu.c adapted from Hacker's Delight; here are the changes:
** 1. Eliminated signed arithmetic (shift left and conversions) since
**    these are undefined behavior for C; adapted similar solution from 
**    David Ireland's BigDigits library.
** 2. Removed the option to return the remainder, and added the rounding
**    feature needed for IEEE conversion.
** 3. Eliminated the alloca() calls; we know that for IEEE doubles none
**    of our bignums will be larger than 112 bits.
** 4. Added code specific to emyg_atod() to force result size to 53 bits
**    and returning a -1 to indicate the scaling by /2
** Hacker's Delight web site has a permission statement, incuding: you are 
** free to use, copy, and distribute any of the code on this web site,
** whether modified by you or not. You need not give attribution.
*/

/* This divides an n-word dividend by an m-word divisor, giving an
** n-m+1-word quotient and m-word remainder. The bignums are in arrays of
** words. Here a "word" is 32 bits. This routine is designed for a 64-bit
** machine which has a 64/64 division instruction. */

/* q[0], u[0], and v[0] contain the LEAST significant words.
** (The sequence is in little-endian order).
** 
** This is a fairly precise implementation of Knuth's Algorithm D, for a
** binary computer with base b = 2**32. The caller supplies:
**    1. Space q for the quotient, m - n + 1 words (at least one).
**    2. The dividend u, m words, m >= 1.
**    3. The divisor v, n words, n >= 1.
** The most significant digit of the divisor, v[n-1], must be nonzero.  The
** dividend u may have leading zeros; this just makes the algorithm take
** longer and makes the quotient contain more leading zeros. 
**    The program does not alter the input parameters u and v.
**    The quotient returned may have leading zeros.  The
** function itself returns a value of 0 for success and 1 for invalid
** parameters (e.g., division by 0).
**    For now, we must have m >= n.  Knuth's Algorithm D also requires
** that the dividend be at least as long as the divisor.  (In his terms,
** m >= 0 (unstated).  Therefore m+n >= n.) 
*/

int quornd (uint32_t q[], const uint32_t u[], const uint32_t v[], int m, int n)
{
    const uint64_t b = 4294967296ULL; // Number base (2**32).
    uint32_t un[BIGNUM_JUMBO_SIZE_UINT32]; // Normalized form of u, v.
    uint32_t vn[BIGNUM_JUMBO_SIZE_UINT32];
    uint64_t qhat;                    // Estimated quotient digit.
    uint64_t rhat;                    // A remainder.
    uint64_t p;                       // Product of two digits.
    uint64_t t, k;
    int s, i, j;
    int res = 0;

    if (m < n || n <= 0 || v[n-1] == 0)
        return 1;                         // Return if invalid param.

    if (n == 1)
    {
        k = 0;                            // Take care of the case of a
        for (j = m - 1; j >= 0; j--)      // single-digit divisor here.
        {
            q[j] = (k*b + u[j]) / v[0];
            k = (k*b + u[j]) - (q[j] * v[0]);
        }
#ifdef OPTIMIZE_FOR_ATOD
        if ((64 - nlz32(q[1])) > doubleMantissaBits)
        {
           // we need to divide quotient by 2 to make it fit
           res = -1; // let caller know we made this adjustment
           uint32_t saved_q0 = q[0];
           q[0] = (q[0] >> 1) | (q[1] << 31);
           q[1] =  q[1] >> 1;

           if (0 == (saved_q0 & 1))
               return res; // no need to round
           // now the remainder is >= 0.5
           // if un is 0, then the remainder is 0.5
           // otherwise it is > 0.5
           if ((k & UINT32_MAX) != 0)
                goto round_up; // (2 * remainder) > divisor, round
           // continue with check for round_even
           if ((q[0] & 1) == 1)
                goto round_up; // round to even
           return res;
        }
#endif
        // rounding
        k = (k & UINT32_MAX) * 2; // k is now 2 * remainder
        if ((k > v[0]) || ((k == v[0]) && ((q[0] & 1) == 1)))
        {
round_up:
            i = 0;
            do
            {
                t = (uint64_t)q[i] + 1;
                q[i++] = t;
            } while ((t > UINT32_MAX) && (i < (m - n + 1)));
        }
        return res;
    }

   /* Normalize by shifting v left just enough so that its high-order
   bit is on, and shift u left the same amount. We may have to append a
   high-order digit on the dividend; we do that unconditionally. */

    s = nlz32(v[n-1]);             // 0 <= s <= 31.

    if (n > BIGNUM_JUMBO_SIZE_UINT32) return 1;
    for (i = n - 1; i > 0; i--)
       vn[i] = (v[i] << s) | ((uint64_t)v[i-1] >> (32-s));
    vn[0] = v[0] << s;

    if ((m + 1) > BIGNUM_JUMBO_SIZE_UINT32) return 1;
    un[m] = (uint64_t)u[m-1] >> (32-s);
    for (i = m - 1; i > 0; i--)
       un[i] = (u[i] << s) | ((uint64_t)u[i-1] >> (32-s));
    un[0] = u[0] << s;

    for (j = m - n; j >= 0; j--)         // Main loop.
    {
        // Compute estimate qhat of q[j].
        qhat = (un[j+n]*b + un[j+n-1]) / vn[n-1];
        rhat = (un[j+n]*b + un[j+n-1]) - qhat*vn[n-1];
again:
        if (qhat >= b || qhat*vn[n-2] > b*rhat + un[j+n-2])
        {
            qhat = qhat - 1;
            rhat = rhat + vn[n-1];
            if (rhat < b) goto again;
        }

        // Multiply and subtract.
        k = 0;
        for (i = 0; i < n; i++)
        {
            p = qhat * vn[i];
            un[i+j] -= k;
            k = (un[i+j] > (UINT32_MAX - k)) ? 1 : 0;
            un[i+j] -= (p & UINT32_MAX);
            if (un[i+j] > (UINT32_MAX - (p & UINT32_MAX))) k++;
            k += (p >> 32);
        }
        un[j+n] -= k;

        q[j] = qhat;              // Store quotient digit.
        if (un[j+n])              // If we subtracted too
        {
            q[j] = q[j] - 1;       // much, add back.
            k = 0;
            for (i = 0; i < n; i++)
            {
                t = (uint64_t)un[i+j] + vn[i] + k;
                un[i+j] = t;
                k = t >> 32;
            }
            un[j+n] = un[j+n] + k;
        }
    } // End j.

#ifdef OPTIMIZE_FOR_ATOD
    if ((64 - nlz32(q[1])) > doubleMantissaBits)
    {
        // we need to divide quotient by 2 to make it fit
        res = -1; // let caller know we made this adjustment
        uint32_t saved_q0 = q[0];
        q[0] = (q[0] >> 1) | (q[1] << 31);
        q[1] =  q[1] >> 1;

        if (0 == (saved_q0 & 1))
            return -1; // no need to round
        // now the remainder is >= 0.5
        // if un is 0, then the remainder is 0.5
        // otherwise it is > 0.5
        for (i = n-1; i >= 0; i--)
        {
            if (un[i] != 0)
                goto round_up; // (2 * remainder) > divisor, round
        }
        // continue with check for round_even
    }
    else
#endif
    {
        // Rounding: multiply the remainder by 2 and compare with the divisor
        //
        if (un[n-1] > (UINT32_MAX / 2u))
            goto round_up;

        for (i = n-1; i > 0; i--)
        {
            uint32_t ud = (un[i] << 1) | (un[i-1] >> 31);
            if (ud > vn[i])
                goto round_up; // (2 * remainder) > divisor, round
            if (ud < vn[i])
                return 0;      // (2 * remainder) < divisor, done
        }
        if ((un[0] << 1) > vn[0])
            goto round_up; // (2 * remainder) > divisor, round
        if ((un[0] << 1) < vn[0])
            return 0;      // (2 * remainder) < divisor, done
   }
   // Check for round to even
   // (2 * remainder) == divisor
   if ((q[0] & 1) == 1) goto round_up; // round to even
   return res;
}

#if TESTING_QUOREM
void print_bigint (const char *nm, const uint32_t num[], const int n)
{
    int i;
    printf("%s: ", nm);
    for (i = 0; i < n; i++)
        printf("%u ", num[i]);
    printf("\n");
}
#endif

static inline void one_shiftLeft (uint32_t v[], int s)
{
    int x = 0;
    while (s >= 32) { v[x++] = 0; s -= 32; }
    v[x] = 1u << s;
}

static inline void scl_shift_left_by (uint32_t v[], const uint32_t x[], int sz, int s)
{
    // v and x may be identical
    int w = s / 32; // words to shift
    int b = s % 32; // bits to shift
    int i;

    QUODBG(printf("scl_shift_left_by sz %d shift %d (%d %d)\n", sz, s, w, b));

    // sz is the size of the result, v
    // x is guaranteed to be appropriately sized to provide enough data
    // this is a dumb api but works for now

    // first copy words
    v[sz - 1] = 0u;
    for (i = sz - 2; i >= 0; i--)
    {
        v[i] = ((i - w) >= 0) ? x[i - w] : 0u;
    }

    // then shift bits
    for (i = sz - 1; i > 0; i--)
    {
        v[i] = (v[i] << b) | (v[i - 1] >> (32 - b));
    }
    v[0] = (v[0] << b);
}

static inline void u64_shiftLeft (uint32_t v[], uint64_t n, int sz, int s)
{
    uint64_t ns;
    int x = 0;

    if (s >= 0)
    {
        while (s >= 32) { v[x++] = 0; s -= 32; }
        ns = n << s;
        v[x++] = (ns & UINT32_MAX);
        if (x < sz) // -- don't write past end of v[], caller determined size needed
        {
            v[x++] = (ns >> 32);
            if (x < sz && s != 0) // no more data if s == 0
                v[x] = n >> (64 - s);
        }
    }
}

static inline double doubleValue (uint32_t v[])
{
    // only the fist 64-bits of v[] are used; caller has determined that's all that's needed
    return (double )((uint64_t )v[0] + ((uint64_t )v[1] << 32));
}

static inline int bitLength (const uint32_t v[], int sz_in_32bit_words)
{
    int x = sz_in_32bit_words - 1;

    while ((v[x] == 0) && (x > 0)) x = x - 1;
    return (x * 32) + (32 - nlz32(v[x]));
}

static inline void mulbyu64 (uint32_t p[], const uint64_t u64mant, const uint32_t m[], int z)
{
    // p is z+2 32-bit words
    // m is z   32-bit words

    // TODO: could this be optimized to use 128 bit math on platforms that support it?

    uint64_t lo = u64mant & UINT32_MAX;
    uint64_t hi = u64mant >> 32;
    uint64_t a = 0;
    int i;

    for (i = 0; i < z; i++)
    {
        a += lo * m[i];
        p[i] = a & UINT32_MAX;
        a = a >> 32;
    }
    p[z] = a;
    //p[z+1] = 0;
    a = 0;
    if (hi > 0) for (i = 0; i < z; i++)
    {
        a += (hi * m[i]) + p[i+1]; // this always (just) fits in 64 bits
        p[i+1] = (a & UINT32_MAX);
        a = a >> 32;
    }
    p[z+1] = a;
}

/* atod_guts
** input: u64mant  dpoint 
** result: u64mant * 10**dpoint
** 
** case dpoint >= 0:
**     reframe as: u64mant * 5**dpoint * 2**dpoint
**         if (u64mant * 5**dpoint) is less than 53 bits
**         then the answer is just scalb(u64mant * 5**dpoint, dpoint)
**         else
**             calculate bex = the bit length of (u64mant * 5**dpoint)
**             divide and round: (u64mant * 5**dpoint) / 2**bex
**             the answer is scalb(u64mant * 5**dpoint, bex + dpoint)
** case dpoint < 0
**     reframe as: (u64mant / (5**(-dpoint)) / 2**(-dpoint)
*/
double atod_guts (uint64_t u64mant, int dpoint)
{
    const uint32_t *pow5p;
    uint32_t num[BIGNUM_JUMBO_SIZE_UINT32]; // up to (log (expt 5 325) 2) = 755 bits => ~104 bytes
    uint32_t scl[BIGNUM_NORMAL_SIZE_UINT32];
    uint32_t quo[BIGNUM_QUOTIENT_SIZE_UINT32];
    int n, m; // as per quornd: n is size of divisor, m is size of dividend in 32-bit words
    int z;    // z is size of pow5p in 32-bit words
    int bex;  // binary exponentish

    QUODBG(fprintf(stderr, "mant: %llu dp: %d\n", u64mant, dpoint));

    if (dpoint >= 0)
    {
        int r = get_pow5(dpoint, &pow5p, &z);
        if (r) return 0.0/0.0; // NaN for bad input -- TODO: use inf for excessive +exponents?
        m = z + 2; // size is sum of lengths of multiplicands
        if (m > BIGNUM_JUMBO_SIZE_UINT32) { n = 0; goto atod_fail; }
        mulbyu64(num, u64mant, pow5p, z); // num = pow5 * u64mant
        bex = bitLength(num, m) - doubleMantissaBits;
        if (bex <= 0) return scalbn(doubleValue(num), dpoint);
        QUODBG(fprintf(stderr, "+ bex: %d z: %d m: %d\n", bex, z, m));
        n = (bex / 32) + 1; // 32-bit words needed to hold 1 << bex
        m = (bex + doubleMantissaBits + 31) / 32; // we may be able to shrink by a word
        if (n > BIGNUM_NORMAL_SIZE_UINT32) goto atod_fail;
        one_shiftLeft(scl, bex);
        if ((m - n + 1) > BIGNUM_QUOTIENT_SIZE_UINT32) goto atod_fail;
        QUODBG(fprintf(stderr, "+ n: %d m: %d q: %d\n", n, m, m - n + 1));
        if (quornd(quo, num, scl, m, n))
        {
            QUODBG(fprintf(stderr, "+ quornd returned error\n"));
            return 0.0/0.0; // NaN for bad input or software error
        }
        QUODBG(print_bigint("num", num, m));
        QUODBG(print_bigint("scl", scl, n));
        QUODBG(print_bigint("quo", quo, m - n + 1));
        return scalbn(doubleValue(quo), bex + dpoint);
    }
    else
    {
        int bma = 64 - nlz64(u64mant); // bits in mantissa
        int r = get_pow5(-dpoint, &pow5p, &z);
        if (r) return 0.0/0.0; // NaN for bad input -- TODO: use 0.0 for excessive -exponents?
        bex = bma - bitLength(pow5p, z) - doubleMantissaBits;
        if (bex > 0)
        {
            // to avoid losing significant bits, which could occur in the u64_shiftLeft below
            // instead of shifting num right, let's shift pow5 left
            // DANGER Will Robinson! We are aliasing pow5p and scl -- this is OK since 
            // the only place below they are both used in the same statement or call is 
            // scl_shift_left_by(), which is designed to handle aliased pointers
            // and which doesn't happen at all if OPTIMIZE_FOR_ATOD is defined
            //
            m = 2;
            if (m > BIGNUM_JUMBO_SIZE_UINT32) { n = 0; goto atod_fail; }
            num[0] = u64mant & UINT32_MAX;
            num[1] = u64mant >> 32;
            // use z+1 because scl_shift_left_by my shift by as many as 10 bits (64 - 1 - 53)
            if ((z + 1) > BIGNUM_NORMAL_SIZE_UINT32) { n = 0; goto atod_fail; }
            scl_shift_left_by(scl, pow5p, z + 1, bex);
            QUODBG(print_bigint("scl", scl, z + 1));
            n = (bitLength(scl, z + 1) + 31) / 32;
            pow5p = scl; // DANGER Will Robinson! -- see above
            QUODBG(fprintf(stderr, "- bma %d bex: %d z: %d m: %d n: %d\n", bma, bex, z, m, n));
        }
        else
        {
            m = (((bma - bex) + 31) / 32); // u64mant << -bex 
            if (m > BIGNUM_JUMBO_SIZE_UINT32) { n = 0; goto atod_fail; }
            QUODBG(fprintf(stderr, "- bma %d bex: %d z: %d m: %d\n", bma, bex, z, m));
            u64_shiftLeft(num, u64mant, m, -bex);
            n = z;
        }
        if ((m - n + 1) > BIGNUM_QUOTIENT_SIZE_UINT32) goto atod_fail;
        QUODBG(fprintf(stderr, "- n: %d m: %d q: %d\n", n, m, m - n + 1));
        r = quornd(quo, num, pow5p, m, n);
        if (r > 0)
        {
            QUODBG(fprintf(stderr, "- quornd returned error\n"));
            return 0.0/0.0; // NaN for bad input
        }
        QUODBG(print_bigint("num", num, m));
        QUODBG(print_bigint("pow5p", pow5p, n));
        QUODBG(print_bigint("quo", quo, m - n + 1));
#ifdef OPTIMIZE_FOR_ATOD
        QUODBG(fprintf(stderr, "- r: %d\n", r));
        if (r < 0)
        {
            bex++;
        }
#else
        bma = 64 - nlz64(doubleValue(quo));
        if (bma > doubleMantissaBits)
        {
            bex++;
            n = (pow5p[z-1] > (UINT32_MAX / 2u)) ? z + 1 : z;
            // use z+2 because mulbyu64 will set those words even though they may be zero
            if ((z + 2) > BIGNUM_NORMAL_SIZE_UINT32) goto atod_fail;
            QUODBG(fprintf(stderr, "- n: %d m: %d q: %d bma: %d\n", n, m, m - n + 1, bma));
            scl_shift_left_by(scl, pow5p, z + 1, 1);
            if (quornd(quo, num, scl, m, n))
            {
                QUODBG(fprintf(stderr, "- quornd returned error; v[n-1]: %u\n", scl[n-1]));
                return 0.0/0.0; // NaN for bad input
            }
            QUODBG(print_bigint("num", num, m));
            QUODBG(print_bigint("scl", scl, n));
            QUODBG(print_bigint("quo", quo, m - n + 1));
        }
#endif
        return scalbn(doubleValue(quo), bex + dpoint);
   }
atod_fail:
    QUODBG(fprintf(stderr, "atod_guts undersized bignum n: %d m: %d\n", n, m));
    return 0.0/0.0; // NaN for bad input
}

/*
double strtod(const char *nptr, char **endptr);
    The expected form of the (initial portion of the) string is optional
    leading white space as recognized by isspace(3), an optional plus
    ('+') or minus sign ('-') and then either (i) a decimal number, or
    (ii) a hexadecimal number, or (iii) an infinity, or (iv) a NAN (not-
    a-number).
 
    A decimal number consists of a nonempty sequence of decimal digits
    possibly containing a radix character (decimal point, locale-
    dependent, usually '.'), optionally followed by a decimal exponent.
    A decimal exponent consists of an 'E' or 'e', followed by an optional
    plus or minus sign, followed by a nonempty sequence of decimal
    digits, and indicates multiplication by a power of 10.
 
    A hexadecimal number consists of a "0x" or "0X" followed by a
    nonempty sequence of hexadecimal digits possibly containing a radix
    character, optionally followed by a binary exponent.  A binary
    exponent consists of a 'P' or 'p', followed by an optional plus or
    minus sign, followed by a nonempty sequence of decimal digits, and
    indicates multiplication by a power of 2.  At least one of radix
    character and binary exponent must be present.
 
    An infinity is either "INF" or "INFINITY", disregarding case.
 
    A NAN is "NAN" (disregarding case) optionally followed by a string,
    (n-char-sequence), where n-char-sequence specifies in an
    implementation-dependent way the type of NAN (see NOTES).

    These functions return the converted value, if any.

    If endptr is not NULL, a pointer to the character after the last
    character used in the conversion is stored in the location referenced
    by endptr.

    If no conversion is performed, zero is returned and the value of nptr
    is stored in the location referenced by endptr.

    If the correct value would cause overflow, plus or minus HUGE_VAL
    (HUGE_VALF, HUGE_VALL) is returned (according to the sign of the
    value), and ERANGE is stored in errno.  If the correct value would
    cause underflow, zero is returned and ERANGE is stored in errno.
*/

double emyg_strtod(const char *nptr, char **endptr)
{
    const char *cp = nptr;
    double res;
    uint64_t mant;  // mantissa
    int minus = 0;  // mantissa minus
    int dadp = 0;   // digits after decimal point
    int expt = 0;   // explicit exponent
    int expm = 0;   // exponent minus
    char c;

    while (isspace(c = *cp++)) /*skip*/;

    if ('-' == c) { minus = 1; c = *cp++; }
    else if ('+' == c) { c = *cp++; }
    else {}

    if (('n' == c     || 'N' == c)
     && ('a' == cp[0] || 'A' == cp[0])
     && ('n' == cp[1] || 'N' == cp[1]))
    {
        // for Scheme allow (require?) ".0"
        if ('.' == cp[2] && '0' == cp[3]) cp = &cp[4];
        else cp = &cp[2];
        res = 0.0/0.0;
    }
    else 
    if (('i' == c     || 'I' == c)
     && ('n' == cp[0] || 'N' == cp[0])
     && ('f' == cp[1] || 'F' == cp[1]))
    {
        // for Scheme allow (require?) ".0"
        if ('.' == cp[2] && '0' == cp[3]) cp = &cp[4];
        else cp = &cp[2];
        res = 1.0/0.0;
        // TODO: allow "INITY"
    }
    else if ('.' == c)
    {
        mant = 0;
        goto after_dp;
    }
    else if (isdigit(c))
    {
        mant = c - '0'; // mantissa
        while (isdigit(c = *cp++))
        {
            uint8_t d = c - '0';
            if (mant <= (UINT64_MAX / 10)) mant *= 10;
            else
            {
                if (0 == d) expt += 1;
                else
                {
                    QUODBG(fprintf(stderr, "- overlow in emyg_strtod 1\n"));
                    goto no_conv;
                }
            }
            if (mant <= (UINT64_MAX - d)) mant += d;
            else
            {
                QUODBG(fprintf(stderr, "- overlow in emyg_strtod 2\n"));
                goto no_conv;
            }
        }
        if ('.' == c)
        {
        after_dp:
            while (isdigit(c = *cp++))
            {
                uint8_t d = c - '0';
                if (mant <= (UINT64_MAX / 10)) mant *= 10;
                else
                {
                    if (0 == d)
                    {
                        /* ignore trailing zeroes by not scaling nor bumping dadp */
                        continue;
                    }
                    else
                    {
                        QUODBG(fprintf(stderr, "- overlow in emyg_strtod 3\n"));
                        //goto no_conv;
                        continue; // ignore extra digits
                    }
                }
                if (mant <= (UINT64_MAX - d)) mant += d;
                else
                {
                    QUODBG(fprintf(stderr, "- overlow in emyg_strtod 4\n"));
                    //goto no_conv;
                    continue; // ignore extra digits
                }
                dadp++;
            }   
        }
        if ('e' == c || 'E' == c)
        {
            c = *cp++;
            if ('-' == c) { expm = 1; c = *cp++; }
            else if ('+' == c) { c = *cp++; }
            else {}

            if (isdigit(c))
            {
                expt += c - '0';
                while (isdigit(c = *cp++))
                {
                    uint8_t d = c - '0';
                    if (expt <= (INT_MAX / 10)) expt *= 10;
                    else
                    {
                        QUODBG(fprintf(stderr, "- overlow in emyg_strtod\n"));
                        goto no_conv;
                    }
                    if (expt <= (INT_MAX - d)) expt += d;
                    else
                    {
                        QUODBG(fprintf(stderr, "- overlow in emyg_strtod\n"));
                        goto no_conv;
                    }
                }
            }
            else
            {
                // oops, not an exp at all
                c = *--cp;
                if (('-' == c) || ('+' == c)) --cp;
                res = 0.0;
            }
        }
        if (expm) expt = -expt;
        res = atod_guts(mant, expt - dadp);
    }
    else // no conversion
    {
no_conv:
        cp = nptr + 1;
        res = 0.0;
    }
    if (NULL != endptr) *endptr = (char *)--cp;
    return minus ? -res : res;
}

double emyg_atod (const char *nptr)
{
    return emyg_strtod(nptr, NULL);
}

#ifdef TESTING_QUOREM

int main (int argc, char **argv)
{
    if ((argc == 4) && (0 == strcmp(argv[1], "-p")))
    {
        char *p;
        double d = atod_guts(strtoull(argv[2], &p, 10), strtol(argv[3], &p, 10));
        printf("%.17g  %a\n", d, d);
        return 0;
    }

    if ((argc == 3) && (0 == strcmp(argv[1], "-s")))
    {
        char *p;
        double d = emyg_strtod(argv[2], &p);
        printf("%.17g  %a\n", d, d);
        return 0;
    }
    return 0;
}

#endif
