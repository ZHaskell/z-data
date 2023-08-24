/*
Copyright (c) 2017-2023 Dong Han
Copyright Johan Tibell 2011
Copyright Dmitry Ivanov 2020
Copyright Georg Rudoy 2021
Copyright (c) 2017 Zach Bjornson (From https://github.com/zbjornson/fast-hex/)
                                             
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.
    * Neither the name of Johan Tibell nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <limits.h>
#include <stdint.h>
#include <string.h>
#include <HsFFI.h>
#if defined(__x86_64__)
#include <x86intrin.h>
#endif

////////////////////////////////////////////////////////////////////////////////
//  duplicate a string, interspersing the character through the elements of the duplicated string
//  from https://github.com/haskell/bytestring/pull/310/files
void z_intersperse(unsigned char *q, 
                     unsigned char *p, HsInt p_offset,
                     HsInt n,
                     unsigned char c) {
#if defined(__SSE2__)
  {
    const __m128i separator = _mm_set1_epi8(c);
    p += p_offset;
    const unsigned char *const p_begin = p;
    const unsigned char *const p_end = p_begin + n - 9;
    while (p < p_end) {
      const __m128i eight_src_bytes = _mm_loadl_epi64((__m128i *)p);
      const __m128i sixteen_dst_bytes = _mm_unpacklo_epi8(eight_src_bytes, separator);
      _mm_storeu_si128((__m128i *)q, sixteen_dst_bytes);
      p += 8;
      q += 16;
    }
    n -= p - p_begin;
  }
#endif
    while (n > 1) {
        *q++ = *p++;
        *q++ = c;
        n--;
    }
    if (n == 1)
        *q = *p;
}

////////////////////////////////////////////////////////////////////////////////
// count the number of occurences of a char in a string 
// from https://github.com/haskell/bytestring/pull/202/files

// this function should be called with large enough len, otherwise underflow will happen.
size_t z_count_addr(unsigned char *str, size_t len, unsigned char w) {
#if defined(__AVX2__) 
    __m256i pat = _mm256_set1_epi8(w);
    size_t prefix = 0, res = 0;
    size_t i = 0;

    for (; i < len && (intptr_t)(str + i) % 64; ++i) {
        prefix += str[i] == w;
    }

    for (size_t end = len - 128; i < end; i += 128) {
        __m256i p0 = _mm256_load_si256((const __m256i*)(str + i + 32 * 0));
        __m256i p1 = _mm256_load_si256((const __m256i*)(str + i + 32 * 1));
        __m256i p2 = _mm256_load_si256((const __m256i*)(str + i + 32 * 2));
        __m256i p3 = _mm256_load_si256((const __m256i*)(str + i + 32 * 3));
        __m256i r0 = _mm256_cmpeq_epi8(p0, pat);
        __m256i r1 = _mm256_cmpeq_epi8(p1, pat);
        __m256i r2 = _mm256_cmpeq_epi8(p2, pat);
        __m256i r3 = _mm256_cmpeq_epi8(p3, pat);
        res += _popcnt64(_mm256_extract_epi64(r0, 0));
        res += _popcnt64(_mm256_extract_epi64(r0, 1));
        res += _popcnt64(_mm256_extract_epi64(r0, 2));
        res += _popcnt64(_mm256_extract_epi64(r0, 3));
        res += _popcnt64(_mm256_extract_epi64(r1, 0));
        res += _popcnt64(_mm256_extract_epi64(r1, 1));
        res += _popcnt64(_mm256_extract_epi64(r1, 2));
        res += _popcnt64(_mm256_extract_epi64(r1, 3));
        res += _popcnt64(_mm256_extract_epi64(r2, 0));
        res += _popcnt64(_mm256_extract_epi64(r2, 1));
        res += _popcnt64(_mm256_extract_epi64(r2, 2));
        res += _popcnt64(_mm256_extract_epi64(r2, 3));
        res += _popcnt64(_mm256_extract_epi64(r3, 0));
        res += _popcnt64(_mm256_extract_epi64(r3, 1));
        res += _popcnt64(_mm256_extract_epi64(r3, 2));
        res += _popcnt64(_mm256_extract_epi64(r3, 3));
    }

    // _mm256_cmpeq_epi8(p, pat) returns a SIMD vector
    // with `i`th byte consisting of eight `1`s if `p[i] == pat[i]`,
    // and of eight `0`s otherwise,
    // hence each matching byte is counted 8 times by popcnt.
    // Dividing by 8 corrects for that.
    res /= 8;

    res += prefix;

    for (; i < len; ++i) {
        res += str[i] == w;
    }

    return res;
#elif defined(__SSE4_2__)
    const __m128i pat = _mm_set1_epi8(w);
    size_t res = 0;
    size_t i = 0;


    for (; i < len && (intptr_t)(str + i) % 64; ++i) {
        res += str[i] == w;
    }

    for (size_t end = len - 128; i < end; i += 128) {
        __m128i p0 = _mm_load_si128((const __m128i*)(str + i + 16 * 0));
        __m128i p1 = _mm_load_si128((const __m128i*)(str + i + 16 * 1));
        __m128i p2 = _mm_load_si128((const __m128i*)(str + i + 16 * 2));
        __m128i p3 = _mm_load_si128((const __m128i*)(str + i + 16 * 3));
        __m128i p4 = _mm_load_si128((const __m128i*)(str + i + 16 * 4));
        __m128i p5 = _mm_load_si128((const __m128i*)(str + i + 16 * 5));
        __m128i p6 = _mm_load_si128((const __m128i*)(str + i + 16 * 6));
        __m128i p7 = _mm_load_si128((const __m128i*)(str + i + 16 * 7));
        // Here, cmpestrm compares two strings in the following mode:
        // * _SIDD_SBYTE_OPS: interprets the strings as consisting of 8-bit chars,
        // * _SIDD_CMP_EQUAL_EACH: computes the number of `i`s
        //    for which `p[i]`, a part of `str`, is equal to `pat[i]`
        //    (the latter being always equal to `w`).
        //
        // q.v. https://software.intel.com/sites/landingpage/IntrinsicsGuide/#text=_mm_cmpestrm&expand=835
#define MODE _SIDD_SBYTE_OPS | _SIDD_CMP_EQUAL_EACH
        __m128i r0 = _mm_cmpestrm(p0, 16, pat, 16, MODE);
        __m128i r1 = _mm_cmpestrm(p1, 16, pat, 16, MODE);
        __m128i r2 = _mm_cmpestrm(p2, 16, pat, 16, MODE);
        __m128i r3 = _mm_cmpestrm(p3, 16, pat, 16, MODE);
        __m128i r4 = _mm_cmpestrm(p4, 16, pat, 16, MODE);
        __m128i r5 = _mm_cmpestrm(p5, 16, pat, 16, MODE);
        __m128i r6 = _mm_cmpestrm(p6, 16, pat, 16, MODE);
        __m128i r7 = _mm_cmpestrm(p7, 16, pat, 16, MODE);
#undef MODE
        res += _popcnt64(_mm_extract_epi64(r0, 0));
        res += _popcnt64(_mm_extract_epi64(r1, 0));
        res += _popcnt64(_mm_extract_epi64(r2, 0));
        res += _popcnt64(_mm_extract_epi64(r3, 0));
        res += _popcnt64(_mm_extract_epi64(r4, 0));
        res += _popcnt64(_mm_extract_epi64(r5, 0));
        res += _popcnt64(_mm_extract_epi64(r6, 0));
        res += _popcnt64(_mm_extract_epi64(r7, 0));
    }

    for (; i < len; ++i) {
        res += str[i] == w;
    }

    return res;
#else
    size_t res;
    for (res = 0; len-- != 0; ++str)
        res += *str == w;
    return res;
#endif
}

HsInt z_count(unsigned char *str, HsInt off, HsInt len, unsigned char w) {
    return (HsInt)z_count_addr(str+off, len, w);
}


////////////////////////////////////////////////////////////////////////////////
// FNV-1a

/* FNV-1a hash
 *
 * The FNV-1a hash description: http://isthe.com/chongo/tech/comp/fnv/#FNV-1a
 * The FNV-1a hash is public domain: http://isthe.com/chongo/tech/comp/fnv/#public_domain
 * 
 * Using FNV-1a because it has better avalanche properties compared to the FNV original version.
 * See also https://softwareengineering.stackexchange.com/questions/49550/which-hashing-algorithm-is-best-for-uniqueness-and-speed
 *
 * The original version from hashable use long type which doesn't match 'Int' in haskell and
 * cause problems on window, here we use HsInt.
 */
HsInt z_fnv_hash_addr(const unsigned char *str, HsInt len, HsInt salt) {

    HsWord hash = salt;
    while (len--) {
      hash = (hash ^ *str++) * 16777619;
    }

    return hash;
}

HsInt z_fnv_hash(const unsigned char *str, HsInt offset, HsInt len, HsInt salt) {
    return z_fnv_hash_addr(str + offset, len, salt);
}

////////////////////////////////////////////////////////////////////////////////
// memchr and memrchr

HsInt z_memchr(uint8_t *a, HsInt aoff, uint8_t b, HsInt n) {
    a += aoff;
    uint8_t *p = memchr(a, b, (size_t)n);
    if (p == NULL) return -1;
    else return (p - a);
}

void* z_memrchr_addr(void *src_void, unsigned char c, size_t length);

HsInt z_memrchr(uint8_t *a, HsInt aoff, uint8_t c, HsInt n) {
    a += aoff;
    uint8_t *p = z_memrchr_addr(a, c, (size_t)n);
    if (p == NULL) return -1;
    else return (p - a);
}

/*
 * memrchr implementation taken from newlib(which is under free software licenses)
 * https://sourceware.org/newlib
*/
/* Nonzero if X is not aligned on a "long" boundary.  */
#define CHECK_UNALIGNED(X) ((long)(X + 1) & (sizeof (long) - 1))
/* How many bytes are loaded each iteration of the word copy loop.  */
#define LBLOCKSIZE (sizeof (long))
/* Threshhold for punting to the bytewise iterator.  */
#define TOO_SMALL(LEN)  ((LEN) < LBLOCKSIZE)
#if LONG_MAX == 2147483647L
#define DETECTNULL(X) (((X) - 0x01010101) & ~(X) & 0x80808080)
#else
#if LONG_MAX == 9223372036854775807L
/* Nonzero if X (a long int) contains a NULL byte. */
#define DETECTNULL(X) (((X) - 0x0101010101010101) & ~(X) & 0x8080808080808080)
#else
#error long int is not a 32bit or 64bit type.
#endif
#endif
#ifndef DETECTNULL
#error long int is not a 32bit or 64bit byte
#endif
/* DETECTCHAR returns nonzero if (long)X contains the byte used
   to fill (long)MASK. */
#define DETECTCHAR(X,MASK) (DETECTNULL(X ^ MASK))
void* z_memrchr_addr(void *src_void, unsigned char c, size_t length){
    unsigned char *src = (unsigned char *) src_void + length - 1;
    unsigned char d = c;
    unsigned long *asrc;
    unsigned long  mask;
    int i;
    while (CHECK_UNALIGNED (src)) {
        if (!length--)
            return NULL;
        if (*src == d)
            return (void *) src;
        src--;
    }
    if (!TOO_SMALL (length)) {
        /* If we get this far, we know that length is large and src is
         word-aligned. */
        /* The fast code reads the source one word at a time and only
         performs the bytewise search on word-sized segments if they
         contain the search character, which is detected by XORing
         the word-sized segment with a word-sized block of the search
         character and then detecting for the presence of NUL in the
         result.  */
        asrc = (unsigned long *) (src - LBLOCKSIZE + 1);
        mask = d << 8 | d;
        mask = mask << 16 | mask;
        for (i = 32; i < LBLOCKSIZE * 8; i <<= 1)
            mask = (mask << i) | mask;
        while (length >= LBLOCKSIZE) {
            if (DETECTCHAR (*asrc, mask))
                break;
            length -= LBLOCKSIZE;
            asrc--;
        }
        /* If there are fewer than LBLOCKSIZE characters left,
         then we resort to the bytewise loop.  */
        src = (unsigned char *) asrc + LBLOCKSIZE - 1;
    }
    while (length--) {
        if (*src == d)
            return (void *) src;
        src--;
    }
    return NULL;
}