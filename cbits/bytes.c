/*
Copyright (c) 2017-2019 Dong Han
Copyright Johan Tibell 2011, Dong Han 2019
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

#include <bytes.h>

HsInt hs_memchr(uint8_t *a, HsInt aoff, uint8_t b, HsInt n) {
    a += aoff;
    uint8_t *p = memchr(a, b, (size_t)n);
    if (p == NULL) return -1;
    else return (p - a);
}

void* __memrchr(void *src_void, unsigned char c, size_t length);
HsInt hs_memrchr(uint8_t *a, HsInt aoff, uint8_t c, HsInt n) {
    uint8_t *s = a + aoff;
    uint8_t *p = __memrchr(s, c, (size_t)n);
    if (p == NULL) return -1;
    else return (p - a);
}

/* FNV-1 hash
 *
 * The FNV-1 hash description: http://isthe.com/chongo/tech/comp/fnv/
 * The FNV-1 hash is public domain: http://isthe.com/chongo/tech/comp/fnv/#public_domain
 *
 * The original version from hashable use long type which doesn't match 'Int' in haskell and
 * cause problems on window, here we use HsInt.
 */
HsInt hs_fnv_hash_addr(const unsigned char *str, HsInt len, HsInt salt) {

    HsWord hash = salt;
    while (len--) {
      hash = (hash * 16777619) ^ *str++;
    }

    return hash;
}

HsInt hs_fnv_hash(const unsigned char *str, HsInt offset, HsInt len, HsInt salt) {
    return hs_fnv_hash_addr(str + offset, len, salt);
}

/*
 * memrchr implementation taken from newlib(which is under free software licenses)
 * https://sourceware.org/newlib
*/
/* Nonzero if X is not aligned on a "long" boundary.  */
#define UNALIGNED(X) ((long)(X + 1) & (sizeof (long) - 1))
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
void* __memrchr(void *src_void, unsigned char c, size_t length){
    unsigned char *src = (unsigned char *) src_void + length - 1;
    unsigned char d = c;
    unsigned long *asrc;
    unsigned long  mask;
    int i;
    while (UNALIGNED (src)) {
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
