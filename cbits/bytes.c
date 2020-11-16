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
#include <turbob64.h>
#ifdef __AVX2__
#include <turbob64avx2.c>
#endif

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


static const char* BIN_TO_HEX = 
	"000102030405060708090a0b0c0d0e0f"
	"101112131415161718191a1b1c1d1e1f"
	"202122232425262728292a2b2c2d2e2f"
	"303132333435363738393a3b3c3d3e3f"
	"404142434445464748494a4b4c4d4e4f"
	"505152535455565758595a5b5c5d5e5f"
	"606162636465666768696a6b6c6d6e6f"
	"707172737475767778797a7b7c7d7e7f"
	"808182838485868788898a8b8c8d8e8f"
	"909192939495969798999a9b9c9d9e9f"
	"a0a1a2a3a4a5a6a7a8a9aaabacadaeaf"
	"b0b1b2b3b4b5b6b7b8b9babbbcbdbebf"
	"c0c1c2c3c4c5c6c7c8c9cacbcccdcecf"
	"d0d1d2d3d4d5d6d7d8d9dadbdcdddedf"
	"e0e1e2e3e4e5e6e7e8e9eaebecedeeef"
	"f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff";

static const char* BIN_TO_HEX_UPPER = 
	"000102030405060708090A0B0C0D0E0F"
	"101112131415161718191A1B1C1D1E1F"
	"202122232425262728292A2B2C2D2E2F"
	"303132333435363738393A3B3C3D3E3F"
	"404142434445464748494A4B4C4D4E4F"
	"505152535455565758595A5B5C5D5E5F"
	"606162636465666768696A6B6C6D6E6F"
	"707172737475767778797A7B7C7D7E7F"
	"808182838485868788898A8B8C8D8E8F"
	"909192939495969798999A9B9C9D9E9F"
	"A0A1A2A3A4A5A6A7A8A9AAABACADAEAF"
	"B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF"
	"C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF"
	"D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF"
	"E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF"
	"F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF";

void hs_hex_encode(char* output, HsInt output_off, const uint8_t* input, HsInt input_off, HsInt input_length){
    uint16_t* output_ptr = (uint16_t*)(output+output_off);
    const uint8_t* input_ptr = input + input_off;
    uint16_t* table = (uint16_t*)BIN_TO_HEX;
    for(size_t i = 0; i != input_length;) {
        *(output_ptr++) = table[input_ptr[i++]];
    }
}

void hs_hex_encode_upper(char* output, HsInt output_off, const uint8_t* input, HsInt input_off, HsInt input_length){
    uint16_t* output_ptr = (uint16_t*)(output+output_off);
    const uint8_t* input_ptr = input + input_off;
    uint16_t* table = (uint16_t*)BIN_TO_HEX_UPPER;
    for(size_t i = 0; i != input_length;) {
        *(output_ptr++) = table[input_ptr[i++]];
    }
}

/*
* Mapping of hex characters to either their binary equivalent
* or to an error code.
*  If valid hex (0-9 A-F a-f), the value.
*  Otherwise 0xFF
* Warning: this table assumes ASCII character encodings
*/
static const uint8_t HEX_TO_BIN[256] = {
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0x80, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x01,
    0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E,
    0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0A, 0x0B, 0x0C,
    0x0D, 0x0E, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF };

HsInt hs_hex_decode(uint8_t* output, const uint8_t* input, HsInt input_off, HsInt input_length) {
    input = input+input_off;
    for(size_t i = 0; i != input_length;) {
        const uint8_t hi = HEX_TO_BIN[input[i++]];
        const uint8_t lo = HEX_TO_BIN[input[i++]];
        if((hi | lo) & 0xF0) { 
            return -1;
        }
        else {
            *(output++) = hi << 4 | lo;
        }
    }
    return 0;
}

void hs_base64_encode(uint8_t* output, HsInt output_off, const uint8_t* input, HsInt off, HsInt len){
#if defined(__AVX2__)
    tb64avx2enc(input+off, (size_t)len, output+output_off);
#elif defined(__AVX__)
    tb64avxenc(input+off, (size_t)len, output+output_off);
#elif defined(__SSE3__)
    tb64sseenc(input+off, (size_t)len, output+output_off);
#else
    tb64xenc(input+off, (size_t)len, output+output_off);
#endif
}

HsInt hs_base64_decode(uint8_t* output, const uint8_t* input, HsInt off, HsInt len){
#if defined(__AVX2__)
    return (HsInt)tb64avx2dec(input+off, (size_t)len, output);
#elif defined(__AVX__)
    return (HsInt)tb64avxdec(input+off, (size_t)len, output);
#elif defined(__SSE3__)
    return (HsInt)tb64ssedec(input+off, (size_t)len, output);
#else
    return (HsInt)tb64xdec(input+off, (size_t)len, output);
#endif
}
