/*
Copyright (c) 2017-2021 Dong Han
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

#include <HsFFI.h>
#include <stdint.h>
#include <utf8rewind.h>
#include <codepoint.h>

#if defined(__AVX512F__) && defined(__AVX512VL__) && defined(__AVX512VBMI__)
#include <simdasciicheck_avx512.h>
#include <simdutf8check_avx512.h>
#elif defined(__SSSE3__) 
#include <simdasciicheck.h>
#include <simdutf8check.h>
#endif

HsInt ascii_validate(const char* p, HsInt off, HsInt len){
    const char* q = p + off;
#if defined(AVX512_IMPLEMENTATION)
    return (HsInt)validate_ascii_fast_avx512(q, (size_t)len);
#elif defined(__AVX2__)
    return (HsInt)validate_ascii_fast_avx(q, (size_t)len);
#elif defined(__SSSE3__) 
    return (HsInt)validate_ascii_fast(q, (size_t)len);
#else
    return (HsInt)ascii_u64(q, (size_t)len);
#endif
}
// for some reason unknown, on windows we have to supply a seperated version of ascii_validate
// otherwise we got segfault if we import the same FFI with different type (Addr# vs ByteArray#)
HsInt ascii_validate_addr(const char* p, HsInt len){
#if defined(AVX512_IMPLEMENTATION)
    return (HsInt)validate_ascii_fast_avx512(p, (size_t)len);
#elif defined(__AVX2__)
    return (HsInt)validate_ascii_fast_avx(p, (size_t)len);
#elif defined(__SSSE3__) 
    return (HsInt)validate_ascii_fast(p, (size_t)len);
#else
    return (HsInt)ascii_u64(p, (size_t)len);
#endif
}

HsInt utf8_validate(const char* p, HsInt off, HsInt len){
    const char* q = p + off;
#if defined(AVX512_IMPLEMENTATION)
    return (HsInt)validate_utf8_fast_avx512(q, (size_t)len);
#elif defined(__AVX2__)
    return (HsInt)validate_utf8_fast_avx(q, (size_t)len);
#elif defined(__SSSE3__) 
    return (HsInt)validate_utf8_fast(q, (size_t)len);
#else
    return utf8_validate_slow(q, (size_t)len);
#endif
}
// for some reason unknown, on windows we have to supply a seperated version of utf8_validate
// otherwise we got segfault if we import the same FFI with different type (Addr# vs ByteArray#)
HsInt utf8_validate_addr(const char* p, HsInt len){
#if defined(AVX512_IMPLEMENTATION)
    return (HsInt)validate_utf8_fast_avx512(p, (size_t)len);
#elif defined(__AVX2__)
    return (HsInt)validate_utf8_fast_avx(p, (size_t)len);
#elif defined(__SSSE3__) 
    return (HsInt)validate_utf8_fast(p, (size_t)len);
#else
    return utf8_validate_slow(p, (size_t)len);
#endif
}

////////////////////////////////////////////////////////////////////////////////

static inline int ascii_u64(const uint8_t *data, size_t len)
{
    uint8_t orall = 0;

    if (len >= 16) {

        uint64_t or1 = 0, or2 = 0;
        const uint8_t *data2 = data+8;

        do {
            or1 |= *(const uint64_t *)data;
            or2 |= *(const uint64_t *)data2;
            data += 16;
            data2 += 16;
            len -= 16;
        } while (len >= 16);

        /*
         * Idea from Benny Halevy <bhalevy@scylladb.com>
         * - 7-th bit set   ==> orall = !(non-zero) - 1 = 0 - 1 = 0xFF
         * - 7-th bit clear ==> orall = !0 - 1          = 1 - 1 = 0x00
         */
        orall = !((or1 | or2) & 0x8080808080808080ULL) - 1;
    }

    while (len--)
        orall |= *data++;

    return orall < 0x80;
}

////////////////////////////////////////////////////////////////////////////////

// Copyright (c) 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.

#define UTF8_ACCEPT 0
#define UTF8_REJECT 12

static const uint8_t utf8d[] = {
  // The first part of the table maps bytes to character classes that
  // to reduce the size of the transition table and create bitmasks.
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   // 0x00 ~ 0x1F
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   // 0x20 ~ 0x3F
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   // 0x40 ~ 0x5F
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   // 0x60 ~ 0x7F
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,   // 0x80 ~ 0x9F
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,   // 0xA0 ~ 0xBF
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,   // 0xC0 ~ 0xDF
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,   // 0xE0 ~ 0xFF

  // The second part is a transition table that maps a combination
  // of a state of the automaton and a character class to a state.
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12, 
};


static uint32_t inline updatestate(uint32_t *state, uint32_t byte) {
    uint32_t type = utf8d[byte];
    *state = utf8d[256 + *state + type];
    return *state;
}

// return 2 instead of 1, so that we can observe difference if SIMD is not used
HsInt utf8_validate_slow(const char* c, size_t len){
    const unsigned char *cu = (const unsigned char *)c;
    uint32_t state = UTF8_ACCEPT;
    for (size_t i = 0; i < len; i++) {
        uint32_t byteval = (uint32_t)cu[i];
        if (updatestate(&state, byteval) == UTF8_REJECT)
            return 0;
    }
    return ((state == UTF8_ACCEPT) ? 2 : 0);
}

static inline uint32_t decode_hex(uint32_t c) {
    if (c >= '0' && c <= '9')      return c - '0';
    else if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    else if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return 0xFFFFFFFF; // Should not happen
}

// Decode, return negative value on error
HsInt decode_json_string(char *dest, const char *src, HsInt srcoff, HsInt srclen) {
    char *d = dest;
    const char *s      = src + srcoff;
    const char *srcend = s + srclen;

    uint32_t state = UTF8_ACCEPT;
    unsigned char cur_byte;

    uint8_t surrogate = 0;
    uint32_t temp_hex = 0;
    uint32_t unidata;
    // ECMA 404 require codepoints beyond Basic Multilingual Plane encoded as surrogate pair
    uint32_t h_surrogate = 0;
    uint32_t l_surrogate = 0;

// read current byte to cur_byte and guard input end
#define DISPATCH(label) {\
    if (s >= srcend) {\
        return -1;\
    }\
    cur_byte = *s++;\
    goto label;\
}

standard:
    // Test end of stream
    while (s < srcend) {
        cur_byte = *s++;
        if (updatestate(&state, (uint32_t)cur_byte) == UTF8_REJECT) { return -1; }

        if (cur_byte == '\\')
            DISPATCH(backslash)
        else {
            *d++ = cur_byte;
        }
    }
    // Exit point, use sign bit to indicate utf8 validation error
    return (state == UTF8_ACCEPT) ? (d - dest) : (dest - d);

backslash:
    switch (cur_byte) {
        case '"':
        case '\\':
        case '/':
            *d++ = cur_byte;
            goto standard;
            break;
        case 'b': *d++ = '\b';goto standard;
        case 'f': *d++ = '\f';goto standard;
        case 'n': *d++ = '\n';goto standard;
        case 'r': *d++ = '\r';goto standard;
        case 't': *d++ = '\t';goto standard;
        case 'u': DISPATCH(unicode1);;break;
        default:
            return -1;
    }

unicode1:
    temp_hex = decode_hex(cur_byte);
    if (temp_hex == 0xFFFFFFFF) { return -1; }
    else unidata = temp_hex << 12;
    DISPATCH(unicode2);
unicode2:
    temp_hex = decode_hex(cur_byte);
    if (temp_hex == 0xFFFFFFFF) { return -1; }
    else unidata |= temp_hex << 8;
    DISPATCH(unicode3);
unicode3:
    temp_hex = decode_hex(cur_byte);
    if (temp_hex == 0xFFFFFFFF) { return -1; }
    else unidata |= temp_hex << 4;
    DISPATCH(unicode4);
unicode4:
    temp_hex = decode_hex(cur_byte);
    if (temp_hex == 0xFFFFFFFF) { return -1; }
    else unidata |= temp_hex;
    if (surrogate) {
        if (unidata < 0xDC00 || unidata > 0xDFFF) // is not low surrogate
            return -1;
        surrogate = 0;
        // decode surrogate pair
        l_surrogate = unidata;  
        unidata = 0x10000;
        unidata += (h_surrogate & 0x03FF) << 10;
        unidata += (l_surrogate & 0x03FF);
    } else if (unidata >= 0xD800 && unidata <= 0xDBFF ) { // is high surrogate
        surrogate = 1;
        DISPATCH(surrogate1);
    } else if (unidata >= 0xDC00 && unidata <= 0xDFFF) { // is low surrogate
        return -1;
    }
    // encode unidata into UTF8 bytes
    if (unidata <= 0x7F) {
        // plain ASCII
        *d++ = (char) unidata;
    }
    else if (unidata <= 0x07FF) {
        // 2-byte unicode
        *d++ = (char) (((unidata >> 6) & 0x1F) | 0xC0);
        *d++ = (char) (((unidata >> 0) & 0x3F) | 0x80);
    }
    else if (unidata <= 0xFFFF) {
        // 3-byte unicode
        *d++ = (char) (((unidata >> 12) & 0x0F) | 0xE0);
        *d++ = (char) (((unidata >>  6) & 0x3F) | 0x80);
        *d++ = (char) (((unidata >>  0) & 0x3F) | 0x80);
    }
    else if (unidata <= 0x10FFFF) {
        // 4-byte unicode
        *d++ = (char) (((unidata >> 18) & 0x07) | 0xF0);
        *d++ = (char) (((unidata >> 12) & 0x3F) | 0x80);
        *d++ = (char) (((unidata >>  6) & 0x3F) | 0x80);
        *d++ = (char) (((unidata >>  0) & 0x3F) | 0x80);
    }
    else { 
        // error 
        return -1;
    }
    goto standard;
surrogate1:
    if (cur_byte != '\\') { return -1; }
    h_surrogate = unidata;
    DISPATCH(surrogate2)
surrogate2:
    if (cur_byte != 'u') { return -1; }
    DISPATCH(unicode1)
}

// This function is used to find the ending double quote for a json string
// if return >= 0, it's the split offset, excluding the last double quote
//    return == -1, string is not ended yet
// the lowest two bytes of state record two things:
//    skip: 1 if we should skip next char, 0 otherwise
//    escaped(LSB): 1 if this string contain escaped char(s),
//                  3 if this string contain unescaped control char(s),
//                  0 otherwise
HsInt find_json_string_end(uint32_t* state, const unsigned char* ba, HsInt offset, HsInt len){
    const unsigned char *s   = ba + offset;
    const unsigned char *end = s + len;
    long skip = *state >> 8;
    long escaped = *state & 0xFF;
    for (; s < end; s++) {
        if (skip == 1){
            skip = 0;       // skip this char
        }
        else if (*s == '\\') {  // backslash
            escaped = 1;
            skip = 1;
        }
        else if (*s == '\"') {  // double quote
            *state = (skip << 8) | escaped; // save the state
            return (s - ba - offset);
        } else if (*s <= 0x1F) {  // unescaped control characters
            escaped = 3;          // even if it's skipped, it will be rejected in decode_json_string
        }
    }
    *state = (skip << 8) | escaped; // save the state
    return (-1);
}

/* rfc8259
 The representation of strings is similar to conventions used in the C
 family of programming languages.  A string begins and ends with
 quotation marks.  All Unicode characters may be placed within the
 quotation marks, except for the characters that MUST be escaped:
 quotation mark, reverse solidus, and the control characters (U+0000
 through U+001F).
*/
static const int escape_char_length[256] =
  { 6,6,6,6,6,6,6,6,2,2,2,6,2,2,6,6,
    6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
    1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,6,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

HsInt escape_json_string_length(const unsigned char *src, HsInt srcoff, HsInt srclen){
    HsInt rv = 2; // for start and end quotes 
    const unsigned char *i = src + srcoff;
    const unsigned char *srcend = i + srclen;
    // unroll loop for ~4% improvement 
    for (; i < srcend-4; ) {
        rv += escape_char_length[*i] + escape_char_length[*(i+1)] +
                escape_char_length[*(i+2)] + escape_char_length[*(i+3)];
        i += 4;
    }
    for (; i < srcend; i++) {
        rv += escape_char_length[*i];
    }
    return rv;
}

static const unsigned char DEC2HEX[16] = {
    '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'
};

HsInt escape_json_string(const unsigned char *src, HsInt srcoff, HsInt srclen, unsigned char *dest, HsInt desoff){
    const unsigned char *i = src + srcoff;
    const unsigned char *srcend = i + srclen;
    unsigned char *j = dest + desoff;
    *j++ = '\"'; // start quote
    for (; i < srcend; i++){
        if (escape_char_length[*i] == 1) {
            *j++ = *i;
        } else {
            switch (*i) {
                case '\"': *j++ = '\\'; *j++ = '\"'; break;
                case '\\': *j++ = '\\'; *j++ = '\\'; break;
                case '\b': *j++ = '\\'; *j++ = 'b'; break;
                case '\f': *j++ = '\\'; *j++ = 'f'; break;
                case '\n': *j++ = '\\'; *j++ = 'n'; break;
                case '\r': *j++ = '\\'; *j++ = 'r'; break;
                case '\t': *j++ = '\\'; *j++ = 't'; break;
                // case '/': *j++ = '\\'; *j++ = '/'; break;
                // see note above, solidus is not required to be escaped by rfc8259
                // it often appears in JSON(URL strings..)
                // for performance consideration it's omitted 
                default: 
                    *j++ = '\\';
                    *j++ = 'u';
                    *j++ = '0';
                    *j++ = '0';
                    *j++ = DEC2HEX[*i >> 4];
                    *j++ = DEC2HEX[*i & 0xF];
            }
        }
    }
    *j++ = '\"'; // end quote
    return (HsInt)(j-dest);
}

////////////////////////////////////////////////////////////////////////////////

HsInt utf8_isnormalized(const char* p, HsInt off, HsInt len, size_t flag){
    size_t offset;
    return (HsInt)utf8isnormalized(p+off, len, flag, &offset);
}

HsInt utf8_normalize(const char* p, HsInt off, HsInt len, char* q, HsInt len2, size_t flag){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8normalize(p+off, len, q, len2, flag, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_normalize_length(const char* p, HsInt off, HsInt len, size_t flag){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8normalize(p+off, len, NULL, 0, flag, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_casefold(const char* p, HsInt off, HsInt len, char* q, HsInt len2, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8casefold(p+off, len, q, len2, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_casefold_length(const char* p, HsInt off, HsInt len, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8casefold(p+off, len, NULL, 0, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_tolower(const char* p, HsInt off, HsInt len, char* q, HsInt len2, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8tolower(p+off, len, q, len2, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_tolower_length(const char* p, HsInt off, HsInt len, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8tolower(p+off, len, NULL, 0, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_toupper(const char* p, HsInt off, HsInt len, char* q, HsInt len2, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8toupper(p+off, len, q, len2, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_toupper_length(const char* p, HsInt off, HsInt len, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8toupper(p+off, len, NULL, 0, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_totitle(const char* p, HsInt off, HsInt len, char* q, HsInt len2, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8totitle(p+off, len, q, len2, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_totitle_length(const char* p, HsInt off, HsInt len, size_t locale){
    size_t converted_size;
    int32_t errors;
    if ((converted_size = utf8totitle(p+off, len, NULL, 0, locale, &errors)) == 0 ||
        errors != UTF8_ERR_NONE)
    {
        return -1;
    } else {
        return converted_size;
    }
}

HsInt utf8_iscategory(const char* p, HsInt off, HsInt len, size_t flags){
    return (HsInt)utf8iscategory(p+off, len, flags);
}

