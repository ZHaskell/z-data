/*
 * Copyright (c) 2017-2018 Dong Han
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the authors or the names of any contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <Rts.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>

HsInt hs_memchr(uint8_t *a, HsInt aoff, uint8_t b, HsInt n);
HsInt hs_memrchr(uint8_t *a, HsInt aoff, uint8_t c, HsInt n);
HsInt hs_fnv_hash_addr(const unsigned char* str, HsInt len, HsInt salt);
HsInt hs_fnv_hash(const unsigned char* str, HsInt offset, HsInt len, HsInt salt);

void hs_hex_encode(char* output, HsInt output_off, const uint8_t* input, HsInt input_off, HsInt input_length);
void hs_hex_encode_upper(char* output, HsInt output_off, const uint8_t* input, HsInt input_off, HsInt input_length);
HsInt hs_hex_decode(uint8_t* output, const uint8_t* input, HsInt input_off, HsInt input_length);

void hs_base64_encode(uint8_t* output, HsInt output_off, const uint8_t* input, HsInt input_off, HsInt input_length);
HsInt hs_base64_decode(uint8_t* output, const uint8_t* input, HsInt input_off, HsInt input_length);
void hs_intersperse(unsigned char *q, unsigned char *p, HsInt p_offset, HsInt n, unsigned char c);
