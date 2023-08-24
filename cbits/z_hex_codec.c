////////////////////////////////////////////////////////////////////////////////
// Hex codec

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

void z_hex_encode_generic(uint8_t* output, size_t output_off,
        const uint8_t* input, size_t input_off, size_t input_length){
    uint16_t* output_ptr = (uint16_t*)(output+output_off);
    const uint8_t* input_ptr = input + input_off;
    uint16_t* table = (uint16_t*)BIN_TO_HEX;
    for(size_t i = 0; i != input_length;) {
        *(output_ptr++) = table[input_ptr[i++]];
    }
}

void z_hex_encode_upper_generic(uint8_t* output, size_t output_off,
        const uint8_t* input, size_t input_off, size_t input_length){
    uint16_t* output_ptr = (uint16_t*)(output+output_off);
    const uint8_t* input_ptr = input + input_off;
    uint16_t* table = (uint16_t*)BIN_TO_HEX_UPPER;
    for(size_t i = 0; i != input_length;) {
        *(output_ptr++) = table[input_ptr[i++]];
    }
}

#if defined(__AVX2__)
inline static __m256i hex(__m256i value) {
    __m256i HEX_LUTR = _mm256_setr_epi8(
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
    return _mm256_shuffle_epi8(HEX_LUTR, value);
}
inline static __m256i hex_upper(__m256i value) {
    __m256i HEX_LUTR_UPPER = _mm256_setr_epi8(
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
    return _mm256_shuffle_epi8(HEX_LUTR_UPPER, value);
}

// a -> [a >> 4, a & 0b1111]
inline static __m256i byte2nib(__m128i val) {
    __m256i ROT2 = _mm256_setr_epi8(
          -1, 0, -1, 2, -1, 4, -1, 6, -1, 8, -1, 10, -1, 12, -1, 14,
          -1, 0, -1, 2, -1, 4, -1, 6, -1, 8, -1, 10, -1, 12, -1, 14
        );
    __m256i doubled = _mm256_cvtepu8_epi16(val);
    __m256i hi = _mm256_srli_epi16(doubled, 4);
    __m256i lo = _mm256_shuffle_epi8(doubled, ROT2);
    __m256i bytes = _mm256_or_si256(hi, lo);
    bytes = _mm256_and_si256(bytes, _mm256_set1_epi8(0b1111));
    return bytes;
}

// len is number of src bytes
void z_hex_encode_avx2(uint8_t* __restrict__ dest, const uint8_t* __restrict__ src, size_t len) {
    const __m128i* input128 = (const __m128i*)(src);
    __m256i* output256 = (__m256i*)(dest);

    size_t tailLen = len % 16;
    size_t vectLen = (len - tailLen) >> 4;
    for (size_t i = 0; i < vectLen; i++) {
      __m128i av = _mm_lddqu_si128(&input128[i]);
      __m256i nibs = byte2nib(av);
      __m256i hexed = hex(nibs);
      _mm256_storeu_si256(&output256[i], hexed);
    }
    z_hex_encode_generic(dest, (vectLen << 5), src, (vectLen << 4), tailLen);
}
// len is number of src bytes
void z_hex_encode_upper_avx2(uint8_t* __restrict__ dest, const uint8_t* __restrict__ src, size_t len) {
    const __m128i* input128 = (const __m128i*)(src);
    __m256i* output256 = (__m256i*)(dest);

    size_t tailLen = len % 16;
    size_t vectLen = (len - tailLen) >> 4;
    for (size_t i = 0; i < vectLen; i++) {
      __m128i av = _mm_lddqu_si128(&input128[i]);
      __m256i nibs = byte2nib(av);
      __m256i hexed = hex_upper(nibs);
      _mm256_storeu_si256(&output256[i], hexed);
    }
    z_hex_encode_upper_generic(dest, (vectLen << 5), src, (vectLen << 4), tailLen);
}
#endif

void z_hex_encode(uint8_t* output, HsInt output_off, const uint8_t* input, HsInt input_off, HsInt input_length){
#if (__AVX2__)
    z_hex_encode_avx2(output+output_off, input+input_off, input_length);
#else
    z_hex_encode_generic(output, output_off, input, input_off, input_length);
#endif
}

void z_hex_encode_upper(uint8_t* output, HsInt output_off, const uint8_t* input, HsInt input_off, HsInt input_length){
#if (__AVX2__)
    z_hex_encode_upper_avx2(output+output_off, input+input_off, input_length);
#else
    z_hex_encode_upper_generic(output, output_off, input, input_off, input_length);
#endif
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

HsInt z_hex_decode(uint8_t* output, const uint8_t* input, HsInt input_off, HsInt input_length) {
    uint8_t* out = output;
    input = input+input_off;
    for(size_t i = 0; i != input_length;) {
        const uint8_t hi = HEX_TO_BIN[input[i++]];
        const uint8_t lo = HEX_TO_BIN[input[i++]];
        if((hi | lo) & 0xF0) { 
            return -1;
        }
        else {
            *(out++) = hi << 4 | lo;
        }
    }
    return out - output;
}

HsInt z_hex_decode_ws(uint8_t* output, const uint8_t* input, HsInt input_off, HsInt input_length) {
    uint8_t* out = output;
    input = input+input_off;
    for(size_t i = 0; i != input_length;) {
        const uint8_t w1 = input[i++];
        
        if (i == input_length) return -1;   // input too short
        if (w1 == ' ' || (w1 >= 0x09 && w1 <= 0x0d)) continue;

        const uint8_t hi = HEX_TO_BIN[w1];
        const uint8_t lo = HEX_TO_BIN[input[i++]];
        if((hi | lo) & 0xF0) { 
            return -1;
        }
        else {
            *(out++) = hi << 4 | lo;
        }
    }
    return out - output;
}

HsWord32 z_hex_decode_hex_word32(const uint8_t* input, HsInt input_off) {
    input = input+input_off;
    return  (HsWord32)HEX_TO_BIN[input[0]] << 28 |
            (HsWord32)HEX_TO_BIN[input[1]] << 24 |
            (HsWord32)HEX_TO_BIN[input[2]] << 20 |
            (HsWord32)HEX_TO_BIN[input[3]] << 16 |
            (HsWord32)HEX_TO_BIN[input[4]] << 12 |
            (HsWord32)HEX_TO_BIN[input[5]] << 8  |
            (HsWord32)HEX_TO_BIN[input[6]] << 4  |
            (HsWord32)HEX_TO_BIN[input[7]];
