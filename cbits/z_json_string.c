////////////////////////////////////////////////////////////////////////////////

static inline uint32_t decode_hex(uint32_t c) {
    if (c >= '0' && c <= '9')      return c - '0';
    else if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    else if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return 0xFFFFFFFF; // Should not happen
}

// Decode, return negative value on error
HsInt z_decode_json_string(char *dest, const char *src, HsInt srcoff, HsInt srclen) {
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
// if return >= 0, it's the next split offset, excluding the last double quote
//    return == -1, string is not ended yet
// the lowest two bytes of state record two things:
//    skip: 1 if we should skip next char, 0 otherwise
//    escaped(LSB): 1 if this string contain escaped char(s),
//                  3 if this string contain unescaped control char(s),
//                  0 otherwise
HsInt find_json_string_end(uint32_t* state, const unsigned char* ba, HsInt off, HsInt end){
    const unsigned char *s = ba + off;
    const unsigned char *e = ba + end;
    long skip = *state >> 8;
    long escaped = *state & 0xFF;
    for (; s < e; s++) {
        if (skip == 1){
            skip = 0;       // skip this char
        }
        else if (*s == '\\') {  // backslash
            escaped = 1;
            skip = 1;
        }
        else if (*s == '\"') {  // double quote
            *state = (skip << 8) | escaped; // save the state
            return (s - ba);
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

HsInt z_escape_json_string_length(const unsigned char *src, HsInt srcoff, HsInt srclen){
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

HsInt z_escape_json_string(const unsigned char *src, HsInt srcoff, HsInt srclen, unsigned char *dest, HsInt desoff){
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
