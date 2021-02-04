/*
Copyright (c) 2020-2021 Dong Han
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
#include <cstddef>
#include <cstdlib>
#include <re2/re2.h>
#include <re2/set.h>

extern "C" {

int64_t hs_re2_kDefaultMaxMem(){
    return re2::RE2::Options::kDefaultMaxMem;
}

re2::RE2 *hs_re2_compile_pattern(const char *input, HsInt off, HsInt input_len
     , bool    posix_syntax                  
     , bool    longest_match                 
     , int64_t max_mem                       
     , bool    literal                       
     , bool    never_nl                      
     , bool    dot_nl                        
     , bool    never_capture                 
     , bool    case_sensitive                
     , bool    perl_classes                  
     , bool    word_boundary                 
     , bool    one_line ){   
    re2::RE2::Options opts;
    opts.set_posix_syntax  ( posix_syntax  ); 
    opts.set_longest_match ( longest_match );
    opts.set_max_mem       ( max_mem       );
    opts.set_literal       ( literal       );
    opts.set_never_nl      ( never_nl      );
    opts.set_dot_nl        ( dot_nl        );
    opts.set_never_capture ( never_capture );
    opts.set_case_sensitive( case_sensitive);
    opts.set_perl_classes  ( perl_classes  );
    opts.set_word_boundary ( word_boundary );
    opts.set_one_line      ( one_line      );
	return new re2::RE2(re2::StringPiece(input+off, input_len), opts);
}

re2::RE2 *hs_re2_compile_pattern_default(const char *input, HsInt off, HsInt input_len) {
	return new re2::RE2(re2::StringPiece(input+off, input_len));
}

void hs_re2_delete_pattern(re2::RE2 *regex) {
	delete regex;
}

bool hs_re2_ok(re2::RE2 *regex) {
	return regex->ok();
}

HsInt hs_num_capture_groups(re2::RE2 *regex) {
    return regex->NumberOfCapturingGroups()+1;
}

HsInt hs_std_string_size(std::string* str) {
    if (str != NULL) return (HsInt)str->size();
    else return 0;
}

void hs_copy_std_string(std::string* str, HsInt siz, char* buf) {
    if (str != NULL) memcpy(buf, str->c_str(), siz);
}

void hs_delete_std_string(std::string* str) {
    delete str;
}

std::string* hs_re2_quote_meta(const char *in, HsInt off, HsInt len) {
	std::string* quoted = new std::string(re2::RE2::QuoteMeta(re2::StringPiece(in+off, len)));
    return quoted;
}

std::string* hs_re2_replace(re2::RE2 *regex, const char *in, HsInt in_off, HsInt in_len
                           , const char *rewrite, HsInt rewrite_off, HsInt rewrite_len) {
	std::string* str = new std::string(in+in_off, in_len);
	re2::RE2::Replace(str, *regex, re2::StringPiece(rewrite+rewrite_off, rewrite_len));
    return str;
}

std::string* hs_re2_replace_g(re2::RE2 *regex, const char *in, HsInt in_off, HsInt in_len
                           , const char *rewrite, HsInt rewrite_off, HsInt rewrite_len) {
	std::string* str = new std::string(in+in_off, in_len);
	re2::RE2::GlobalReplace(str, *regex, re2::StringPiece(rewrite+rewrite_off, rewrite_len));
    return str;
}

std::string* hs_re2_extract(re2::RE2 *regex, const char *in, HsInt in_off, HsInt in_len
                           , const char *rewrite, HsInt rewrite_off, HsInt rewrite_len) {
	std::string* str = new std::string;
	re2::RE2::Extract(re2::StringPiece(in+in_off, in_len)
                     , *regex, re2::StringPiece(rewrite+rewrite_off, rewrite_len), str);
    return str;
}

int hs_re2_match(re2::RE2 *regex, const char *in, HsInt off, HsInt len,
    HsInt num_captures, HsInt *capture_starts, HsInt *capture_lens) {
	re2::StringPiece *vec = new re2::StringPiece[num_captures];
	if (!regex->Match(re2::StringPiece(in+off, len), 0, len, re2::RE2::UNANCHORED, vec, num_captures)) {
		delete[] vec;
		return 0;
	}
    for (HsInt i = 0; i < num_captures; i++) {
        if (vec[i].data() == NULL) {
            capture_starts[i] = 0;
            capture_lens[i] = -1;
        } else {
            capture_starts[i] = vec[i].data()-in;
            capture_lens[i] = vec[i].size();
        }
    }
	delete[] vec;
	return 1;
}

int hs_re2_test(re2::RE2 *regex, const char *in, HsInt off, HsInt len) {
	if (!regex->Match(re2::StringPiece(in+off, len), 0, len, re2::RE2::UNANCHORED, NULL, 0)) {
		return 0;
	}
	return 1;
}

}
