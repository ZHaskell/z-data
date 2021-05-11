#include <HsFFI.h>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <string>

extern "C" {

HsInt hs_std_string_size(std::string *str) {
  if (str != NULL)
    return (HsInt)str->size();
  else
    return 0;
}

void hs_copy_std_string(std::string *str, HsInt siz, char *buf) {
  if (str != NULL)
    memcpy(buf, str->c_str(), siz);
}

void hs_delete_std_string(std::string *str) { delete str; }

std::string *hs_cal_std_string_off(std::string *str, HsInt idx) {
  return str + idx;
}

// End extern "C"
}
