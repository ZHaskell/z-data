#include "Rts.h"

HsInt sum_first_unsafe (StgArrBytes** bufs, HsInt len) {
  HsInt res = 0;
  for(HsInt ix = 0;ix < len;ix++) {
     // take first four bytes as int and sum
     res = res + ((HsInt*)(bufs[ix]->payload))[0];
  }
  return res;
}

HsInt sum_first_safe (HsInt** bufs, HsInt len) {
  HsInt res = 0;
  for(HsInt ix = 0;ix < len;ix++) {
     // take first four bytes as int and sum
     res = res + bufs[ix][0];
  }
  return res;
}
