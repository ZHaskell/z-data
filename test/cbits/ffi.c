#include "Rts.h"
#include "HsFFI.h"

#if __GLASGOW_HASKELL__ < 810
HsInt sum_first_unsafe (StgMutArrPtrs *arr, HsInt len) {
  StgArrBytes **bufs = (StgArrBytes**)arr->payload;
#else
HsInt sum_first_unsafe (StgArrBytes **bufs, HsInt len) {
#endif
  HsInt res = 0;
  for(HsInt ix = 0;ix < len;ix++) {
     res = res + ((HsInt*)(bufs[ix]->payload))[0];
  }
  return res;
}

HsInt sum_first_safe (HsInt** bufs, HsInt len) {
  HsInt res = 0;
  for(HsInt ix = 0;ix < len;ix++) {
     res = res + bufs[ix][0];
  }
  return res;
}
