module Z.MonoList.IntList (
    IntList(..)
  , IsList(..)
  , (++)
  , map
  , replicate
  , all
  , init
  , null
  , splitAt
  ) where 


#define X     Int
#define XList IntList
#define XEnd  IntEnd

#include "XList.hs"