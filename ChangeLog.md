# Revision history for Z-Data

## 0.1.4.3  -- 2020-10-02

* Rework `CBytes` type to use unpinned byte array only, add `withCBytesUnsafe`, `allocCBytesUnsafe`.
* Export `head`, `tail`, `init`, `last` from `Z.IO.Vector`, `Z.IO.Text` (well, safety first).

## 0.1.4.2  -- 2020-10-02

* Remove `withMutablePrimArrayUnsafe/Safe` from `Z.Foreign`.
* Add `allocPrimArrayUnsafe/Safe`, `allocPrimVectorUnsafe/Safe`, `allocBytesUnsafe/Safe`, `fromPtr/fromPrimPtr`
  to `Z.Foreign`.

## 0.1.4.1  -- 2020-09-29

* Add `fromNullTerminated` to `Z.Foreign`(and `Z.Data.Vector.Base`).
* Change `Category` and `Locale`(`Z.Data.Text`) to use pattern synonyms, add `envLocale` to `Z.Data.Text`.
* Add `validateASCII` and `validateASCIIMaybe` to `Z.Data.Text`.

## 0.1.4.0  -- 2020-09-24

* Make library works with GHC 8.6 and 8.8 again.
* Add `pinPrimVector` and `pinPrimArray` to `Z.Foreign`.
* Export `fail'` from `Z.Data.Parser`

## 0.1.3.1  -- 2020-09-24

* Change `clearMBA` 's type to match `clearPtr`.
* Move `peekMBA`, `pokeMBA` to `UnalignedAccess` class.

## 0.1.3.0  -- 2020-09-20

* Add indexing funtion to `Z.Data.Vector` and `Z.Data.Text`.
* Add `peekMBA`, `pokeMBA` and `clearMBA` to `Z.Foreign`.

## 0.1.2.0  -- 2020-09-19

* Rename `read/write/indexWord8ArrayAs` to `read/write/indexWord8ArrayAs#`.
* Add `read/write/indexWord8ArrayAs`, `read/write/indexPrimWord8ArrayAs`.
* Fix JSON encoding code in generic instance(constructor with single payload case).

