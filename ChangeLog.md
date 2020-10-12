# Revision history for Z-Data

## 0.1.6.1  -- 2020-10-09

* Remove `Str` newtype.
* Make `CBytes` a newtype.
* Add JSON instances for `ExitCode`, Add Unaligned instances for `Ptr a`.
* Use type alias instead of newtypes for `Locale`, `Category` in `Z.Data.Text`. 

## 0.1.6.0  -- 2020-10-09

* Rename `ToText` to `ShowT`, `toText` to `showT`, add FFI types instances, remove `Str` newtype.
* Change `Text` 's `Show` and `ShowT` escaping rules to reuse JSON escaping, remove `Read` instance.
* Add `ShowT` instances to `CBytes` and FFI types.

## 0.1.5.0  -- 2020-10-02

* Rework `CBytes` type to use unpinned byte array, add `withCBytesUnsafe`, `allocCBytesUnsafe`.
* Export `head`, `tail`, `init`, `last` from `Z.IO.Vector`, `Z.IO.Text` (well, safety first).
* Change `unalignedSize` in `UnalignedAccess` class's type to take a instance type and return `Int`.
* Rename `UnalignedAccess` to `Unaligned`.

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

