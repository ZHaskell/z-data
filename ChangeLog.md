# Revision history for Z-Data

## 0.7.1.0  -- 2021-03-03

* Add `CPtr` type, a more lightweight foreign pointer.
* Add `toLower/toUpper/toLowerLatin/toUpperLatin` to `Z.Data.ASCII`.

## 0.7.0.0  -- 2021-03-03

* Add more patterns to `Z.Data.ASCII`.
* Fix a bug in `Z.Data.Vector.Search` where needle is a singleton.
* Optimize `mergeDupAdjacent` family funtions in `Z.Data.Vector.Sort` to not copy vector in case of no duplication.
* Add `scientific'` to `Z.Data.Builder`, Add more `Print` instances.
* Bump deps to support GHC 9.0.

## 0.6.1.0  -- 2021-02-04

* Add `key` and `nth` lens to `Z.Data.JSON.Value` for manipulating nested value more easily.
* Port patch from bytestring #301 #310 #315, Improve `stime`, `sconcat`, `intersperse`.
* Add JSON pretty printer `prettyJSON/prettyValue` to `Z.Data.JSON`.
* Move many instances from `Z.Data.JSON.Base` to `Z.Data.JSON` to reduce the chance of heap overflow when compile.
* Add `modifyIndex/insertIndex/deleteIndex` to array and vector, rewrite `FlatMap/FlatSet/FlatInMap/FlatIntSet' to use them.
* Remove `linearSearch` from `Z.Data.Vector.FlatMap/FlatInMap`, use `find/findR` from `Z.Data.Vector.Search` instead.
* Add `displayWidth` to `Z.Data.Text`.
* Move `floatToScientific/doubleToScientific` to `Z.Data.JSON.Value`.

## 0.6.0.0  -- 2021-02-04

* Add `primArrayFromBE/primArrayFromBE` to `Z.Data.Array.Unaligned`.
* Change `[Char]` 's JSON instance to use JSON text, Add `Bytes` 's JSON instance to use base64 encoding.
* Add `escapeCBytes`, `toEscapedText` to `Z.Data.CBytes`, change `CBytes` 's JSON instance to use base64 encoding.

## 0.5.0.0  -- 2021-01-15

* Add `ParseChunks` type alias, remove `parseChunks'` from `Z.Data.JSON`.
* Change JSON instance of `Data.Version` to use JSON Array.
* Add `fromByteString` and `toByteString` to `Z.Foreign`.
* Change license's copyright to test --test-show-details=direct

## 0.4.0.0  -- 2021-01-11

* Merge `FromValue`, `ToValue`, `EncodeJSON` to `JSON` class, add instances for containers.
* A new `Z.Data.JSON.Converter` module to be reused in other protocol decoding.
* Add more tests for JSON module, fix buggy instances(`Ordering`, `DayOfWeek`).
* Fix a bug in `BE Double` `Unaligned` instance.
* Add `arrVec` to `Z.Data.Vector` to convert a whole array to vector.

## 0.3.0.0  -- 2020-12-29

* Hide `Text` constructor from `Z.Data.Text`.
* Add `fromStdString` to `Z.Data.CBytes`.
* Re-export `touch` from `Z.Foreign`, export `ConvertError` constructor from `Z.Data.JSON`.
* Add `anyChar8` to `Z.Data.Parser`.
* Add `Z.Data.ASCII`, move `c2w`, `w2c`, `isDigit`, `isSpace`, `isHexDigit` and ASCII constants to it.
* Rename `ShowT` class to `Print`, re-export from `Z.Data.Text` module.
* Add various JSON instances of types from `time` package, add `Z.Data.Builder.Time` and `Z.Data.Parser.Time`.
* Fix a bug affecting comparing `PrimVector`s.

## 0.2.0.0  -- 2020-12-15

* Add `Z.Data.Text.Regex` module, which provide regex functions using RE2.
* Rename `buildBytes`, `buildBytesList` in `Z.Data.Builder` to `build`, `buildChunks`.
* Rename `encodeBytes`, `encodeBytesList` in `Z.Data.Builder` to `encode`, `encodeChunks`.
* `buildChunks` now produces a lazy bytes list, (so that `encodeChunks`, etc.).
* Functions in `Z.Data.Text.Search` now return char index by default, add `findBytesIndices/findBytesIndex/R` to
  search for bytes index.
* Add `fromStdString` to `Z.Foreign` to marshall `std::string` from c++ code.

## 0.1.9.0  -- 2020-10-23

* Add `Z.Data.Vecotr.Hex`, `Z.Data.Vector.Base64` module.
* Improve `Z.Data.Builder` 's performance, remove `AllocateStrategy`, `buildAndRun`.
* Remove `TextBuilder` newtype from `Z.Data.Text.ShowT`, add `buildText`, `unsafeBuildText` to `Z.Data.Builder`.

## 0.1.8.0  -- 2020-10-23

* Rename `ascii` to `vecASCII` in `Z.Data.Vector.QQ` to match array QQ.
* Add `FoldCase` instance to `Text`.
* Add `hex'`, `hex_`, `uint_`, `int_`, `integer` to `Z.Data.Parser`, change `hex`, `uint`, `int` to fail in case of overflow.
* Add `takeN` to `Z.Data.Parser`.
* Rename `withCBytesListSafe` to `withCBytesList` to match rest of the module.

## 0.1.7.2  -- 2020-10-17

* Add `withPrimArrayListUnsafe`, `withPrimArrayListSafe`, `withCBytesUnsafe`, `withCBytesListSafe`.

## 0.1.7.1  -- 2020-10-17

* Add `singleton` to `Z.IO.CBytes`, fix `unpack` when there're trailing illegal UTF8 codepoints.

## 0.1.7.0  -- 2020-10-15

* Change `defaultChunkSize` from 32K to 16K to reduce memory overhead.
* Change `CBytes` to use null terminated byte array, add JSON instances.
* Add JSON instances to various arrays, Fix `Char`'s JSON instance to reject string length > 1.
* Add `decodeText` and `decodeText'` to `Z.Data.JSON`.

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

