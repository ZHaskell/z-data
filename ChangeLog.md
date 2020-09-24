# Revision history for Z-Data

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

