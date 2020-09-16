<p align=center>
  <img src="https://github.com/haskell-Z/Z/raw/master/projectZ.svg">
</p>

## Z-Data

[![Linux Build Status](https://img.shields.io/travis/haskell-z/z-data/master.svg?label=Linux%20build)](https://travis-ci.org/haskell-z/z-data)

This package provides basic data structures and functions:

* Array, vector(array slice)
* Text based UTF-8, basic unicode manipulating
* FFI utilties
* Parsing and building monad
* JSON encoding and decoding

## dev guide

+ GHC(>=8.10.2) 
+ cabal-install(>=3.4)

```bash
# get code
git clone --recursive git@github.com:haskell-Z/z-data.git 
cd z-data
# build
cabal build
# test
cabal run Z-Data-Test
# install 
cabal install
# generate document
cabal haddock
```
