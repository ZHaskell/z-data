<p align=center>
  <img src="https://github.com/haskell-Z/Z/raw/master/projectZ.svg">
</p>

## z-data

This package provides basic data structures and functions/本库提供一些基本数据结构和操作:

* Array, vector(array slice)
* Text based UTF-8, basic unicode manipulating
* FFI utilties
* Parsing and building monad
* JSON encoding and decoding

## dev guide/开发指南

```bash
# get code
git clone --recursive git@github.com:haskell-Z/z-data.git 
cd z-data
# build
cabal build
# test
cabal test
# install 
cabal install
# generate document
cabal haddock
# generate chinese version document
cabal haddock -f haddock-lang-cn
```
