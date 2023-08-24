lib: 
	cabal build

clean:
	cabal clean

doctest:
	cabal repl --with-ghc=doctest

doctest_verbose:
	cabal repl --with-ghc=doctest --repl-options=--verbose

doc:
	cabal haddock \
		--haddock-options="--mathjax=https://z.haskell.world/haddock.inject.utterances.via.mathjax.js?repo=ZHaskell/z-data"\
		--haddock-hyperlink-sources --haddock-css=doc-assets/z.haddock.css \
		--haddock-html-location='https://hackage.haskell.org/package/$$pkg-$$version/docs'

cndoc:
	cabal -fbuild-chinese-doc haddock \
		--haddock-options="--mathjax=https://z.haskell.world/haddock.inject.utterances.via.mathjax.js?repo=ZHaskell/z-data"\
		--haddock-hyperlink-sources --haddock-css=doc-assets/z.haddock.css \
		--haddock-html-location='https://hackage.haskell.org/package/$$pkg-$$version/docs'

