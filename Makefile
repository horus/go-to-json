.PHONY: all

all:
	cabal build --with-hsc2hs=/home/ghc/_build/stage1/bin/javascript-unknown-ghcjs-hsc2hs
