test: music-theory.cabal lib/* test/*
	stack test

test-watch:
	fd | entr -s "make test"

build: lib/*
	stack build
