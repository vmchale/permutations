.PHONY: build install

build:
	idris2 --build permutations.ipkg

install:
	idris2 --install permutations.ipkg
