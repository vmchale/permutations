.PHONY: build install

build:
	idris2 --build permutations.ipkg

install:
	idris2 --install permutations.ipkg

docs:
	rm -rf docs
	idris2 --mkdoc permutations.ipkg
	mv build/docs/ .
