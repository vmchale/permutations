build:
    @idris --build permutations.ipkg

test:
    @idris --testpkg test.ipkg

update-docs:
    sn c .
    rm -rf docs
    idris --mkdoc permutations.ipkg
    mv permutations_doc/ docs/

test-fetch:
    git clone https://github.com/HuwCampbell/idris-lens depends/idris-lens
    git clone https://github.com/pheymann/specdris depends/specdris
    cd depends/idris-lens/ && idris --install lens.ipkg
    cd depends/specdris && idris --install specdris.ipkg
    idris --testpkg test.ipkg

clean:
    rm -rf depends
    sn c .
