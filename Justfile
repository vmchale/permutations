test:
    @idris --testpkg test.ipkg

update-docs:
    sn c .
    rm -rf docs
    idris --mkdoc permutations.ipkg
    mv permutations_doc/ docs/

clean:
    rm -rf depends
    sn c .
