test:
    @idris --testpkg test.ipkg

update-docs:
    sn c .
    rm -rf docs
    idris --build permutations.ipkg
    idris --mkdoc permutations.ipkg
    mv permutations_doc/ docs/
