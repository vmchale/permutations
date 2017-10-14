test-fetch:
    git clone https://github.com/HuwCampbell/idris-lens depends/idris-lens
    cd depends/idris-lens/ && idris --install lens.ipkg
    idris --testpkg test.ipkg

clean:
    rm -f depends
    sn c .
