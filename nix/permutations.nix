{ build-idris-package
, fetchgit
, prelude
, base
, effects
, lib
, idris
}:

build-idris-package {
  name = "permutations";

  src = ../../permutation;

  propagatedBuildInputs = [ prelude base ];

  buildPhase = ''
    ${idris}/bin/idris --build permutations.ipkg
  '';

  checkPhase = ''
    echo 'skipping check...'
    ''; #      ${idris}/bin/idris --testpkg test.ipkg
    #'';

  installPhase = ''
    ${idris}/bin/idris --install permutations.ipkg --ibcsubdir $IBCSUBDIR
  '';

  meta = {
    description = "Permutations";
    homepage = https://github.com/vmchale/permutation;
    license = lib.licenses.bsd3;
    inherit (idris.meta) platforms;
  };
}
