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

  src = fetchgit {
    url = "https://github.com/vmchale/permutation";
    rev = "0d2a84dffaeaa2fbaae280273c4b7347267cdfc7";
    sha256 = "e726fe72e673fadeb248e4b562e172973eb2046659f37b9e9672847a1c86fd60";
  };

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
