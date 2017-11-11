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
    rev = "d69eb44fdd92fcab81a5498536f039c6bdea17f7";
    sha256 = "bb8f4d1684f8e25ada5d5e5a31121fe741279a170294fb882c994085ddba2fe4";
  };

  propagatedBuildInputs = [ prelude base ];

  buildPhase = ''
    ${idris}/bin/idris --build permutations.ipkg
  '';

  meta = {
    description = "Permutations";
    homepage = https://github.com/vmchale/permutation;
    license = lib.licenses.bsd3;
    inherit (idris.meta) platforms;
  };
}
