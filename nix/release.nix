let
  pkgs = import <nixpkgs> { };

in
  (pkgs.idrisPackages.callPackage ./permutations.nix { })
