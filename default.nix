{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc8107",
  executableSystemDepends ? []
  }: nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./clutter.nix { inherit executableSystemDepends; }

