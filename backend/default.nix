{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc925",
  executableSystemDepends ? []
  }: nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./clutter.nix { inherit executableSystemDepends; }
