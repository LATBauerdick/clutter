{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc8107",
  executableSystemDepends ? []
  }: nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./backend.nix { inherit executableSystemDepends; }

