{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc8107"
  }: nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./backend.nix { }

