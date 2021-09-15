{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc8106" }:
  (import ./default.nix { inherit nixpkgs compiler; }).env
