{
  description = "clutter is a way to display and search records";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem ( system :
        let
          clutter = import ./package.nix { inherit system; };
        in rec {
          devShells.default = import ./shell.nix { inherit system; };
          packages.default = clutter;
          apps.default = {
            type = "app";
            program = "${clutter}/bin/clutter";
          };
        }
    );
}
