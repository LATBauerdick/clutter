{  system ? builtins.currentSystem } :
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  hlib = pkgs.haskell.lib.compose;
  fs = pkgs.lib.fileset;
  # only what the Haskell build reads, so edits to docs, scripts or the
  # frontend do not change the source hash and force a full rebuild
  src = fs.toSource {
    root = ./.;
    fileset = fs.unions [
      ./src
      ./app
      ./test
      ./clutter.cabal
      ./package.yaml
      ./README.md
      ./LICENSE
    ];
  };
in pkgs.lib.pipe
  (pkgs.haskellPackages.callCabal2nix "clutter" src {  })
  [ hlib.dontHaddock ]
