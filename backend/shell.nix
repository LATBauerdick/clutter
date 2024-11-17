{ system ? builtins.currentSystem, devTools ? true }:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  myHaskellPackages = pkgs.haskellPackages.extend
    (final: prev: { clutter = import ./package.nix { inherit system; }; });
in myHaskellPackages.shellFor {
    packages = p: [ p.clutter ];
    nativeBuildInputs = with pkgs;
      [ ghc cabal-install ] ++ lib.optional devTools [
        niv
        hlint
        ormolu
        (ghc.withPackages (p: [ p.haskell-language-server ]))
        python310     # Replace with the Python version you need
        python310Packages.pip
        python310Packages.requests
        python310Packages.python-dateutil
        python310Packages.typing-extensions
        python310Packages.isodate
        python310Packages.mpegdash
      ];
  }
