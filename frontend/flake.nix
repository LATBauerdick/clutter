{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      name = "clutter-frontend";
      supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        config = { };
        overlays = builtins.attrValues self.overlays;
      });
    in {
      overlays = {
        purescript = inputs.purescript-overlay.overlays.default;
      };

      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in {
          default = pkgs.hello; # your package here
        });

      devShells = forAllSystems (system:
        # pkgs now has access to the standard PureScript toolchain
        let pkgs = nixpkgsFor.${system}; in {
          default = pkgs.mkShell {
            name = "my-purescript-project";
            inputsFrom = builtins.attrValues self.packages.${system};
            buildInputs = [
              pkgs.esbuild
              pkgs.nodejs_20
              pkgs.nixpkgs-fmt
              pkgs.purs
              pkgs.purs-tidy
            # pkgs.purs-tidy-bin.purs-tidy-0_10_0
              pkgs.purs-backend-es
              pkgs.purescript-language-server
              pkgs.spago-unstable
            ]
            ++ (pkgs.lib.optionals (system == "aarch64-darwin")
               (with pkgs.darwin.apple_sdk.frameworks; [ Cocoa CoreServices ]));
          };
        });
  };
}

