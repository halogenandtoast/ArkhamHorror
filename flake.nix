{
  description = "Arkham Horror: The Card Game - Online";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
      in {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs;
              [
                nodejs_21
                haskell.compiler.ghc96
              ];
          };
        };
      });
}
