{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          hpkgs = pkgs.haskellPackages;

          name = "telegram-spongifybot";
        in
        {
          packages.${name} = hpkgs.callCabal2nix name self {};

          packages.default = self.packages.${system}.${name};

          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              hpkgs.haskell-language-server
              ghcid
              cabal-install
            ];
            inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
          };

          defaultPackage = self.packages.${system}.default;

          devShell = self.devShells.${system}.default;
        }
      );
}
