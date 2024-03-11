{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          hpkgs = pkgs.haskellPackages;

          name = "telegram-spongifybot";

          pkg = hpkgs.callCabal2nix name self {};

        in
        {
          packages.${name} = pkg;

          packages.default = pkg;

          packages.image = pkgs.dockerTools.buildImage {
            inherit name;
            tag = "latest";
            copyToRoot = [
              pkg
              pkgs.cacert
            ];
            config = {
              Cmd = [ "${pkg}/bin/${name}" ];
            };
          };

          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              hpkgs.haskell-language-server
              ghcid
              cabal-install
            ];
            inputsFrom = [ pkg."env" ];
          };

          defaultPackage = self.packages.${system}.default;

          devShell = self.devShells.${system}.default;
        }
      );
}
