{
  description = "fvckbot";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: rec {
      defaultPackage = packages.fvckbot;
      packages = {
        fvckbot = nixpkgs.legacyPackages."${system}".haskellPackages.callPackage ./default.nix {};
        fvckbot-docker = with nixpkgs.legacyPackages."${system}";
          dockerTools.buildImage {
            name = "fvckbot";
            tag = "latest";
            config.Entrypoint = "${packages.fvckbot}/bin/fvckbot";
          };
      };
    });
}
