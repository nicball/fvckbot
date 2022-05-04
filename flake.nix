{
  description = "fvckbot";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      defaultPackage = nixpkgs.legacyPackages."${system}".haskellPackages.callPackage ./default.nix {};
    });
}
