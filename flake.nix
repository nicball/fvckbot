{
  description = "fvckbot";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    let overlay = self: super: {
      fvckbot = self.haskellPackages.callPackage ./default.nix {};
      fvckbot-docker =
        self.dockerTools.buildImage {
          name = "fvckbot";
          tag = "latest";
          config.Entrypoint = "${self.fvckbot}/bin/fvckbot";
          copyToRoot = with self.dockerTools; [ caCertificates ];
        };
    }; in
    flake-utils.lib.eachDefaultSystem (system: rec {
      packages =
        let pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; }; in
        builtins.intersectAttrs (overlay 42 42) pkgs // {
          default = packages.fvckbot;
        };
    }) // {
      overlays.default = overlay;
    };
}
