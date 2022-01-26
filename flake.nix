{
  description = "fvckbot";
  outputs = { self, nixpkgs }: {
    defaultPackage.aarch64-linux =
      let
        inherit (nixpkgs.legacyPackages.aarch64-linux) pkgs;
      in
        pkgs.haskellPackages.callPackage ./default.nix {};
  };
}
