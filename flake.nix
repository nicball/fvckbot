{
  description = "fvckbot";
  outputs = { self, nixpkgs }: {
    defaultPackage =
      let
        mkPackage = arch: nixpkgs.legacyPackages."${arch}".haskellPackages.callPackage ./default.nix {};
      in
        {
          aarch64-linux = mkPackage "aarch64-linux";
          x86_64-linux = mkPackage "x86_64-linux";
        };
  };
}
