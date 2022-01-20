{
  description = "fvckbot";
  outputs = { self, nixpkgs }: {
    defaultPackage.aarch64-linux =
      let
        inherit nixpkgs.legacyPackages.aarch64-linux.pkgs;
        f = { mkDerivation, aeson, async, base, bytestring, direct-sqlite
            , hint, http-conduit, lens, lens-aeson, lib, random, sqlite-simple
            , text, time
            }:
            mkDerivation {
              pname = "fvckbot";
              version = "0.1.0.0";
              src = ./.;
              isLibrary = false;
              isExecutable = true;
              executableHaskellDepends = [
                aeson async base bytestring direct-sqlite hint http-conduit lens
                lens-aeson random sqlite-simple text time
              ];
              homepage = "https://github.com/nicball/fvckbot#readme";
              license = lib.licenses.bsd3;
            };
      in
        pkgs.haskellPackages.callPackage f {};
  };
}
